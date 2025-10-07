// SPDX-License-Identifier: MIT
pragma solidity ^0.8.26;

import {ERC20} from "@openzeppelin/contracts/token/ERC20/ERC20.sol";
import {Math} from "@openzeppelin/contracts/utils/math/Math.sol";

// VRF v2.5 consumer + client
import {VRFConsumerBaseV2Plus} from "@chainlink/contracts/src/v0.8/vrf/dev/VRFConsumerBaseV2Plus.sol";
import {VRFV2PlusClient}      from "@chainlink/contracts/src/v0.8/vrf/dev/libraries/VRFV2PlusClient.sol";


/**
 * @title Purged (PURGE) — game token + VRF/RNG coordinator side
 * @notice ERC20 with game mechanics (coinflip, jackpots, staking, affiliates) and Chainlink VRF v2.5.
 * @dev The coin contract is the coordinator side the game calls into. This file contains declarations,
 *      configuration, storage, events, and constructor wiring. Functional sections follow below in the file.
 */
 interface IVRFCoordinatorV2PlusLike {
    function getSubscription(uint256 subId)
        external
        view
        returns (
            uint96 balance,
            uint96 nativeBalance,
            uint64 reqCount,
            address owner,
            address[] memory consumers
        );
}

interface IPurgeGame {
    function gameState() external view returns (uint8);
    function level() external view returns (uint24);
    function getJackpotWinners(
        uint256 randomWord,
        uint8 trait,
        uint8 numWinners,
        uint8 salt
    ) external view returns (address[] memory);
}

interface ILinkToken {
    function transferAndCall(address to, uint256 value, bytes calldata data) external returns (bool);
}

contract Purgecoin is ERC20, VRFConsumerBaseV2Plus {
    // ---------------------------------------------------------------------
    // Errors
    // ---------------------------------------------------------------------
    error OnlyDeployer();              // caller must be contract deployer
    error OnlyGame();                  // caller must be the PurgeGame contract
    error BettingPaused();             // coinflip entry disabled
    error Zero();                      // generic zero-value disallow
    error Insufficient();              // insufficient balance / bad payment / generic
    error AmountLTMin();               // provided amount is below minimum
    error FlipLTMin();                 // coinflip deposit below minimum
    error BurnLT2pct();                // burn requirement not met vs deposit
    error E();   
    error InvalidLeaderboard();        // bad leaderboard selector
    error PresaleExceedsRemaining();   // over cap on presale
    error InvalidKind();               // parameter enumerations out of range
    error StakeCollision();            // staking schedule conflict

    // ---------------------------------------------------------------------
    // Types
    // ---------------------------------------------------------------------
    struct PlayerScore { address player; uint256 score; }

    /// @dev RNG request state (one active at a time)
    struct RngReq { uint256 word; bool fulfilled; }

    /// @dev BAF jackpot accounting
    struct BAFState {
        uint128 totalPrizePoolWei;
        uint120 returnAmountWei;
        bool    inProgress;
    }

    /// @dev BAF scatter scan cursor
    struct BAFScan  {
        uint120 per;
        uint32  limit;
        uint8   offset;
    }

    /// @dev Decimator per-player burn snapshot for a given level
    struct DecEntry {
        uint232 burn;
        uint24  level;
    }

    // ---------------------------------------------------------------------
    // Constants (units & limits)
    // ---------------------------------------------------------------------
    uint256 private constant MILLION           = 1e6;       // token has 6 decimals
    uint16  private constant RANK_NOT_FOUND    = 11;
    uint256 private constant PRESALEAMOUNT     = 4_000_000 * MILLION;
    uint256 private constant MIN               = 100 * MILLION;   // min burn / min flip (100 PURGED)
    uint8   private constant MAX_RISK          = 11;        // staking risk 1..11
    uint256 private constant RF_BASE           = 100;       // last 2 digits encode risk
    uint256 private constant ONEK              = 1_000 * MILLION; // 1,000 PURGED (6d)
    uint32  private constant BAF_BATCH         = 5000;
    uint256 private constant BUCKET_SIZE       = 750;
    uint256 private constant TOKEN_PRICE_WEI   = 25_000_000_000_000; // 0.000025 ETH per PURGED (info)
    uint256 private constant UNITS_PER_ETH     = 40_000 * 1_000_000; // 40k PURGED (6d) per 1 ETH (info)
    uint32  private constant CLEANUP_BATCH     = 100;       // buckets per cleanup call
    uint256 private constant LUCK_PER_LINK     = 220 * MILLION; // 220 PURGE per 1 LINK


    // ---------------------------------------------------------------------
    // VRF configuration
    // ---------------------------------------------------------------------
    uint32  private constant vrfCallbackGasLimit   = 200_000;
    uint16  private constant vrfRequestConfirmations = 5;
    bool    private constant vrfPayInNative        = false;

    // ---------------------------------------------------------------------
    // Scan sentinels
    // ---------------------------------------------------------------------
    uint32  private constant SS_IDLE = type(uint32).max;     // not started
    uint32  private constant SS_DONE = type(uint32).max - 1; // finished

    // ---------------------------------------------------------------------
    // Immutables / external wiring
    // ---------------------------------------------------------------------
    address private immutable creator;              // deployer / ETH sink
    bytes32 private immutable vrfKeyHash;           // VRF key hash
    uint256 private immutable vrfSubscriptionId;    // VRF sub id
 
    // LINK token (Chainlink ERC677) — network-specific address
    ILinkToken public constant LINK = ILinkToken(0x779877A7B0D9E8603169DdbD7836e478b4624789);

    // ---------------------------------------------------------------------
    // Game wiring & state
    // ---------------------------------------------------------------------
    address private purgeGameContract;  // PurgeGame contract address (set once)

    // Session flags
    bool   public  isBettingPaused;     // set while VRF is pending unless explicitly allowed
    bool   private tbActive;            // “tenth player” bonus active
    uint8  private extMode;             // external jackpot mode (state machine)

    // Leaderboard lengths
    uint8 private luckboxLen;
    uint8 private affiliateLen;
    uint8 private topLen;

    // “tenth player” bonus fields
    uint8   private tbMod;      // wheel mod (0..9)
    uint32  private tbRemain;   // remaining awards
    uint256 private tbPrize;    // prize per tenth player

    // Scan cursors / progress
    uint24 private stakeLevelComplete;
    uint32 private scanCursor = SS_IDLE;
    uint32 private cleanupCursor = SS_IDLE;
    uint32 private coinflipPlayersCount;
    uint32 private payoutIndex;

    // Daily jackpot accounting
    uint256 private dailyCoinBurn;
    uint256 private currentTenthPlayerBonusPool;

    // Buckets for large player sets (flattened into fixed-size chunks)
    address[][] private coinflipBuckets;
    mapping(address => uint256) public coinflipAmount;

    // O(1) “largest bettor” tracking
    PlayerScore[4] public topBettors;

    // Affiliates / luckbox
    mapping(bytes32 => address) private affiliateCode;
    mapping(uint24 => mapping(address => uint256)) public affiliateCoinEarned; // level => player => earned
    mapping(address => address) public referredBy;
    mapping(address => uint256) public playerLuckbox;
    PlayerScore[10] public luckboxLeaderboard;
    PlayerScore[8]  public affiliateLeaderboard;

    // Staking
    mapping(uint24 => address[]) private stakeAddr;                  // level => stakers
    mapping(uint24 => mapping(address => uint256)) private stakeAmt; // level => encoded principal+risk

    // Leaderboard index maps (1-based positions)
    mapping(address => uint8) private luckboxPos;
    mapping(address => uint8) private affiliatePos;
    mapping(address => uint8) private topPos;

    // RNG
    RngReq  private rng;
    uint256 private rngRequestId;

    // Bounty / BAF heads
    uint256 public currentBounty     = ONEK;
    uint256 public biggestFlipEver   = ONEK;
    address private bountyOwedTo;

    // BAF / Decimator execution state
    BAFState private bafState;
    BAFScan  private bs;
    uint256  private extVar; // decimator accumulator/denominator

    // Decimator tracking
    mapping(address => DecEntry) private decBurn;
    mapping(uint24 => mapping(uint256 => address[])) private decBuckets; // level => bucketIdx => players
    mapping(uint24 => uint256) private decPlayersCount;

    // Presale tiers
    uint256 public totalPresaleSold;
    uint256 private constant TIER1_BOUNDARY = 800_000 * MILLION;
    uint256 private constant TIER2_BOUNDARY = 1_600_000 * MILLION;
    uint256 private constant TIER3_BOUNDARY = 2_400_000 * MILLION;
    uint256 private constant TIER1_PRICE    = 0.0000125 ether;
    uint256 private constant TIER2_PRICE    = 0.000015  ether;
    uint256 private constant TIER3_PRICE    = 0.0000175 ether;
    uint256 private constant TIER4_PRICE    = 0.00002   ether;

    // ---------------------------------------------------------------------
    // Events
    // ---------------------------------------------------------------------
    event Affiliate(uint256 amount, bytes32 indexed code, address sender);
    event CoinflipFinished(bool result);
    event CoinJackpotPaid(uint16 trait, address winner, uint256 amount);
    event BountyOwed(address indexed to, uint256 bountyAmount, uint256 newRecordFlip);
    event BountyPaid(address indexed to, uint256 amount);

    // ---------------------------------------------------------------------
    // Modifiers
    // ---------------------------------------------------------------------
    modifier onlyPurgeGameContract() {
        if (msg.sender != purgeGameContract) revert OnlyGame();
        _;
    }

    // ---------------------------------------------------------------------
    // Constructor
    // ---------------------------------------------------------------------
    /**
     * @param _vrfCoordinator    Chainlink VRF v2.5 coordinator
     * @param _keyHash           Key hash for the coordinator
     * @param _subId             Subscription id (funded with LINK / native per config)
     */
    constructor(
        address _vrfCoordinator,
        bytes32 _keyHash,
        uint256 _subId
    ) ERC20("Purgecoin", "PURGE") VRFConsumerBaseV2Plus(_vrfCoordinator) {
        creator            = msg.sender;
        vrfKeyHash         = _keyHash;
        vrfSubscriptionId  = _subId;

        // Initial supply: reserve for presale and deployer allocation.
        _mint(address(this), PRESALEAMOUNT);
        _mint(creator,       PRESALEAMOUNT);
    }

    // Lucky burn + optional coinflip deposit
    /// @notice Burn PURGED to grow luckbox and (optionally) place a coinflip deposit in the same tx.
    /// @dev
    /// - Reverts if betting is paused.
    /// - `amount` must be ≥ MIN; if `coinflipDeposit` > 0 it must be ≥ MIN and burn must be at least 2% of it.
    /// - Burns the sum (`amount + coinflipDeposit`), then (if provided) schedules the flip via `addFlip`.
    /// - Credits luckbox with `amount + coinflipDeposit/50` and updates the luckbox leaderboard.
    /// - If the Decimator window is active, accumulates the caller’s burn for the current level.
    function luckyCoinBurn(uint256 amount, uint256 coinflipDeposit) external {
        if (isBettingPaused) revert BettingPaused();
        if (amount < MIN) revert AmountLTMin();

        address caller = msg.sender;
        uint256 burnTotal = amount + coinflipDeposit;

        if (balanceOf(caller) < burnTotal) revert Insufficient();

        if (coinflipDeposit != 0) {
            if (coinflipDeposit < MIN) revert FlipLTMin();
            // Require burn to be at least 2% of the coinflip deposit.
            if (amount * 50 < coinflipDeposit) revert BurnLT2pct();
        }

        _burn(caller, burnTotal);

        if (coinflipDeposit != 0) {
            // Internal flip accounting; assumed non-reentrant / no external calls.
            addFlip(caller, coinflipDeposit, true);
        }

        unchecked {
            // Track aggregate burn and grow caller luckbox (adds +2% of deposit).
            dailyCoinBurn += amount;
            uint256 newLuck = playerLuckbox[caller] + amount + coinflipDeposit / 50;
            playerLuckbox[caller] = newLuck;

            // Update luckbox leaderboard (board 0 = luckbox).
            _updatePlayerScore(0, caller, newLuck);
        }

        // If Decimator window is active, accumulate burn this level.
        (bool decOn, uint24 lvl) = _decWindow();
        if (decOn) {
            DecEntry storage e = decBurn[caller];
            if (e.level != lvl) {
                e.level = lvl;
                e.burn  = uint232(amount);
                _decPush(lvl, caller);
            } else {
                e.burn += uint232(amount);
            }
        }
    }

    // Affiliate code management
    /// @notice Create a new affiliate code mapping to the caller.
    /// @dev Reverts if `code_` is zero or already taken.
    function createAffiliateCode(bytes32 code_) external {
        if (code_ == bytes32(0)) revert Zero();
        if (affiliateCode[code_] != address(0)) revert Insufficient();
        affiliateCode[code_] = msg.sender;
        emit Affiliate(1, code_, msg.sender); // 1 = code created
    }

    /// @notice Set the caller’s referrer once using a valid affiliate code.
    /// @dev Reverts if code is unknown, self-referral, or caller already has a referrer.
    function referPlayer(bytes32 code_) external {
        address referrer = affiliateCode[code_];
        if (
            referrer == address(0) ||
            referrer == msg.sender ||
            referredBy[msg.sender] != address(0)
        ) {
            revert Insufficient();
        }
        referredBy[msg.sender] = referrer;
        emit Affiliate(0, code_, msg.sender); // 0 = player referred
    }
    // Stake with encoded risk window
    /// @notice Burn PURGED to open a future “stake window” targeting `targetLevel` with a risk radius.
    /// @dev
    /// - `burnAmt` must be ≥ 250e6 (6d).
    /// - `targetLevel` must be at least 10 levels ahead of the current game level.
    /// - `risk` ∈ [1..MAX_RISK] and cannot exceed the distance to `targetLevel`.
    /// - Encodes stake as: principal rounded to RF_BASE (100) + 2‑digit risk code.
    /// - Enforces no overlap/collision with caller’s existing stakes.
    function stake(uint256 burnAmt, uint24 targetLevel, uint8 risk) external {
        if (burnAmt < 250 * MILLION) revert AmountLTMin();
        if(isBettingPaused) revert BettingPaused();

        uint24 currLevel = IPurgeGame(purgeGameContract).level();
        if (targetLevel < currLevel + 10) revert Insufficient();

        uint24 distance = targetLevel - currLevel;
        if (risk == 0 || risk > MAX_RISK || risk > distance) revert Insufficient();

        // Starting level where this stake is placed (inclusive)
        uint24 placeLevel = uint24(targetLevel - (risk - 1));

        // 1) Guard against direct collisions in the risk window [placeLevel .. placeLevel+risk-1]
        uint256 existingEncoded;
        for (uint24 offset = 0; offset < risk; ) {
            uint24 checkLevel = uint24(placeLevel + offset);
            existingEncoded = stakeAmt[checkLevel][msg.sender];
            if (existingEncoded != 0) {
                uint16 haveRisk = uint16(existingEncoded % RF_BASE);
                uint16 wantRisk = uint16(risk - offset);
                if (haveRisk != wantRisk) revert StakeCollision();
            }
            unchecked { ++offset; }
        }

        // 2) Guard against overlap from earlier stakes that extend into placeLevel
        uint24 scanStart = currLevel;
        uint24 scanFloor = placeLevel > (MAX_RISK - 1) ? uint24(placeLevel - (MAX_RISK - 1)) : uint24(1);
        if (scanStart < scanFloor) scanStart = scanFloor;

        for (uint24 scanLevel = scanStart; scanLevel < placeLevel; ) {
            uint256 existingAtScan = stakeAmt[scanLevel][msg.sender];
            if (existingAtScan != 0) {
                uint16 existingRisk = uint16(existingAtScan % RF_BASE);
                uint24 reachLevel   = scanLevel + uint24(existingRisk) - 1;
                if (reachLevel >= placeLevel) {
                    // Risk code implied at placeLevel must match `risk`
                    uint24 impliedRiskAtPlace = uint24(existingRisk) - (placeLevel - scanLevel);
                    if (uint8(impliedRiskAtPlace) != risk) revert StakeCollision();
                }
            }
            unchecked { ++scanLevel; }
        }

        // Burn principal
        _burn(msg.sender, burnAmt);

        // Base credit and compounded boost factors
        uint256 levelBps = 5 * uint256(distance);        if (levelBps > 300) levelBps = 300;
        uint256 riskBps  = 30 * uint256(risk - 1);       if (riskBps  > 300) riskBps  = 300;
        uint256 stepBps  = levelBps + riskBps;           // per-level growth in bps

        // Luckbox: +5% of burn
        uint256 fivePercent = burnAmt / 20;
        playerLuckbox[msg.sender] += fivePercent;
        uint256 newLuck = playerLuckbox[msg.sender];
        _updatePlayerScore(0, msg.sender, newLuck);

        // Compounded boost starting from 95% of burn (fivePercent * 19)
        uint256 boostedPrincipal = fivePercent * 19;
        for (uint24 i = distance; i != 0; ) {
            boostedPrincipal = Math.mulDiv(boostedPrincipal, 10_000 + stepBps, 10_000);
            unchecked { --i; }
        }

        // Encode: round principal to RF_BASE and append risk code in the last two digits
        uint256 principalRounded = boostedPrincipal - (boostedPrincipal % RF_BASE);
        uint256 encoded          = principalRounded + risk;

        // Track first-time stakers at this start level
        if (stakeAmt[placeLevel][msg.sender] == 0) {
            stakeAddr[placeLevel].push(msg.sender);
        }

        // Merge with existing stake at `placeLevel` (risk equality already validated)
        existingEncoded = stakeAmt[placeLevel][msg.sender];
        if (existingEncoded == 0) {
            stakeAmt[placeLevel][msg.sender] = encoded;
        } else {
            uint16 existingRiskAtPlace = uint16(existingEncoded % RF_BASE);
            uint256 existingPrincipal  = existingEncoded - existingRiskAtPlace;
            uint16 mergedRisk = (risk > existingRiskAtPlace) ? risk : existingRiskAtPlace; // equal per precheck
            unchecked {
                stakeAmt[placeLevel][msg.sender] = existingPrincipal + principalRounded + mergedRisk;
            }
        }
    }

    /// @notice Return the recorded referrer for `player` (zero address if none).
    function getReferrer(address player) external view returns (address) {
        return referredBy[player];
    }
    /// @notice Credit affiliate rewards for a purchase (invoked by the game contract).
    /// @dev
    /// Referral rules:
    /// - If `referredBy[sender] == address(1)`: sender is “locked” and we no-op.
    /// - Else if a referrer already exists: pay that address (`affiliateAddr = referrer`).
    /// - Else if `code` resolves to a valid address different from `sender`: bind it and use it.
    /// - Else: lock the sender to `address(1)` (no future attempts) and return.
    /// Payout rules:
    /// - `amount` is optionally doubled on levels `level % 25 == 1`.
    /// - Direct ref gets full `amount`; their upline (if any and already active this level)
    ///   receives a 20% bonus of the same (post‑doubling) amount.
    function payAffiliate(uint256 amount, bytes32 code, address sender, uint24 lvl)
        external
        onlyPurgeGameContract
    {
        address affiliateAddr = affiliateCode[code];
        address referrer      = referredBy[sender];

        // Locked sender: ignore
        if (referrer == address(1)) return;

        // Resolve the effective affiliate
        if (referrer != address(0)) {
            affiliateAddr = referrer;
        } else if (affiliateAddr != address(0) && affiliateAddr != sender) {
            referredBy[sender] = affiliateAddr;
        } else {
            // Permanently lock sender from further referral attempts
            referredBy[sender] = address(1);
            return;
        }

        if (amount != 0) {
            if (lvl % 25 == 1) amount <<= 1;

            // Pay direct affiliate (skip sentinels)
            if (affiliateAddr != address(0) && affiliateAddr != address(1)) {
                uint256 newTotal = affiliateCoinEarned[lvl][affiliateAddr] + amount;
                affiliateCoinEarned[lvl][affiliateAddr] = newTotal;
                _mint(affiliateAddr, amount);
                _updatePlayerScore(1, affiliateAddr, newTotal);
            }

            // Upline bonus (20%) only if upline is active this level
            address upline = referredBy[affiliateAddr];
            if (
                upline != address(0) &&
                upline != address(1) &&
                upline != sender &&
                affiliateCoinEarned[lvl][upline] > 0
            ) {
                uint256 bonus = amount / 5;
                if (bonus != 0) {
                    uint256 newTotalU = affiliateCoinEarned[lvl][upline] + bonus;
                    affiliateCoinEarned[lvl][upline] = newTotalU;
                    _mint(upline, bonus);
                    _updatePlayerScore(1, upline, newTotalU);
                }
            }
        }

        emit Affiliate(amount, code, sender);
    }

    /// @notice Clear the affiliate leaderboard and index for the next cycle (invoked by the game).
    function resetAffiliateLeaderboard() external onlyPurgeGameContract {
        uint8 len = affiliateLen;
        for (uint8 i; i < len; ) {
            address addr = affiliateLeaderboard[i].player;
            delete affiliatePos[addr];
            unchecked { ++i; }
        }
        delete affiliateLeaderboard; // zero out fixed-size array entries
        affiliateLen = 0;
    }
    /// @notice Buy PURGE during the tiered presale by sending exact ETH.
    /// @dev
    /// Requirements:
    /// - `amount` uses token base units (decimals = 6 assumed by constants).
    /// - `amount >= 5e6` and is a multiple of `1e6`.
    /// - Not more than the remaining presale allocation.
    /// Effects:
    /// - Increments `totalPresaleSold`.
    /// - Forwards ETH to `creator`.
    /// - Transfers `amount` tokens from this contract to the buyer.
    function presale(uint256 amount) external payable {
        if (amount < 5 * MILLION) revert AmountLTMin();
        if (amount % MILLION != 0) revert InvalidKind();

        uint256 sold = totalPresaleSold;
        if (amount > PRESALEAMOUNT - sold) revert PresaleExceedsRemaining();

        uint256 costWei = _computePresaleCostAtSold(amount, sold);
        if (msg.value != costWei) revert Insufficient();

        // Effects
        totalPresaleSold = sold + amount;

        // Interactions (ETH to creator)
        (bool ok, ) = payable(creator).call{value: costWei}("");
        if (!ok) revert Insufficient();

        // Token transfer to buyer
        _transfer(address(this), msg.sender, amount);
    }

    /// @notice Quote the ETH required to purchase `amount` at the current presale tier state.
    /// @dev Reverts if `amount` is zero, not a multiple of `1e6`, or exceeds remaining allocation.
    function quotePresaleCost(uint256 amount) external view returns (uint256 costWei) {
        if (amount == 0) revert Insufficient();
        if (amount % MILLION != 0) revert InvalidKind();

        uint256 sold = totalPresaleSold;
        if (amount > PRESALEAMOUNT - sold) revert PresaleExceedsRemaining();

        return _computePresaleCostAtSold(amount, sold);
    }

    /// @notice Compute the tiered presale cost for `amount`, given `sold` units already sold.
    /// @dev Splits `amount` across tier buckets [T1..T4] using remaining capacity in order,
    ///      then multiplies by per‑tier prices. All arithmetic uses base‑unit amounts (1e6).
    function _computePresaleCostAtSold(uint256 amount, uint256 sold)
        internal
        pure
        returns (uint256 costWei)
    {
        unchecked {
            uint256 tier1Qty;
            uint256 tier2Qty;
            uint256 tier3Qty;
            uint256 remainingQty = amount;

            if (sold < TIER1_BOUNDARY) {
                uint256 space1 = TIER1_BOUNDARY - sold;
                tier1Qty   = remainingQty < space1 ? remainingQty : space1;
                sold      += tier1Qty;
                remainingQty -= tier1Qty;
            }
            if (remainingQty != 0 && sold < TIER2_BOUNDARY) {
                uint256 space2 = TIER2_BOUNDARY - sold;
                tier2Qty   = remainingQty < space2 ? remainingQty : space2;
                sold      += tier2Qty;
                remainingQty -= tier2Qty;
            }
            if (remainingQty != 0 && sold < TIER3_BOUNDARY) {
                uint256 space3 = TIER3_BOUNDARY - sold;
                tier3Qty   = remainingQty < space3 ? remainingQty : space3;
                sold      += tier3Qty;
                remainingQty -= tier3Qty;
            }

            // Divide by 1e6 at the end since amounts are in base units (decimals = 6).
            costWei =
                (tier1Qty   * TIER1_PRICE +
                tier2Qty   * TIER2_PRICE +
                tier3Qty   * TIER3_PRICE +
                remainingQty * TIER4_PRICE) / MILLION;
        }
    }

    function requestRngPurgeGame(bool dontStopFlips)
        external
        onlyPurgeGameContract
        returns (uint256 id)
    {
        id = s_vrfCoordinator.requestRandomWords(
            VRFV2PlusClient.RandomWordsRequest({
                keyHash: vrfKeyHash,
                subId: vrfSubscriptionId,
                requestConfirmations: vrfRequestConfirmations,
                callbackGasLimit: vrfCallbackGasLimit,
                numWords: 1,
                extraArgs: VRFV2PlusClient._argsToBytes(
                    VRFV2PlusClient.ExtraArgsV1({ nativePayment: vrfPayInNative })
                )
            })
        );

        rngRequestId = id;
        rng = RngReq({ fulfilled: false, word: 0 });
        isBettingPaused = !dontStopFlips;
    }

    /// @notice VRF callback: store the random word once for the expected request.
    /// @dev Reverts if `requestId` does not match the most recent request or if already fulfilled.
    function fulfillRandomWords(uint256 requestId, uint256[] calldata randomWords) internal override {
        if (requestId != rngRequestId) revert E();
        if (rng.fulfilled) revert E();
        rng.fulfilled = true;
        rng.word = randomWords[0];
    }

    /// @notice Read the current VRF word (0 if not yet fulfilled).
    function pullRng() external view returns (uint256) {
        return rng.fulfilled ? rng.word : 0;
    }
    /// @notice One‑time wiring of the PurgeGame contract address.
    /// @dev Access: deployer/creator only; irreversible (no admin update).
    function addContractAddress(address a) external {
        if (purgeGameContract != address(0) || msg.sender != creator) revert OnlyDeployer();
        purgeGameContract = a;
    }

    /// @notice Mint PURGE during gameplay flows (affiliates, jackpots, rebates).
    /// @dev Access: PurgeGame only. If `recipient` is zero, credits this contract instead of reverting.
    function mintInGame(address recipient, uint256 _amount) external onlyPurgeGameContract {
        if (recipient == address(0)) return;
        else _mint(recipient, _amount);
    }

    /// @notice Burn PURGE from `target` during gameplay flows (purchases, fees),
    ///         and credit 2% of the burned amount to their luckbox.
    /// @dev Access: PurgeGame only. OZ ERC20 `_burn` reverts on zero address or insufficient balance.
    ///      Leaderboard is refreshed only when a non‑zero credit is applied.
    function burnInGame(address target, uint256 amount) external onlyPurgeGameContract {
        _burn(target, amount);
        // 2% luckbox credit; skip if too small to matter after integer division.
        uint256 credit = amount / 50; // 2%
        uint256 newLuck = playerLuckbox[target] + credit;
        playerLuckbox[target] = newLuck;
        _updatePlayerScore(0, target, newLuck); // luckbox leaderboard
    }

    /// @inheritdoc ERC20
    /// @dev Fixed 6‑decimals token.
    function decimals() public pure override returns (uint8) { return 6; }


    /// @notice Progress coinflip payouts for the current level in bounded slices.
    /// @dev Called by PurgeGame. Operates in four phases per “day”:
    ///      (1) Optional stake propagation (once per level/day; only if coinflip win).
    ///      (2) Arm bounty & “tenth‑player” bonus on the first payout window.
    ///      (3) Pay player flips (and tenth‑player bonuses) in batches.
    ///      (4) Cleanup in batches; on completion, reset per‑round state and unpause betting.
    /// @param level Current PurgeGame level (used to gate 1/run and propagate stakes).
    /// @param cap   Work cap hint. cap==0 uses defaults; otherwise applies directly.
    /// @return finished True once the entire cycle—including cleanup—has completed.
    function processCoinflipPayouts(uint24 level, uint32 cap, bool bonusFlip)
        external
        onlyPurgeGameContract
        returns (bool finished)
    {
        if (!isBettingPaused) return true;
        // --- Step sizing (bounded work) ----------------------------------------------------
        uint32 stepStake   = (cap == 0) ? 150 : cap;      // # stakers to process this call
        uint32 stepPayout  = (cap == 0) ? 150 : cap;      // # players to pay this call
        uint32 cleanupStep = (cap == 0)
            ? CLEANUP_BATCH
            : uint32((uint256(cap) + BUCKET_SIZE - 1) / BUCKET_SIZE + 2); // ~cap/BUCKET_SIZE + slack

        uint256 word = rng.word;
        bool  win = (word & 1) == 1;
        // --- (1) Stake propagation (once per level; only if coinflip result is win) --------
        if (payoutIndex == 0 && stakeLevelComplete < level) {
            
            uint32 st = scanCursor;
            if (st == SS_IDLE) { st = 0; scanCursor = 0; }

            if (st != SS_DONE) {
                // If loss: mark complete; no propagation.
                if (!win) {
                    scanCursor = SS_DONE;
                    stakeLevelComplete = level;
                } else {
                    // Win: process stakers at this level in slices.
                    address[] storage a = stakeAddr[level];
                    uint32 len = uint32(a.length);
                    if (st < len) {
                        uint32 en = st + stepStake; if (en > len) en = len;

                        for (uint32 i = st; i < en; ) {
                            address s   = a[i];
                            uint256 enc = stakeAmt[level][s];
                            uint16  rf  = uint16(enc % RF_BASE); // encoded risk (1..MAX_RISK)
                            uint256 pr  = enc - rf;               // principal (multiple of RF_BASE)

                            if (rf <= 1) {
                                // Matured: realize 5% of principal into luckbox; credit 95% as a flip.
                                uint256 fivePct = pr / 20;
                                uint256 newLuck = playerLuckbox[s] + fivePct;
                                playerLuckbox[s] = newLuck;
                                _updatePlayerScore(0, s, newLuck);
                                addFlip(s, fivePct * 19, false);
                            } else {
                                // Roll forward: double principal and decrement risk into next level.
                                uint24  nextL   = level + 1;
                                uint256 doubled = pr * 2;
                                uint256 ex      = stakeAmt[nextL][s];
                                uint16  newRf   = rf - 1;

                                if (ex == 0) {
                                    stakeAmt[nextL][s] = doubled + newRf;
                                    stakeAddr[nextL].push(s);
                                } else {
                                    uint16  exRf = uint16(ex % RF_BASE);
                                    uint256 exPr = ex - exRf;
                                    stakeAmt[nextL][s] = exPr + doubled + exRf; // merge
                                }
                            }
                            unchecked { ++i; }
                        }

                        scanCursor = (en == len) ? SS_DONE : en;
                        return false; // more stake processing remains
                    }

                    // Finished this phase.
                    scanCursor = SS_DONE;
                    stakeLevelComplete = level;
                    return false; // allow caller to continue in a subsequent call
                }
            }
        }

        // --- (2) Bounty payout & tenth‑player bonus arming (first payout window only) -------
        uint256 totalPlayers = coinflipPlayersCount;

        // Bounty: convert any owed bounty into a flip credit on the first window.
        if (totalPlayers != 0 && payoutIndex == 0 && bountyOwedTo != address(0) && currentBounty > 0) {
            address to = bountyOwedTo;
            uint256 amt = currentBounty;
            bountyOwedTo = address(0);
            currentBounty = 0;
            addFlip(to, amt, false);
            emit BountyPaid(to, amt);
        }
        
        // “Every 10th player” bonus pool: arm once per round when win and enough players.
        if (win && payoutIndex == 0 && currentTenthPlayerBonusPool > 0 && totalPlayers >= 10) {
            uint256 bonusPool = currentTenthPlayerBonusPool; currentTenthPlayerBonusPool = 0;
            uint32 rem = uint32(totalPlayers / 10); // how many 10th slots exist
            if (rem != 0) {
                uint256 prize = bonusPool / rem;
                uint256 dust  = bonusPool - prize * rem;
                if (dust > 0) _addToBounty(dust);
                tbPrize = prize; tbRemain = rem; tbActive = (prize != 0);
            } else {
                _addToBounty(bonusPool); tbActive = false;
            }
            tbMod = uint8(uint256(keccak256(abi.encodePacked(word, "tenthMod"))) % 10); // wheel offset 0..9
        }

        // --- (3) Player payouts (windowed by stepPayout) -----------------------------------
        uint256 start = payoutIndex;
        uint256 end   = start + stepPayout; if (end > totalPlayers) end = totalPlayers;

        
        uint8 wheel = uint8(start % 10); // rolling 0..9 index for tenth‑player bonus

        for (uint256 i = start; i < end; ) {
            address p = _playerAt(i);

            uint256 credit; // accumulate bonus + flip payout

            // Tenth‑player bonus
            if (tbActive && tbRemain != 0 && wheel == tbMod) {
                credit = tbPrize;
                unchecked { --tbRemain; }
                if (tbRemain == 0) tbActive = false;
            }

            // Flip payout: double on win, zero out stake in all cases.
            uint256 amt = coinflipAmount[p];
            if (amt != 0) {
                coinflipAmount[p] = 0;
                if (win) {
                    if (bonusFlip) amt = (amt * 11) / 10; // keep current rounding semantics
                    unchecked { credit += amt * 2; }
                }
            }

            if (credit != 0) _mint(p, credit);

            unchecked { ++i; wheel = (wheel == 9) ? 0 : (wheel + 1); }
        }
        payoutIndex = uint32(end);

        // --- (4) Cleanup (windowed by cleanupStep); reset state when fully complete ----------
        if (end >= totalPlayers) {
            uint256 len = coinflipBuckets.length;
            uint32 cur  = (cleanupCursor == SS_IDLE) ? 0 : cleanupCursor;
            uint256 cend = uint256(cur) + cleanupStep; if (cend > len) cend = len;

            for (uint256 b = cur; b < cend; ) { delete coinflipBuckets[b]; unchecked { ++b; } }
            cleanupCursor = uint32(cend);

            if (cend != len) return false; // continue cleanup next call

            // Hard reset for next round
            delete coinflipBuckets;
            coinflipPlayersCount = 0;

            scanCursor    = SS_IDLE;
            cleanupCursor = SS_IDLE;

            for (uint8 k; k < topLen; ) {
                address q = topBettors[k].player;
                if (q != address(0)) topPos[q] = 0;
                unchecked { ++k; }
            }
            delete topBettors;
            topLen = 0;

            payoutIndex = 0;

            tbActive = false; tbRemain = 0; tbPrize = 0; tbMod = 0;

            isBettingPaused = false;
            emit CoinflipFinished(win);

            rngRequestId = 0;
            return true;
        }

        return false;
    }
    /// @notice Distribute “coin jackpots” after the daily coinflip result is known.
    /// @dev
    /// Flow (only callable by PurgeGame):
    ///  - If loss: reset `dailyCoinBurn` and exit.
    ///  - If win:
    ///     * Add 15% of (max(dailyCoinBurn, 8k PURGED)) to the bounty.
    ///     * Pay 4 trait jackpots (15% total → 3.75% each) to the top‑luckbox among 5 candidates per trait,
    ///       else roll that slice into the bounty if no candidate qualifies.
    ///     * From 30% pool: credit the largest bettor, a random pick among #3/#4, arm the “every 10th player”
    ///       bonus (paid during payouts), and split the remainder to luckbox leaderboard players with ≥ 1000 PURGED
    ///       currently staked in flips. Any remainder dust is rolled into the bounty.
    ///  - Finally, zero `dailyCoinBurn` for the next cycle.
    function triggerCoinJackpot() external onlyPurgeGameContract {
        bool flipWin = (rng.word & 1) == 1;
        if (!flipWin) { dailyCoinBurn = 0; return; }

        uint256 randWord  = rng.word;
        uint256 burnBase  = dailyCoinBurn;
        if (burnBase < 8000 * MILLION) burnBase = 8000 * MILLION;

        // ----- (A) Always add 15% to the bounty -----
        _addToBounty((burnBase * 15) / 100);

        // ----- (B) 4× trait jackpots from another 15% (3.75% each) -----
        {
            uint256 traitPool = (burnBase * 15) / 100;
            if (traitPool != 0) {
                uint256 traitRnd = uint256(keccak256(abi.encodePacked(randWord, "CoinJackpot")));
                uint256 perTraitPrize = traitPool / 4; // remainder intentionally not used

                for (uint8 i; i < 4; ) {
                    if (i > 0) traitRnd = uint256(keccak256(abi.encodePacked(traitRnd, i)));
                    uint8 winningTrait = uint8(traitRnd & 0x3F) + (i << 6);

                    // Up to 5 candidates sampled by the game; pick highest luckbox.
                    address[] memory candidates = IPurgeGame(purgeGameContract).getJackpotWinners(
                        randWord, winningTrait, 5, uint8(42 + i)
                    );
                    address winnerAddr = getTopLuckbox(candidates);

                    emit CoinJackpotPaid(winningTrait, winnerAddr, perTraitPrize);
                    if (winnerAddr != address(0)) {
                        _mint(winnerAddr, perTraitPrize);
                    } else {
                        _addToBounty(perTraitPrize);
                    }
                    unchecked { ++i; }
                }
            }
        }

        // ----- (C) Player / leaderboard jackpots from 30% -----
        {
            uint256 totalPlayers = coinflipPlayersCount;
            uint256 playerPool   = (burnBase * 30) / 100;

            if (totalPlayers != 0 && playerPool != 0) {
                uint256 lbPrize   = (playerPool * 35) / 100; // largest bettor
                uint256 midPrize  = (playerPool * 15) / 100; // random among #3 / #4 bettors
                currentTenthPlayerBonusPool = (playerPool * 20) / 100; // paid during payouts
                uint256 lbPool    = playerPool - lbPrize - midPrize - currentTenthPlayerBonusPool;

                // Largest bettor gets credited now (paid when payouts run).
                address largestBettor = topBettors[0].player;
                if (largestBettor != address(0)) {
                    coinflipAmount[largestBettor] += lbPrize;
                }

                // Randomly pick #3 or #4.
                address midWinner = topBettors[2 + (uint256(keccak256(abi.encodePacked(randWord, "p34"))) & 1)].player;
                if (midWinner != address(0)) {
                    coinflipAmount[midWinner] += midPrize;
                }

                // Luckbox leaderboard split (only those with ≥ 1000 PURGED in active flips).
                if (lbPool != 0) {
                    address[10] memory eligible;
                    uint256 eligibleCount;
                    address topLuck = luckboxLeaderboard[0].player;
                    bool topIncluded;

                    for (uint8 i; i < 10; ) {
                        address p = luckboxLeaderboard[i].player;
                        if (p != address(0) && coinflipAmount[p] >= ONEK) {
                            eligible[eligibleCount] = p;
                            if (p == topLuck) topIncluded = true;
                            unchecked { ++eligibleCount; }
                        }
                        unchecked { ++i; }
                    }

                    if (eligibleCount == 0) {
                        _addToBounty(lbPool);
                    } else if (topIncluded) {
                        if (eligibleCount == 1) {
                            _mint(topLuck, lbPool);
                            emit CoinJackpotPaid(420, topLuck, lbPool);
                        } else {
                            uint256 topCut = (lbPool * 25) / 100;
                            _mint(topLuck, topCut);
                            emit CoinJackpotPaid(420, topLuck, topCut);

                            uint256 rem  = lbPool - topCut;
                            uint256 each = rem / (eligibleCount - 1);
                            uint256 paid;

                            for (uint256 i; i < eligibleCount; ) {
                                address w = eligible[i];
                                if (w != topLuck) {
                                    _mint(w, each);
                                    emit CoinJackpotPaid(420, w, each);
                                    paid += each;
                                }
                                unchecked { ++i; }
                            }
                            uint256 dust = lbPool - (paid + topCut);
                            if (dust != 0) _addToBounty(dust);
                        }
                    } else {
                        uint256 each = lbPool / eligibleCount;
                        uint256 paid;
                        for (uint256 i; i < eligibleCount; ) {
                            address w = eligible[i];
                            _mint(w, each);
                            emit CoinJackpotPaid(420, w, each);
                            paid += each;
                            unchecked { ++i; }
                        }
                        uint256 dust = lbPool - paid;
                        if (dust != 0) _addToBounty(dust);
                    }
                }
            }
        }

        // Reset for next day.
        dailyCoinBurn = 0;
    }
    /// @notice Progress an external jackpot: BAF (kind=0) or Decimator (kind=1).
    /// @dev
    /// Lifecycle:
    /// - First call (not in progress): arms the run based on `kind`, snapshots limits, computes offsets,
    ///   and optionally performs BAF “headline” allocations (largest bettor, etc.). When more work is
    ///   required, returns (finished=false, partial winners/amounts, 0).
    /// - Subsequent calls stream through the remaining work in windowed batches until finished=true.
    /// Storage/Modes:
    /// - `bafState.inProgress` gates the run. `extMode` encodes the sub‑phase:
    ///     0 = idle, 1 = BAF scatter pass, 2 = Decimator denom accumulation, 3 = Decimator payouts.
    /// - `scanCursor` walks the population starting at `bs.offset` then advancing in steps of 10.
    /// Returns:
    /// - `finished` signals completion of the whole external run.
    /// - `winners/amounts` are the credits to be applied by the caller on this step only.
    /// - `returnAmountWei` is any ETH to send back to the game (unused mid‑run).
    function runExternalJackpot(
        uint8 kind,
        uint256 poolWei,
        uint32 cap,
        uint24 lvl
    )
        external
        onlyPurgeGameContract
        returns (
            bool finished,
            address[] memory winners,
            uint256[] memory amounts,
            uint256 returnAmountWei
        )
    {
        uint32 batch = (cap == 0) ? BAF_BATCH : cap;

        uint256 executeWord = rng.word;

        // ----------------------------------------------------------------------
        // Arm a new external run
        // ----------------------------------------------------------------------
        if (!bafState.inProgress) {
            if (kind > 1) revert InvalidKind();

            bafState.inProgress = true;

            uint32 limit = (kind == 0)
                ? uint32(coinflipPlayersCount)
                : uint32(decPlayersCount[lvl]);

            // Randomize the stride modulo for the 10‑way sharded buckets
            bs.offset = uint8(executeWord % 10);
            bs.limit  = limit;
            scanCursor = bs.offset;

            // Pool/accounting snapshots
            bafState.totalPrizePoolWei = uint128(poolWei);
            bafState.returnAmountWei   = 0;

            extVar  = 0;
            extMode = (kind == 0) ? uint8(1) : uint8(2);

            // ---------------------------
            // kind == 0 : BAF headline + setup scatter
            // ---------------------------
            if (kind == 0) {
                uint256 P = poolWei;
                uint256 lbMin = (ONEK / 4) * uint256(lvl); // minimum “active” threshold
                address[6] memory tmpW; uint256[6] memory tmpA; uint256 n;
                uint256 credited; uint256 toReturn;

                // (1) Largest bettor — 20%
                {
                    uint256 prize = (P * 20) / 100;
                    address w = topBettors[0].player;
                    if (_eligible(w, lbMin)) { tmpW[n] = w; tmpA[n] = prize; unchecked { ++n; } credited += prize; }
                    else { toReturn += prize; }
                }
                // (2) Random among #3/#4 — 10%
                {
                    uint256 prize = (P * 10) / 100;
                    address w = topBettors[2 + (uint256(keccak256(abi.encodePacked(executeWord, "p34"))) & 1)].player;
                    if (_eligible(w, lbMin)) { tmpW[n] = w; tmpA[n] = prize; unchecked { ++n; } credited += prize; }
                    else { toReturn += prize; }
                }
                // (3) Random eligible — 10%
                {
                    uint256 prize = (P * 10) / 100;
                    address w = _randomEligible(uint256(keccak256(abi.encodePacked(executeWord, "re"))), lbMin);
                    if (w != address(0)) { tmpW[n] = w; tmpA[n] = prize; unchecked { ++n; } credited += prize; }
                    else { toReturn += prize; }
                }
                // (4) Luckbox LB #1/#2/#3 — 7%/5%/3%
                {
                    uint256 p1 = (P * 7) / 100;
                    uint256 p2 = (P * 5) / 100;
                    uint256 p3 = (P * 3) / 100;
                    address w1 = luckboxLeaderboard[0].player;
                    address w2 = luckboxLeaderboard[1].player;
                    address w3 = luckboxLeaderboard[2].player;
                    if (_eligible(w1, lbMin)) { tmpW[n] = w1; tmpA[n] = p1; unchecked { ++n; } credited += p1; } else { toReturn += p1; }
                    if (_eligible(w2, lbMin)) { tmpW[n] = w2; tmpA[n] = p2; unchecked { ++n; } credited += p2; } else { toReturn += p2; }
                    if (_eligible(w3, lbMin)) { tmpW[n] = w3; tmpA[n] = p3; unchecked { ++n; } credited += p3; } else { toReturn += p3; }
                }

                // Scatter the remainder equally across shard‑stride participants
                uint256 scatter = P - credited - toReturn;
                if (limit >= 10 && bs.offset < limit) {
                    uint256 occurrences = 1 + (uint256(limit) - 1 - bs.offset) / 10; // count of indices visited
                    uint256 perWei = scatter / occurrences;
                    bs.per = uint120(perWei);

                    // Accumulate “toReturn” plus any scatter dust
                    uint256 rem = toReturn + (scatter - perWei * occurrences);
                    bafState.returnAmountWei = uint120(rem);
                } else {
                    bs.per = 0;
                    bafState.returnAmountWei = uint120(toReturn + scatter);
                }

                // Emit headline winners for this step
                winners = new address[](n);
                amounts = new uint256[](n);
                for (uint256 i; i < n; ) { winners[i] = tmpW[i]; amounts[i] = tmpA[i]; unchecked { ++i; } }

                // If nothing to scatter (or empty population), finish immediately
                if (bs.per == 0 || limit < 10 || bs.offset >= limit) {
                    uint256 ret = uint256(bafState.returnAmountWei);
                    delete bafState; delete bs; extMode = 0; extVar = 0; scanCursor = SS_IDLE;
                    return (true, winners, amounts, ret);
                }
                return (false, winners, amounts, 0);
            }

            // ---------------------------
            // kind == 1 : Decimator armed; denom pass first
            // ---------------------------
            return (false, new address[](0), new uint256[](0), 0);
        }

        // ----------------------------------------------------------------------
        // BAF scatter pass (extMode == 1)
        // ----------------------------------------------------------------------
        if (extMode == 1) {
            uint32 end = scanCursor + batch; if (end > bs.limit) end = bs.limit;

            uint256 tmpCap = uint256(batch) / 10 + 2; // rough upper bound on step winners
            address[] memory tmpWinners = new address[](tmpCap);
            uint256[] memory tmpAmounts = new uint256[](tmpCap);
            uint256 n2;
            uint256 per = uint256(bs.per);
            uint256 lbMin = (ONEK / 4) * uint256(lvl);
            uint256 retWei = uint256(bafState.returnAmountWei);

            for (uint32 i = scanCursor; i < end; ) {
                address p = _playerAt(i);
                if (_eligible(p, lbMin)) { tmpWinners[n2] = p; tmpAmounts[n2] = per; unchecked { ++n2; } }
                else { retWei += per; }
                unchecked { i += 10; }
            }
            scanCursor = end;
            bafState.returnAmountWei = uint120(retWei);

            winners = new address[](n2);
            amounts = new uint256[](n2);
            for (uint256 k; k < n2; ) { winners[k] = tmpWinners[k]; amounts[k] = tmpAmounts[k]; unchecked { ++k; } }

            if (end == bs.limit) {
                uint256 ret = uint256(bafState.returnAmountWei);
                delete bafState; delete bs; extMode = 0; extVar = 0; scanCursor = SS_IDLE;
                return (true, winners, amounts, ret);
            }
            return (false, winners, amounts, 0);
        }

        // ----------------------------------------------------------------------
        // Decimator denom accumulation (extMode == 2)
        // ----------------------------------------------------------------------
        if (extMode == 2) {
            uint32 end = scanCursor + batch; if (end > bs.limit) end = bs.limit;
            uint256 lbMin = (ONEK / 4) * uint256(lvl);

            for (uint32 i = scanCursor; i < end; ) {
                address p = _srcPlayer(1, lvl, i);
                DecEntry storage e = decBurn[p];
                if (e.level == lvl && _eligibleLuckbox(p, lbMin)) { extVar += e.burn; }
                unchecked { i += 10; }
            }
            scanCursor = end;

            if (end < bs.limit) return (false, new address[](0), new uint256[](0), 0);

            // Nothing eligible → refund entire pool
            if (extVar == 0) {
                uint256 refund = uint256(bafState.totalPrizePoolWei);
                delete bafState; delete bs; extMode = 0; extVar = 0; scanCursor = SS_IDLE;
                return (true, new address[](0), new uint256[](0), refund);
            }

            // Proceed to payouts
            extMode = 3;
            scanCursor = bs.offset;
            return (false, new address[](0), new uint256[](0), 0);
        }

        // ----------------------------------------------------------------------
        // Decimator payouts (extMode == 3)
        // ----------------------------------------------------------------------
        {
            uint32 end = scanCursor + batch; if (end > bs.limit) end = bs.limit;

            uint256 tmpCap = uint256(batch) / 10 + 2;
            address[] memory tmpWinners = new address[](tmpCap);
            uint256[] memory tmpAmounts = new uint256[](tmpCap);
            uint256 n2;

            uint256 lbMin = (ONEK / 4) * uint256(lvl);
            uint256 pool = uint256(bafState.totalPrizePoolWei);
            uint256 denom = extVar;
            uint256 paid  = uint256(bafState.returnAmountWei);

            for (uint32 i = scanCursor; i < end; ) {
                address p = _srcPlayer(1, lvl, i);
                DecEntry storage e = decBurn[p];
                if (e.level == lvl && _eligibleLuckbox(p, lbMin)) {
                    uint256 amt = Math.mulDiv(pool, e.burn, denom);
                    if (amt != 0) { tmpWinners[n2] = p; tmpAmounts[n2] = amt; unchecked { ++n2; paid += amt; } }
                }
                unchecked { i += 10; }
            }
            scanCursor = end;
            bafState.returnAmountWei = uint120(paid);

            winners = new address[](n2);
            amounts = new uint256[](n2);
            for (uint256 k; k < n2; ) { winners[k] = tmpWinners[k]; amounts[k] = tmpAmounts[k]; unchecked { ++k; } }

            if (end == bs.limit) {
                uint256 ret = pool > paid ? (pool - paid) : 0;
                delete bafState; delete bs; extMode = 0; extVar = 0; scanCursor = SS_IDLE;
                return (true, winners, amounts, ret);
            }
            return (false, winners, amounts, 0);
        }
    }

    /// @notice Return 1-based rank of `player` on the luckbox leaderboard, or `RANK_NOT_FOUND` (11) if absent.
    /// @dev Defensive check ensures mapping hint matches current leaderboard slot.
    function getPlayerRank(address player) external view returns (uint16) {
        uint8 pos = luckboxPos[player]; // 1..luckboxLen or 0 (not present)
        return (pos != 0 && pos <= luckboxLen && luckboxLeaderboard[pos - 1].player == player)
            ? pos
            : RANK_NOT_FOUND;
    }

    /// @notice Return addresses from a leaderboard.
    /// @param which 0 = luckbox (≤10), 1 = affiliate (≤8), 2 = top bettors (≤4).
    function getLeaderboardAddresses(uint8 which) external view returns (address[] memory out) {
        if (which == 0) {
            uint8 len = luckboxLen;
            out = new address[](len);
            for (uint8 i; i < len; ) { out[i] = luckboxLeaderboard[i].player; unchecked { ++i; } }
        } else if (which == 1) {
            uint8 len = affiliateLen;
            out = new address[](len);
            for (uint8 i; i < len; ) { out[i] = affiliateLeaderboard[i].player; unchecked { ++i; } }
        } else if (which == 2) {
            uint8 len = topLen;
            out = new address[](len);
            for (uint8 i; i < len; ) { out[i] = topBettors[i].player; unchecked { ++i; } }
        } else {
            revert InvalidLeaderboard();
        }
    }

    /// @notice Eligibility gate requiring both luckbox balance and active coinflip stake ≥ `min`.
    function _eligible(address player, uint256 min) internal view returns (bool) {
        return playerLuckbox[player] >= min && coinflipAmount[player] >= min;
    }

    /// @notice Eligibility gate requiring only luckbox balance ≥ `min` (no coinflip amount check).
    function _eligibleLuckbox(address player, uint256 min) internal view returns (bool) {
        return playerLuckbox[player] >= min;
    }

    /// @notice Pick the first eligible player when scanning up to 300 candidates from a pseudo-random start.
    /// @dev Uses stride 2 for odd N to cover the ring without repeats; stride 1 for even N (contiguous window).
    function _randomEligible(uint256 seed, uint256 min) internal view returns (address) {
        uint256 total = coinflipPlayersCount;
        if (total == 0) return address(0);

        uint256 idx   = seed % total;
        uint256 stride = (total & 1) == 1 ? 2 : 1;           // ensures full coverage when total is odd
        uint256 maxChecks = total < 300 ? total : 300;       // cap work

        for (uint256 tries; tries < maxChecks; ) {
            address p = _playerAt(idx);
            if (_eligible(p, min)) return p;
            unchecked {
                idx += stride;
                if (idx >= total) idx -= total;
                ++tries;
            }
        }
        return address(0);
    }
    /// @notice Return player address at global coinflip index `idx`.
    /// @dev Indexing is flattened into fixed-size buckets to avoid resizing a single array.
    ///      Callers must ensure 0 <= idx < coinflipPlayersCount for the current session.
    function _playerAt(uint256 idx) internal view returns (address) {
        uint256 bucketIdx       = idx / BUCKET_SIZE;
        uint256 offsetInBucket  = idx - bucketIdx * BUCKET_SIZE; // saves an extra mod op
        return coinflipBuckets[bucketIdx][offsetInBucket];
    }

    /// @notice Source player address for a given index in either coinflip or decimator lists.
    /// @param kind 0 = coinflip roster, 1 = decimator roster for `lvl`.
    /// @param lvl  Level to read when `kind == 1`.
    /// @param idx  Global flattened index (0..N-1).
    function _srcPlayer(uint8 kind, uint24 lvl, uint256 idx) internal view returns (address) {
        if (kind == 0) return _playerAt(idx);
        uint256 bucketIdx      = idx / BUCKET_SIZE;
        uint256 offsetInBucket = idx - bucketIdx * BUCKET_SIZE;
        return decBuckets[lvl][bucketIdx][offsetInBucket];
    }

    /// @notice Append `p` to the end of the coinflip roster, creating a new bucket as needed.
    function _pushPlayer(address p) internal {
        uint32 i = coinflipPlayersCount;                  // next global slot
        uint256 bucketIdx = i / BUCKET_SIZE;
        if (bucketIdx == coinflipBuckets.length) coinflipBuckets.push();
        address[] storage bucket = coinflipBuckets[bucketIdx];
        bucket.push(p);
        unchecked { coinflipPlayersCount = i + 1; }
    }

    /// @notice Increase a player’s pending coinflip stake and possibly arm a bounty.
    /// @param player           Target player.
    /// @param coinflipDeposit  Amount to add to their current pending flip stake.
    /// @param canArmBounty     If true, a sufficiently large deposit may arm a bounty.
    function addFlip(address player, uint256 coinflipDeposit, bool canArmBounty) internal {
        uint256 prevStake = coinflipAmount[player];
        if (prevStake == 0) _pushPlayer(player);

        uint256 newStake = prevStake + coinflipDeposit;
        coinflipAmount[player] = newStake;
        _updatePlayerScore(2, player, newStake);

        uint256 record = biggestFlipEver; 
        if (newStake > record && topBettors[0].player == player) {
            biggestFlipEver = newStake;

            if (canArmBounty) {
                uint256 threshold = (bountyOwedTo != address(0)) ? (record + record / 100) : record;
                if (newStake >= threshold) {
                    bountyOwedTo = player;
                    emit BountyOwed(player, currentBounty, newStake); 
                }
            }
        }
    }

    /// @notice Increase the global bounty pool.
    /// @dev Uses unchecked addition; will wrap on overflow.
    /// @param amount Amount of PURGED to add to the bounty pool.
    function _addToBounty(uint256 amount) internal {
        unchecked { currentBounty += amount; }
    }

    /// @notice Pick the address with the highest luckbox from a candidate list.
    /// @param players Candidate addresses (may include address(0)).
    /// @return best Address with the maximum `playerLuckbox` value among `players` (zero if none).
    function getTopLuckbox(address[] memory players) internal view returns (address best) {
        uint256 top;
        uint256 len = players.length;
        for (uint256 i; i < len; ) {
            address p = players[i];
            if (p != address(0)) {
                uint256 v = playerLuckbox[p];
                if (v > top) { top = v; best = p; }
            }
            unchecked { ++i; }
        }
    }

    /// @notice Check whether the Decimator window is active for the current game phase.
    /// @dev Returns `(false, 0)` while purge phase (gameState==4) is active.
    /// @return on  True if level >= 25 and `level % 10 == 5` (Decimator checkpoint).
    /// @return lvl Current game level (0 if purge phase).
    function _decWindow() internal view returns (bool on, uint24 lvl) {
        IPurgeGame g = IPurgeGame(purgeGameContract);
        uint8 s = g.gameState();
        if (s == 4) return (false, 0);
        lvl = g.level();
        on = (lvl >= 25 && (lvl % 10) == 5);
    }

    /// @notice Append a player to the Decimator roster for a given level.
    /// @param lvl Level bucket to push into.
    /// @param p   Player address.
    function _decPush(uint24 lvl, address p) internal {
        uint256 idx = decPlayersCount[lvl];
        decBuckets[lvl][idx / BUCKET_SIZE].push(p);
        unchecked { decPlayersCount[lvl] = idx + 1; }
    }

    function _tierMultPermille(uint256 subBal) internal pure returns (uint16) {
        if (subBal <  200 ether) return 2000; // +100%
        if (subBal <  300 ether) return 1500; // +50%
        if (subBal <  600 ether) return 1000; //  0%
        if (subBal < 1000 ether) return  500; // -50%
        if (subBal < 2000 ether) return  100; // -90%
        return 0;                              // ≥2000: no credit
    }

    // Users call: LINK.transferAndCall(address(this), amount, "")
    function onTokenTransfer(address from, uint256 amount, bytes calldata) external {
        if (isBettingPaused) revert BettingPaused();
        if (msg.sender != address(LINK)) revert E();
        if (amount == 0) revert Zero();

        // fund VRF sub
        if (!LINK.transferAndCall(address(s_vrfCoordinator), amount, abi.encode(vrfSubscriptionId))) {
            revert E();
        }

        // post‑fund subscription LINK balance
        (uint96 bal,, , , ) = IVRFCoordinatorV2PlusLike(address(s_vrfCoordinator)).getSubscription(vrfSubscriptionId);

        uint16 mult = _tierMultPermille(uint256(bal));
        uint256 credit;
        if (mult != 0) {
            uint256 base = Math.mulDiv(amount, LUCK_PER_LINK, 1 ether); // amount is 18d LINK
            credit = Math.mulDiv(base, mult, 1000);
            uint256 newLuck = playerLuckbox[from] + credit;
            playerLuckbox[from] = newLuck;
            _updatePlayerScore(0, from, newLuck);

        }
    }


    /// @notice Route a player score update to the appropriate leaderboard.
    /// @param lid 0 = luckbox top10, 1 = affiliate top8, else = top bettor top4.
    /// @param p   Player address.
    /// @param s   New score for that board.
    function _updatePlayerScore(uint8 lid, address p, uint256 s) internal {
        if (lid == 0) {
            _updateBoard10(p, s);
        } else if (lid == 1) {
            _updateBoard8(p, s);
        } else {
            _updateBoard4(p, s);
        }
    }
    /// @notice Insert/update `p` with score `s` on the luckbox top‑10 board.
    /// @dev Keeps a 1‑based position map in `luckboxPos`. Returns true if the board changed.
    function _updateBoard10(address p, uint256 s) internal returns (bool) {
        PlayerScore[10] storage board = luckboxLeaderboard;
        uint8 curLen   = luckboxLen;
        uint8 prevPos  = luckboxPos[p]; // 1..curLen, or 0 if not present
        uint8 idx;

        // Case 1: already on board — bubble up if improved
        if (prevPos != 0) {
            idx = prevPos - 1;
            if (s <= board[idx].score) return false; // no improvement
            for (; idx > 0 && s > board[idx - 1].score; ) {
                board[idx] = board[idx - 1];
                luckboxPos[board[idx].player] = idx + 1;
                unchecked { --idx; }
            }
            board[idx] = PlayerScore(p, s);
            luckboxPos[p] = idx + 1;
            return true;
        }

        // Case 2: space available — insert and grow
        if (curLen < 10) {
            idx = curLen;
            for (; idx > 0 && s > board[idx - 1].score; ) {
                board[idx] = board[idx - 1];
                luckboxPos[board[idx].player] = idx + 1;
                unchecked { --idx; }
            }
            board[idx] = PlayerScore(p, s);
            luckboxPos[p] = idx + 1;
            unchecked { luckboxLen = curLen + 1; }
            return true;
        }

        // Case 3: full — must beat the tail to enter
        if (s <= board[9].score) return false;
        address dropped = board[9].player;
        idx = 9;
        for (; idx > 0 && s > board[idx - 1].score; ) {
            board[idx] = board[idx - 1];
            luckboxPos[board[idx].player] = idx + 1;
            unchecked { --idx; }
        }
        board[idx] = PlayerScore(p, s);
        luckboxPos[p] = idx + 1;
        if (dropped != address(0)) luckboxPos[dropped] = 0;
        return true;
    }

    /// @notice Insert/update `p` with score `s` on the affiliate top‑8 board.
    /// @dev Keeps a 1‑based position map in `affiliatePos`. Returns true if the board changed.
    function _updateBoard8(address p, uint256 s) internal returns (bool) {
        PlayerScore[8] storage board = affiliateLeaderboard;
        uint8 curLen   = affiliateLen;
        uint8 prevPos  = affiliatePos[p];
        uint8 idx;

        if (prevPos != 0) {
            idx = prevPos - 1;
            if (s <= board[idx].score) return false;
            for (; idx > 0 && s > board[idx - 1].score; ) {
                board[idx] = board[idx - 1];
                affiliatePos[board[idx].player] = idx + 1;
                unchecked { --idx; }
            }
            board[idx] = PlayerScore(p, s);
            affiliatePos[p] = idx + 1;
            return true;
        }

        if (curLen < 8) {
            idx = curLen;
            for (; idx > 0 && s > board[idx - 1].score; ) {
                board[idx] = board[idx - 1];
                affiliatePos[board[idx].player] = idx + 1;
                unchecked { --idx; }
            }
            board[idx] = PlayerScore(p, s);
            affiliatePos[p] = idx + 1;
            unchecked { affiliateLen = curLen + 1; }
            return true;
        }

        if (s <= board[7].score) return false;
        address dropped = board[7].player;
        idx = 7;
        for (; idx > 0 && s > board[idx - 1].score; ) {
            board[idx] = board[idx - 1];
            affiliatePos[board[idx].player] = idx + 1;
            unchecked { --idx; }
        }
        board[idx] = PlayerScore(p, s);
        affiliatePos[p] = idx + 1;
        if (dropped != address(0)) affiliatePos[dropped] = 0;
        return true;
    }

    /// @notice Insert/update `p` with score `s` on the top‑bettors top‑4 board.
    /// @dev Keeps a 1‑based position map in `topPos`. Returns true if the board changed.
    function _updateBoard4(address p, uint256 s) internal returns (bool) {
        PlayerScore[4] storage board = topBettors;
        uint8 curLen   = topLen;
        uint8 prevPos  = topPos[p];
        uint8 idx;

        if (prevPos != 0) {
            idx = prevPos - 1;
            if (s <= board[idx].score) return false;
            for (; idx > 0 && s > board[idx - 1].score; ) {
                board[idx] = board[idx - 1];
                topPos[board[idx].player] = idx + 1;
                unchecked { --idx; }
            }
            board[idx] = PlayerScore(p, s);
            topPos[p] = idx + 1;
            return true;
        }

        if (curLen < 4) {
            idx = curLen;
            for (; idx > 0 && s > board[idx - 1].score; ) {
                board[idx] = board[idx - 1];
                topPos[board[idx].player] = idx + 1;
                unchecked { --idx; }
            }
            board[idx] = PlayerScore(p, s);
            topPos[p] = idx + 1;
            unchecked { topLen = curLen + 1; }
            return true;
        }

        if (s <= board[3].score) return false;
        address dropped = board[3].player;
        idx = 3;
        for (; idx > 0 && s > board[idx - 1].score; ) {
            board[idx] = board[idx - 1];
            topPos[board[idx].player] = idx + 1;
            unchecked { --idx; }
        }
        board[idx] = PlayerScore(p, s);
        topPos[p] = idx + 1;
        if (dropped != address(0)) topPos[dropped] = 0;
        return true;
    }
}