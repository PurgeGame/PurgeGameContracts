// SPDX-License-Identifier: MIT
pragma solidity ^0.8.26;

import "erc721a/contracts/ERC721A.sol";
import "./JackpotUtils.sol";

/**
 * @title Purge Game — Core NFT game contract
 * @notice This file defines the on-chain game logic surface (interfaces + core state).
 * @dev The full audit will annotate each function as it is reviewed. This section covers
 *      declarations, configuration, and constructor. No state-variable renames are applied here
 *      to avoid breaking linkages in later functions; comments call out semantics and risks.
 */

// ===========================================================================
// External Interfaces
// ===========================================================================

/**
 * @dev Interface to the PURGE ERC20 + game-side coordinator.
 *      All calls are trusted (set via constructor in the ERC20 contract).
 */
interface IPurgeCoinInterface {
    function mintInGame(address recipient, uint256 amount) external;
    function burnInGame(address target, uint256 amount) external;
    function payAffiliate(uint256 amount, bytes32 code, address sender, uint24 lvl) external;
    function requestRngPurgeGame(bool dontStopFlips) external returns (uint256 reqId);
    function pullRng() external view returns (uint256 word);
    function processCoinflipPayouts(uint24 level, uint32 cap, bool bonusFlip)external returns (bool);
    function triggerCoinJackpot() external;
    function runExternalJackpot(
        uint8 kind,
        uint256 poolWei,
        uint32 cap,
        uint24 lvl
    ) external returns (bool finished, address[] memory winners, uint256[] memory amounts, uint256 returnAmountWei);
    function resetAffiliateLeaderboard() external;
    function getLeaderboardAddresses(uint8 which) external view returns (address[] memory);
    function playerLuckbox(address player) external view returns (uint256);
}

/**
 * @dev Interface to the off-chain renderer used for tokenURI generation.
 */
interface IPurgeRenderer {
    function setStartingTraitRemaining(uint32[256] calldata values) external;
    function tokenURI(
        uint256 tokenId,
        uint256 data,
        uint32[4] calldata remaining
    ) external view returns (string memory);
}

// ===========================================================================
// Contract
// ===========================================================================

contract PurgeGame is ERC721A {
    // -----------------------
    // Custom Errors
    // -----------------------
    error E();                 // Generic guard (reverts in multiple paths)
    error LuckboxTooSmall();   // Caller does not meet luckbox threshold for the action
    error RngNotReady();       // RNG not yet fulfilled and not expired
    error NotTimeYet();        // Called in a phase where the action is not permitted
    error WrongValue();        // Ether value mismatch

    // -----------------------
    // Events
    // -----------------------
    event PlayerCredited(address indexed player, uint256 amount);
    event Jackpot(uint256 traits); // Encodes jackpot metadata
    event Purge(address indexed player, uint256[] tokenIds);

    // -----------------------
    // Immutable Addresses
    // -----------------------
    address private immutable _renderer; // Trusted renderer; used for tokenURI composition
    address private immutable _coin;     // Trusted coin/game-side coordinator (PURGE ERC20)

    // -----------------------
    // Game Constants
    // -----------------------
    uint48  private constant JACKPOT_RESET_TIME = 82620; // Offset anchor for "daily" windows
    uint256 private constant MILLION = 1_000_000;        // 6-decimal unit helper
    uint256 private constant PRICE_PURGECOIN  = 1000 * MILLION;   // 1,000 Purgecoin (6d)
    uint32  private constant NFT_AIRDROP_PLAYER_BATCH_SIZE = 75;    // Mint batch cap (# players)
    uint32  private constant NFT_AIRDROP_TOKEN_CAP         = 3_000; // Mint batch cap (# tokens)
    uint72  private constant MAP_PERMILLE = 0x0A0A07060304050564;   // Payout permilles packed (9 bytes)

    // -----------------------
    // Economic / Admin
    // -----------------------
    address private immutable creator;      // Receives protocol ETH (end-game drains, etc.)
    uint256 private price = 0.025 ether;    // Base mint price (adjusts with level milestones)                  / MILLION FOR TESTNET

    // -----------------------
    // Prize Pools and RNG
    // -----------------------
    uint256 private lastPrizePool     = 125 ether; // Snapshot from previous epoch (non-zero post L1)         / MILLION FOR TESTNET
    uint256 private levelPrizePool;                // Snapshot for endgame distribution of current level
    uint256 private prizePool;                     // Live ETH pool for current level
    uint256 private carryoverForNextLevel;         // Saved carryover % for next level
    uint256 private rngWord;                       // Cached fulfilled RNG word (game-level scope)

    // -----------------------
    // Time / Session Tracking
    // -----------------------
    uint48 private levelStartTime = type(uint48).max; // Wall-clock start of current level
    uint48 private dailyIdx;                          // Daily session index (derived from JACKPOT_RESET_TIME)
    uint48 private rngTs;                             // Timestamp of last RNG request/fulfillment

    // -----------------------
    // Game Progress
    // -----------------------
    uint24 public level      = 1;                // 1-based level counter
    uint8  public gameState  = 1;                // Phase FSM (audited in advanceGame() review)
    uint8  private jackpotCounter;               // # of daily jackpots paid in current level
    uint8  private earlyPurgeJackpotPaidMask;    // Bitmask for early purge jackpots paid (progressive)
    uint8  private phase;                        // Airdrop sub-phase (0..7)

    // -----------------------
    // RNG Liveness Flags
    // -----------------------
    bool private rngFulfilled; // Last RNG request has been fulfilled
    bool private rngConsumed;  // Current RNG has been consumed by game logic

    // -----------------------
    // Minting / Airdrops
    // -----------------------
    uint32 private purchaseCount;              // Total purchased NFTs this level (incl. bonuses)
    uint64 private baseTokenId = 1;            // Rolling base for token IDs across levels
    uint32 private airdropMapsProcessedCount;  // Progress inside current map-mint player's queue
    uint32 private airdropIndex;               // Progress across players in pending arrays

    address[] private pendingNftMints;         // Queue of players with owed NFTs
    address[] private pendingMapMints;         // Queue of players with owed "map" mints

    mapping(address => uint32) private playerTokensOwed;   // Player => NFTs owed
    mapping(address => uint32) private playerMapMintsOwed; // Player => Maps owed

    // -----------------------
    // Token / Trait State
    // -----------------------
    mapping(uint256 => uint24) private tokenTraits;         // Packed 4×6-bit traits (low to high)
    mapping(address => uint256) private claimableWinnings;  // ETH claims accumulated on-chain
    mapping(uint256 => uint256) private trophyData;         // Trophy metadata by tokenId
    uint256[] private trophyTokenIds;                       // Historical trophies

    /**
     * @dev Per-level storage (avoid large structs in arrays; keep mappings nested for gas).
     * - `endgamePayoutAmount` — per-ticket payout for purge contributors on exterminated trait.
     * - `exterminatedTrait`  — 0..255 when a trait wins; 420 sentinel when "no extermination" / rollover.
     * - `traitContributions` — contribution counts per trait per address for endgame payouts.
     * - `traitPurgeTicket`   — per-trait ticket arrays for jackpot drawings.
     */
    struct LevelData {
        uint256 endgamePayoutAmount;
        uint16  exterminatedTrait;
        mapping(uint8 => mapping(address => uint32)) traitContributions;
        mapping(uint8 => address[])                  traitPurgeTicket;
    }
    mapping(uint24 => LevelData) private L;

    // -----------------------
    // Daily / Trait Counters
    // -----------------------
    uint32[80]  internal dailyPurgeCount;   // 8 + 8 + 64
    uint32[256] internal traitRemaining;    // Remaining supply per trait (0..255)

    // -----------------------
    // Constructor
    // -----------------------

    /**
     * @param purgeCoinContract Trusted PURGE ERC20 / game coordinator address
     * @param renderer_         Trusted off-chain renderer
     *
     * @dev Mints a sentinel tokenId=0 to the contract (reserved trophy slot). Initializes
     *      sentinel values for L[0] / trophyData to simplify edge checks in early levels.
     */
    constructor(address purgeCoinContract, address renderer_) ERC721A("Purge Game", "PG") {
        creator   = msg.sender;
        _coin     = purgeCoinContract;
        _renderer = renderer_;

        // Mint a sentinel token to contract for trophy bookkeeping (tokenId 0).
        _mint(address(this), 1);

        // Initialize sentinel trophyData for tokenId 0 (0xFFFF in high 16 bits of slot schema).
        trophyData[0] = (0xFFFF << 152);

        // Level 0 sentinel: no extermination (420 = sentinel "none")
        L[0].exterminatedTrait = 420;
    }
    // --- View: lightweight game status -------------------------------------------------

    /// @notice Public snapshot of selected game state for UI/indexers.
    /// @return phase_                   Current airdrop sub-phase (0..7)
    /// @return jackpotCounter_          Number of daily jackpots processed this level
    /// @return price_                   Current mint price (wei)
    /// @return lastPrizePool_           Previous level's effective prize pool snapshot (wei)
    /// @return prizePool_               Current level’s live prize pool (wei)
    /// @return enoughPurchases          True if purchaseCount >= 1500
    /// @return rngFulfilled_            Last VRF request fulfilled
    /// @return rngConsumed_             Last VRF word consumed by game logic
    function gameInfo()
        external
        view
        returns (
            uint8 phase_,
            uint8 jackpotCounter_,
            uint256 price_,
            uint256 lastPrizePool_,
            uint256 prizePool_,
            bool enoughPurchases,
            bool rngFulfilled_,
            bool rngConsumed_
        )
    {
        phase_                   = phase;
        jackpotCounter_          = jackpotCounter;
        price_                   = price;
        lastPrizePool_           = lastPrizePool;
        prizePool_               = prizePool;
        enoughPurchases          = (purchaseCount >= 1500);
        rngFulfilled_            = rngFulfilled;
        rngConsumed_             = rngConsumed;
    }

    // --- State machine: advance one tick ------------------------------------------------

    /// @notice Advances the game state machine. Anyone can call, but certain steps
    ///         require the caller to meet a luckbox threshold (payment for work/luckbox bonus).
    /// @param cap Emergency unstuck function, in case a necessary transaction is too large for a block.
    ///            Using cap removes Purgecoin payment.
    function advanceGame(uint32 cap) external {
        uint48 ts = uint48(block.timestamp);

        // Liveness drain
        if (ts - 365 days > levelStartTime) {
            gameState = 0;
            phase = 6;
            uint256 bal = address(this).balance;
            if (bal > 0) { address dst = creator; assembly { pop(call(gas(), dst, bal, 0, 0, 0, 0)) } }
        }

        uint24 lvl  = level;
        uint8  s    = gameState;
        uint8  ph   = phase;
        bool   dontStopFlips = (s == 2) && (ph < 3) && (lvl % 20 == 0);

        IPurgeCoinInterface coin = IPurgeCoinInterface(_coin);
        uint48 day = uint48((ts - JACKPOT_RESET_TIME) / 1 days);
        uint48 dayIdx = dailyIdx;

        do {
            // RNG pull / SLA
            if (rngTs != 0 && !rngFulfilled) {
                uint256 w = coin.pullRng();
                if (w != 0) { rngFulfilled = true; rngWord = w; rngTs = ts; }
                else if (ts - rngTs > 6 hours) { _requestVrf(ts, dontStopFlips); break; }
                else revert RngNotReady();
            }

            // luckbox rewards
            if (cap == 0 && coin.playerLuckbox(msg.sender) < PRICE_PURGECOIN * lvl) revert LuckboxTooSmall();

            // Arm VRF when due/new (reward allowed)
            if ((rngConsumed && day != dayIdx) || rngTs == 0) { _requestVrf(ts, dontStopFlips); break; }
            if (day == dayIdx && s != 1) revert NotTimeYet();

            // --- State 1 - Pregame ---
            if (s == 1) {
                _finalizeEndgame(lvl);
                if (gameState == 1 && _endJackpot(lvl, cap, day, false)) gameState = 2;
                break;
            }

            // --- State 2 - Purchase ---
            if (s == 2) {
                _updateEarlyPurgeJackpots(lvl);
                if (ph == 2 && purchaseCount >= 1500 && prizePool >= lastPrizePool) {
                    if (lvl % 100 > 90) price = (price * 4) / 5;
                        if (_endJackpot(lvl, cap, day, false)) phase = 3;
                        break;
                }else if (ph == 3 && jackpotCounter == 0) {
                    gameState = 3;
                    IPurgeRenderer(_renderer).setStartingTraitRemaining(traitRemaining);
                    if (_processMapBatch(cap)) { phase = 4; airdropIndex = 0; airdropMapsProcessedCount = 0; }
                    break;
                }
                if (airdropIndex < pendingMapMints.length) { _processMapBatch(cap); break; }
                if (jackpotCounter > 0) { payDailyJackpot(false, lvl); break; }

                _endJackpot(lvl, cap, day, false);
                break;
            }

            // --- State 3 - Airdrop ---
            if (s == 3) {
                if (ph == 3) {
                    if (_processMapBatch(cap)) { phase = 4; airdropIndex = 0; airdropMapsProcessedCount = 0; }
                    break;
                }
                if (ph == 4) { if (payMapJackpot(cap, lvl)) phase = 5; break; }
                if (ph == 5) {
                    if (_processNftBatch(cap)) { 
                        if (lvl % 100 > 90) price = (price * 125) / 100; 
                        phase = 6; 
                    }
                    break;
                }
                if (_endJackpot(lvl, cap, day, false)) { levelStartTime = ts; gameState = 4; }
                break;
            }

            // --- State 4 - Purge ---
            if (s == 4) {
                if (ph == 6) {
                    payDailyJackpot(true, lvl);
                    coin.triggerCoinJackpot();
                    phase = 7;
                    break;
                }
                bool bonusFlip = (jackpotCounter == 2);
                if (_endJackpot(lvl, cap, day, bonusFlip)) phase = 6;
                break;
            }

            // --- State 0 ---
            if (s == 0) { _endJackpot(lvl, cap, day, false); }
        } while (false);

        if (s != 0 && cap == 0) coin.mintInGame(msg.sender, PRICE_PURGECOIN);
    }




    // --- Purchases: schedule NFT mints (traits precomputed) ----------------------------------------

    /// @notice Records a purchase during state 2, either paying in Purgecoin or ETH.
    /// @dev
    /// - ETH path forwards affiliate credit to the coin contract.
    /// - No NFTs are minted here; this only schedules mints and precomputes traits.
    /// - Traits are derived from the current VRF word snapshot (`rngWord`) at call time.
    /// @param quantity Number of base items to purchase (1..100).
    /// @param payInCoin If true, burn Purgecoin instead of paying ETH.
    /// @param affiliateCode Optional affiliate code for ETH purchases (ignored for coin payments).
    function purchase(
        uint256 quantity,
        bool payInCoin,
        bytes32 affiliateCode
    ) external payable {
        uint8 ph = phase;
        if (quantity == 0 || quantity > 100 || (!rngConsumed && ph == 3) || ph > 3) revert NotTimeYet();
        uint24 lvl = level;
        if (lvl % 100 == 0) {
            if (IPurgeCoinInterface(_coin).playerLuckbox(msg.sender) < 10 * PRICE_PURGECOIN * (lvl / 100)) revert LuckboxTooSmall();
        }

        // Payment handling (ETH vs coin)
        if (payInCoin) {
            if (msg.value != 0) revert WrongValue();
            _coinReceive(quantity * PRICE_PURGECOIN, lvl);
        } else {
            _ethReceive(quantity * 100, affiliateCode, lvl); // price × (quantity * 100) / 100
        }

        // Compute total scheduled outputs (10% bonus)
        uint32 totalScheduled;
        unchecked {
            totalScheduled = uint32(quantity + quantity / 10);
        }

        // Push buyer to the pending list once (de-dup)
        if (playerTokensOwed[msg.sender] == 0) {
            pendingNftMints.push(msg.sender);
        }

        // Precompute traits for future mints using the current RNG snapshot.
        // NOTE: tokenIds are not minted yet; only trait counters are updated.
        uint256 randomWord   = rngWord;
        uint256 tokenIdStart = uint256(baseTokenId) + uint256(purchaseCount);
        for (uint32 i; i < totalScheduled; ) {
            uint256 _tokenId = tokenIdStart + i;
            uint256 rand = uint256(keccak256(abi.encodePacked(_tokenId, randomWord)));
            uint8 tA = _getTrait(uint64(rand));
            uint8 tB = _getTrait(uint64(rand >>  64));
            uint8 tC = _getTrait(uint64(rand >> 128));
            uint8 tD = _getTrait(uint64(rand >> 192));

            // Pack 4x6-bit traits (A,B,C,D) into 24 bits.
            tokenTraits[_tokenId] = uint24(tA)
                | (uint24(tB) << 6)
                | (uint24(tC) << 12)
                | (uint24(tD) << 18);

            // Increment remaining counts for each tier-mapped trait bucket.
            unchecked {
                traitRemaining[tA]          += 1;       // base 0..63
                traitRemaining[tB |  64]    += 1;       // 64..127
                traitRemaining[tC | 128]    += 1;       // 128..191
                traitRemaining[tD | 192]    += 1;       // 192..255
            }
            unchecked { ++i; }
        }

        // Accrue scheduled mints to the buyer
        unchecked {
            playerTokensOwed[msg.sender] += totalScheduled;
            purchaseCount               += totalScheduled;
        }
    }

    // --- “Mint & Purge” map flow: schedule symbol drops --------------------------------------------

    /// @notice Buys map symbols in state 2 and immediately schedules purge-map entries.
    /// @dev
    /// - Requires the current RNG word to be already consumed (fresh session).
    /// - ETH path: mints a coin rebate if quantity > 3.
    /// - No on-chain burning/minting of NFTs here; symbols are scheduled and later batched.
    /// @param quantity Number of map entries requested (≥1).
    /// @param payInCoin If true, pay with Purgecoin (with level-conditional multiplier).
    /// @param affiliateCode Optional affiliate code for ETH payments.
    function mintAndPurge(
        uint256 quantity,
        bool payInCoin,
        bytes32 affiliateCode
    ) external payable {
        if (gameState != 2 || quantity == 0 || !rngConsumed) revert NotTimeYet();
        uint24 lvl = level;
        if (lvl % 100 == 0) {
            if (IPurgeCoinInterface(_coin).playerLuckbox(msg.sender) < 10 * PRICE_PURGECOIN * (lvl / 100)) revert LuckboxTooSmall();
        }

        // Pricing / rebates
        uint256 qty    = quantity;
        uint256 isOne  = qty == 1 ? 1 : 0;
        uint256 isTwo  = qty == 2 ? 1 : 0;
        uint256 scaledQty = qty * 25 + isOne * 13 + isTwo * 10; // ETH path scale factor (÷100 later)
        uint256 coinCost  = qty * (PRICE_PURGECOIN / 4) + (PRICE_PURGECOIN * (130 * isOne + 100 * isTwo)) / 1000;
        uint256 rebate    = (qty / 4) * PRICE_PURGECOIN / 10;

        if (payInCoin) {
            if (msg.value != 0) revert WrongValue();
            _coinReceive(coinCost - rebate, lvl);
        } else {
            _ethReceive(scaledQty, affiliateCode, lvl);
            if (rebate != 0) IPurgeCoinInterface(_coin).mintInGame(msg.sender, rebate);
        }

        // Schedule symbol mints (extra 4 per each block of 40)
        uint32 totalMaps = uint32(qty + 4 * (qty / 40));

        if (playerMapMintsOwed[msg.sender] == 0) {
            pendingMapMints.push(msg.sender);
        }
        unchecked {
            playerMapMintsOwed[msg.sender] += totalMaps;
        }
    }
    // --- Purging NFTs into tickets & potentially ending the level -----------------------------------

    /// @notice Burn up to 75 owned NFTs to contribute purge tickets; may end the level if a trait hits zero.
    /// @dev
    /// Security:
    /// - Blocks contract calls (`extcodesize(caller()) != 0`) and `tx.origin` relays.
    /// - Requires `gameState == 4` and a consumed RNG session.
    /// Accounting:
    /// - For each token, burns it, updates daily counters and remaining-per-trait,
    ///   and appends four tickets (one per trait) to the current level’s buckets.
    /// - If any trait’s remaining count reaches 0 during the loop, `_endLevel(trait)` is invoked
    ///   and the function returns immediately (tickets for that *last* NFT are not recorded).
    /// Rewards:
    /// - Mints Purgecoin to the caller: base `n` plus up to +0.9×n (in tenths) if the NFT
    ///   included last level’s exterminated trait.
    function purge(uint256[] calldata tokenIds) external {
        // Disallow contracts and meta-tx relays
        uint256 extSize;
        assembly {
            extSize := extcodesize(caller())
        }
        if (extSize != 0 || tx.origin != msg.sender) revert E();

        if (gameState != 4 || !rngConsumed) revert NotTimeYet();

        uint256 count = tokenIds.length;
        if (count == 0 || count > 75) revert E();
        uint24 lvl = level;

        uint16 prevExterminated = L[lvl - 1].exterminatedTrait;
        LevelData storage lvlData = L[lvl];
        mapping(uint8 => address[]) storage purgeTickets = lvlData.traitPurgeTicket;

        address caller = msg.sender;
        uint256 bonusTenths;

        for (uint256 i; i < count; ) {
            uint256 tokenId = tokenIds[i];

            // Ownership & trophy checks
            if (ownerOf(tokenId) != caller || trophyData[tokenId] != 0) revert E();

            // Decode the four traits packed in `tokenTraits[tokenId]`
            uint24 traits = tokenTraits[tokenId];
            uint8 trait0 = uint8(traits & 0x3F);
            uint8 trait1 = (uint8(traits >> 6) & 0x3F) | 64;
            uint8 trait2 = (uint8(traits >> 12) & 0x3F) | 128;
            uint8 trait3 = (uint8(traits >> 18) & 0x3F) | 192;

            // Bonus if the NFT includes last level’s exterminated trait
            if (
                uint16(trait0) == prevExterminated ||
                uint16(trait1) == prevExterminated ||
                uint16(trait2) == prevExterminated ||
                uint16(trait3) == prevExterminated
            ) {
                unchecked {
                    bonusTenths += 9; // +0.9 per such token
                }
            }

            // Burn first, then counters / termination checks
            _burn(tokenId, false);

            unchecked {
                // Daily counters (grouped by class/slot)
                dailyPurgeCount[trait0 & 0x07] += 1;
                dailyPurgeCount[((trait1 - 64) >> 3) + 8] += 1;
                dailyPurgeCount[trait2 - 128 + 16] += 1;

                // Per-trait remaining supply; end the level immediately when any hits zero
                if (--traitRemaining[trait0] == 0) {
                    _endLevel(trait0);
                    return;
                }
                if (--traitRemaining[trait1] == 0) {
                    _endLevel(trait1);
                    return;
                }
                if (--traitRemaining[trait2] == 0) {
                    _endLevel(trait2);
                    return;
                }
                if (--traitRemaining[trait3] == 0) {
                    _endLevel(trait3);
                    return;
                }

                ++i;
            }

            // Append four tickets (and contributions) for this token’s traits
            purgeTickets[trait0].push(caller);
            unchecked {
                lvlData.traitContributions[trait0][caller] += 1;
            }

            purgeTickets[trait1].push(caller);
            unchecked {
                lvlData.traitContributions[trait1][caller] += 1;
            }

            purgeTickets[trait2].push(caller);
            unchecked {
                lvlData.traitContributions[trait2][caller] += 1;
            }

            purgeTickets[trait3].push(caller);
            unchecked {
                lvlData.traitContributions[trait3][caller] += 1;
            }
        }

        // Purge reward: base n + bonus tenths (scaled by PRICE_PURGECOIN/10)
        if (lvl % 10 == 2) count <<= 1;
        IPurgeCoinInterface(_coin).mintInGame(caller, (count + bonusTenths) * (PRICE_PURGECOIN / 10));

        emit Purge(msg.sender, tokenIds);
    }


    // --- Level finalization -------------------------------------------------------------------------

    /// @notice Finalize the current level either due to a trait being exterminated (<256) or a timed-out “420” end.
    /// @dev
    /// When a trait is exterminated (<256):
    /// - Lock the exterminated trait in the current level’s storage.
    /// - Split prize pool: 90% to participants, 10% to trophies and affiliates (handled elsewhere),
    ///   from which the “exterminator” gets 20% (or 40% for 7th steps since L25),
    ///   and the rest is divided evenly per ticket for the exterminated trait.
    /// - Mint the level trophy (transfers the placeholder token owned by the contract).
    /// - Start next level and seed `levelPrizePool`.
    ///
    /// When a non-trait end occurs (>=256, e.g. daily jackpots path):
    /// - Carry the entire `prizePool` forward and reset leaderboards.
    /// - On L%100==0: adjust price and `lastPrizePool`.
    ///
    /// After either path:
    /// - Reset per-level state, mint the next level’s trophy placeholder,
    ///   set default exterminated sentinel to 420, and request fresh VRF.
    function _endLevel(uint16 exterminated) private {
        address exterminator = msg.sender;
        uint256 trophyId = baseTokenId - 1;

        uint24 levelSnapshot = level;
        LevelData storage lvlData = L[levelSnapshot];

        if (exterminated < 256) {
            // Record exterminated trait
            lvlData.exterminatedTrait = uint8(exterminated);

            // Current pool snapshot
            uint256 pool = prizePool;

            // Halving if same trait as prior level
            uint16 prev = L[levelSnapshot - 1].exterminatedTrait;
            if (exterminated == prev) {
                uint256 keep = pool >> 1;
                carryoverForNextLevel += keep;
                pool -= keep;
            }

            // Participant vs exterminator split
            uint256 ninetyPercent = (pool * 90) / 100;
            uint256 exterminatorShare =
                (levelSnapshot % 10 == 7 && levelSnapshot >= 25) ? (pool * 40) / 100 : (pool * 20) / 100;

            uint256 participantShare;
            unchecked {
                participantShare = ninetyPercent - exterminatorShare;
            }

            // Per-ticket endgame payout for the exterminated trait
            uint8 exTrait = uint8(exterminated);
            uint256 ticketsLen = L[levelSnapshot].traitPurgeTicket[exTrait].length;
            lvlData.endgamePayoutAmount = (ticketsLen == 0) ? 0 : (participantShare / ticketsLen);


            // Award trophy (transfer placeholder owned by contract)
            this.transferFrom(address(this), exterminator, trophyId);
            trophyTokenIds.push(trophyId);
            trophyData[trophyId] = (uint256(exTrait) << 152) | (uint256(levelSnapshot) << 128);

            // Seed the next phase’s pool
            levelPrizePool = pool;
            _clearDailyPurgeCount();

        } else {
            // Non-trait end (e.g., daily jackpot progression end)
            lvlData.exterminatedTrait = 420;
            trophyData[trophyId] = 0;

            if (levelSnapshot % 100 == 0) {
                price = 0.05 ether;
                lastPrizePool = prizePool >> 3;
            }

            carryoverForNextLevel += prizePool;
            IPurgeCoinInterface(_coin).resetAffiliateLeaderboard();
        }

        // Move to the next level’s storage index
        unchecked {
            levelSnapshot++; level++;
        }
        // Reset level state
        prizePool = 0;
        for (uint16 t; t < 256; ) {
            traitRemaining[t] = 0;
            unchecked {
                ++t;
            }
        }
        

        delete pendingNftMints;
        delete pendingMapMints;
        airdropIndex = 0;
        airdropMapsProcessedCount = 0;
        jackpotCounter = 0;
        earlyPurgeJackpotPaidMask = 0;
        

        // Mint next level’s trophy placeholder to the contract
        _mint(address(this), 1);
        baseTokenId = uint64(_nextTokenId());
        trophyData[baseTokenId - 1] = (0xFFFF << 152);

        // Initialize next level’s exterminated sentinel
        L[levelSnapshot].exterminatedTrait = 420;

        uint256 mod100 = levelSnapshot % 100;

        if (mod100 == 10 || mod100 == 0) {
            price <<= 1;
        } else if (levelSnapshot % 20 == 0) {
            price += (levelSnapshot < 100) ? 0.05 ether : 0.1 ether;
        }
    

        purchaseCount = 0;
        gameState = 1;

        // Request fresh randomness for the new level
        _requestVrf(uint48(block.timestamp), false);
    }
    // --- Endgame finalization (post-trait extermination resolution) ---------------------------------

    /// @notice Resolve previous level’s endgame payouts and transition toward the next session.
    /// @dev
    /// - Only runs when the previous level ended by extermination (`exterminatedTrait != 420`).
    /// - Distributes affiliate rewards, historical trophy bonuses, and the exterminator’s share.
    /// - Seeds/clears state for the next cycle; may end the 100‑level epoch (`gameState = 0`).
    function _finalizeEndgame(uint24 lvl) internal {
        // Work on the *previous* level; current `level` has already advanced in `_endLevel`.
        uint24 prevLevel;
        unchecked { prevLevel = lvl - 1; }
        phase = 0;
        

        // Only finalize if the prior level ended by trait extermination.
        if (L[prevLevel].exterminatedTrait == 420) { _clearDailyPurgeCount(); return ;}

        // Snapshot of the level’s endgame pool (fixed at `_endLevel` time).
        uint256 poolTotal = levelPrizePool;
        if (poolTotal == 0) return;

        // --- Affiliate reward (10% for L1, 5% otherwise) -------------------------------------------
        uint256 affPool = (prevLevel == 1) ? (poolTotal * 10) / 100 : (poolTotal * 5) / 100;
        address[] memory affLeaders = IPurgeCoinInterface(_coin).getLeaderboardAddresses(1);
        if (affLeaders.length > 0) _addClaimableEth(affLeaders[0], (affPool * 50) / 100);
        if (affLeaders.length > 1) _addClaimableEth(affLeaders[1], (affPool * 25) / 100);
        if (affLeaders.length > 2) _addClaimableEth(affLeaders[2], (affPool * 15) / 100);
        if (affLeaders.length > 3) {
            // Randomize among positions [3..end)
            address rndAddr = affLeaders[3 + (rngWord % (affLeaders.length - 3))];
            _addClaimableEth(rndAddr, (affPool * 10) / 100);
        }

        // --- Historical trophy bonus (5% spread across two past trophies) ---------------------------
        if (prevLevel > 1) {
            uint256 trophyPool = poolTotal / 20; // 5%
            uint256 trophiesLen = trophyTokenIds.length;

            if (trophiesLen > 1) {
                // Pick two indices from [0..trophiesLen-2]
                uint256 idA = trophyTokenIds[rngWord % (trophiesLen - 1)];
                uint256 idB = trophyTokenIds[(rngWord >> 128) % (trophiesLen - 1)];

                uint256 halfA = trophyPool >> 1;
                uint256 halfB = trophyPool - halfA;

                if (_exists(idA)) {
                    _addClaimableEth(ownerOf(idA), halfA);
                    trophyPool -= halfA;
                }
                if (_exists(idB)) {
                    _addClaimableEth(ownerOf(idB), halfB);
                    trophyPool -= halfB;
                }
            }

            // Any undistributed dust rolls forward.
            if (trophyPool != 0) carryoverForNextLevel += trophyPool;
        }

        // --- Exterminator’s share (20% or 40%), plus epoch carry if prevLevel%100==0 ----------------
        uint256 exterminatorShare = (prevLevel % 10 == 7 && prevLevel >= 25)
            ? (poolTotal * 40) / 100
            : (poolTotal * 20) / 100;

        if ((prevLevel % 100) == 0) {
            // End of 100‑level epoch: fold accumulated carry into the exterminator’s credit.
            exterminatorShare += carryoverForNextLevel;
        }

        address exterminatorOwner = ownerOf(trophyTokenIds[trophyTokenIds.length - 1]);
        _addClaimableEth(exterminatorOwner, exterminatorShare);

        // Clear the starting pool marker (consumed by this finalize stage).
        levelPrizePool = 0;

        // Reset affiliate board for the new cycle.
        IPurgeCoinInterface(_coin).resetAffiliateLeaderboard();

        // Epoch conclusion (L%100==0) halts the game; otherwise proceed into main session.
        if ((prevLevel % 100) == 0) {
            gameState = 0;
            phase = 6;
            return;
        }
    }


    // --- Claiming winnings (ETH) --------------------------------------------------------------------

    /// @notice Claim or quote the caller’s accrued ETH winnings (affiliates, jackpots, endgame payouts).
    /// @param player The account whose winnings to aggregate.
    /// @param pay    If true, transfers ETH; if false, returns the computed amount only.
    /// @return amount Total amount in wei attributed to `player`.
    function claimWinnings(address player, bool pay) public returns (uint256 amount) {
        if (pay && player != msg.sender) revert E();

        uint24 lvl = level;
        uint24 scanFrom = lvl > 100 ? lvl - 100 : 1;

        // Start with explicit credit bucket (affiliates / jackpots / misc).
        amount = claimableWinnings[player];

        // Add per-level endgame payouts (only for levels with a real exterminated trait).
        for (uint24 i = scanFrom; i < lvl; ) {
            LevelData storage lvlData = L[i];
            uint16 ex = lvlData.exterminatedTrait;
            if (ex < 256) {
                uint256 payoutPerTicket = lvlData.endgamePayoutAmount;
                if (payoutPerTicket != 0) {
                    uint8 exTrait = uint8(ex);
                    uint256 tickets = lvlData.traitContributions[exTrait][player];
                    if (tickets != 0) {
                        unchecked { amount += tickets * payoutPerTicket; }
                        if (pay) lvlData.traitContributions[exTrait][player] = 0; // consume tickets
                    }
                }
            }
            unchecked { ++i; }
        }

        if (!pay) return amount;

        // Zero explicit credit *before* external call.
        claimableWinnings[player] = 0;

        // Payout
        (bool ok, ) = payable(player).call{value: amount}("");
        if (!ok) revert E();
    }
    // --- Credits & jackpot helpers ------------------------------------------------------------------

    /// @notice Credit ETH winnings to a player’s claimable balance and emit an accounting event.
    /// @param beneficiary Player to credit.
    /// @param weiAmount   Amount in wei to add.
    function _addClaimableEth(address beneficiary, uint256 weiAmount) private {
        unchecked {
            claimableWinnings[beneficiary] += weiAmount;
        }
        emit PlayerCredited(beneficiary, weiAmount);
    }

    /// @notice Map jackpot specification for a given bucket index.
    /// @dev Returns the number of winners and the bucket’s permille share of the prize.
    ///      Index layout (0..8) corresponds to distinct trait groups.
    /// @param index Bucket index.
    /// @return winnersN Number of winners to draw for this bucket.
    /// @return permille Permille share (out of 1000) allocated to this bucket.
    function _mapSpec(uint8 index) internal pure returns (uint8 winnersN, uint8 permille) {
        winnersN = (index & 1) == 1 ? 1 : (index == 0 ? 1 : 20);
        permille = uint8(MAP_PERMILLE >> (uint256(index) * 8));
    }

    /// @notice Expose trait-ticket sampling (view helper for off-chain verification / UI).
    function getJackpotWinners(
        uint256 randomWord,
        uint8 trait,
        uint8 numWinners,
        uint8 salt
    ) external view returns (address[] memory) {
        uint24 lvl = level;
        if (gameState == 1) lvl -=1;
        return JackpotUtils._randTraitTicket(
            L[lvl].traitPurgeTicket,
            randomWord,
            trait,
            numWinners,
            salt
        );
    }


    // --- Map jackpot payout (end of purchase phase) -------------------------------------------------

    /// @notice Compute carry split, set the new prize pool, and pay the 9 map buckets.
    /// @dev
    /// - May invoke external jackpot pipelines (BAF / Decimator) and short‑circuit for continuation.
    /// - Chooses 9 traits from RNG; allocates permille slices; doubles buckets at steps 30/50/70.
    /// - Packs chosen traits into `Jackpot` event high‑bits payload (kind=9).
    /// @param cap Step limit forwarded to external jackpot processors.
    /// @return finished True when complete; false if an external phase needs more calls.
    function payMapJackpot(uint32 cap, uint24 lvl) internal returns (bool finished) {
        uint256 carryWei = carryoverForNextLevel;
        

        // Optional external jackpots (return early until each phase reports finished).
        if (lvl % 20 == 0) {
            uint256 bafPoolWei = (carryWei * 24) / 100; // 24% to BAF
            if (!_progressExternal(0, bafPoolWei, cap, lvl)) return false;
        } else if (lvl >= 25 && (lvl % 10) == 5)  {
            uint256 decPoolWei = (carryWei * 15) / 100; // 15% to Decimator
            if (!_progressExternal(1, decPoolWei, cap, lvl)) return false;
        }
        // Mint a small Purgecoin bonus to creator based on total ETH in the prize pool.

        // Re-read carry after any external phase may have returned funds.
        carryWei = carryoverForNextLevel;
        uint256 totalWei = carryWei + prizePool;
        IPurgeCoinInterface(_coin).mintInGame(creator, (totalWei * 10 * PRICE_PURGECOIN) / 1 ether);
        // Derive level-dependent saving percentage into next level.
        uint256 rndWord = uint256(keccak256(abi.encode(rngWord, uint8(3))));
        uint256 savePct;
        if (lvl < 10) {
            savePct = uint256(lvl) * 5;
        } else {
            if      (lvl < 20) savePct = 55 + (rndWord % 16);
            else if (lvl < 40) savePct = 55 + (rndWord % 21);
            else if (lvl < 60) savePct = 60 + (rndWord % 21);
            else if (lvl < 80) savePct = 60 + (rndWord % 26);
            else if (lvl == 99) savePct = 93;
            else                      savePct = 65 + (rndWord % 26);
            if (lvl % 10 == 9) savePct += 5;
        }

        // Compute effective pool for this map (and rollover savings).
        uint256 effectiveWei;
        if (lvl % 100 == 0) {
            effectiveWei = totalWei;
            carryoverForNextLevel = 0;
        } else {
            uint256 saveNextWei = (totalWei * savePct) / 100;
            carryoverForNextLevel = saveNextWei;
            effectiveWei = totalWei - saveNextWei;
        }

        // Rotate pools: remember last prize pool baseline and set current effective pool.
        lastPrizePool = prizePool;
        prizePool = effectiveWei;

        // Trait selection & payouts (9 buckets).
        bool doubleMap = ((lvl % 100) == 30) || ((lvl % 100) == 50) || ((lvl % 100) == 70);
        uint256 packedTraits;
        uint256 totalPaidWei;

        for (uint8 idx; idx < 9; ) {
            (uint8 winnersN, uint8 permille) = _mapSpec(idx);

            // First bucket: use full 8 random bits; subsequent buckets: 6 bits + quadrant offset.
            uint8 traitId = (idx == 0)
                ? uint8(rndWord & 0xFF)
                : uint8((rndWord >> (8+((idx-1) * 6)) & 0x3F) + ((idx - 1) << 6));

            // Pack the chosen trait (8 bits per slot).
            packedTraits |= uint256(traitId) << (idx * 8);

            // Draw winners from the per‑trait ticket pool (at most `winnersN`).
            address[] memory winners = JackpotUtils._randTraitTicket(
                L[lvl].traitPurgeTicket,
                rndWord,
                traitId,
                winnersN,
                uint8(42 + idx)
            );

            // Bucket value: permille slice of effective pool; doubled at steps 30/50/70.
            uint256 bucketWei = (effectiveWei * permille) / 1000;
            if (doubleMap) bucketWei <<= 1;

            // Split equally among actual winners.
            uint256 winnersLen = winners.length;
            if (winnersLen != 0) {
                uint256 prizeEachWei = bucketWei / winnersLen;
                uint256 paidWei = prizeEachWei * winnersLen;
                unchecked { totalPaidWei += paidWei; }

                for (uint256 k; k < winnersLen; ) {
                    _addClaimableEth(winners[k], prizeEachWei);
                    unchecked { ++k; }
                }
            }

            unchecked { ++idx; }
        }

        // Reduce prize pool by distributions; seed next stage’s starting pool.
        unchecked {
            prizePool -= totalPaidWei;
            levelPrizePool = prizePool;
        }



        // Emit packed traits (header=9 indicates map jackpot event).
        emit Jackpot((uint256(9) << 248) | packedTraits);
        return true;
    }
    // --- Daily & early‑purge jackpots ---------------------------------------------------------------

    /// @notice Pay four jackpot groups either during daily payouts or early‑purge sessions.
    /// @dev
    /// - For daily runs, the RNG word is the current VRF word; for early‑purge runs it is salted with the
    ///   current `jackpotCounter`.
    /// - Winning traits come from either observed purge counts (daily) or uniform draw (early).
    /// - Group sizes scale with the step within the 100‑level epoch: 1× for L..00–19, 2× for 20–39, …, 5× for 80–99.
    /// - Per‑group allocation is an equal split of `dailyTotal / 4` across sampled winners (integer division).
    /// - Updates `jackpotCounter` up/down depending on the mode and emits a `Jackpot(kind=4, traits...)` event.
    function payDailyJackpot(bool isDaily, uint24 lvl) internal {
        // RNG source for this run.
        uint256 randWord = isDaily
            ? rngWord
            : uint256(keccak256(abi.encode(rngWord, uint8(5), jackpotCounter)));

        // Resolve current level & ticket storage.

        LevelData storage lvlData = L[lvl];
        mapping(uint8 => address[]) storage ticketsByTrait = lvlData.traitPurgeTicket;

        // Pick the four winning traits.
        uint8[4] memory winningTraits = isDaily
            ? JackpotUtils._getWinningTraits(randWord, dailyPurgeCount)
            : JackpotUtils._getRandomTraits(randWord);

        // Scale number of winners per group by position within the 100‑level epoch.
        uint8 stepWithinCentury = uint8(lvl % 100);
        uint256 multiplier = 1 + (stepWithinCentury / 20); // 1..5

        // Determine total pot for this run.
        uint256 dailyTotalWei;
        if (isDaily) {
            // 2.5%, 3.0%, …, 9.5% of levelPrizePool as jackpotCounter grows (sum≈90% over 15 days).
            dailyTotalWei = (levelPrizePool * (250 + uint256(jackpotCounter) * 50)) / 10_000;
        } else {
            // Early‑purge session takes a slice from carryover.
            uint256 baseWei = carryoverForNextLevel;
            if ((lvl % 20) == 0) {
                dailyTotalWei = baseWei / 100;           // 1.0%
            } else if (lvl >= 21 && (lvl % 10) == 1) {
                dailyTotalWei = (baseWei * 6) / 100;     // 6.0%
            } else {
                dailyTotalWei = baseWei / 40;            // 2.5%
            }
        }

        // Split pot equally across the four groups.
        uint256 perGroupWei = dailyTotalWei / 4;
        uint256 totalPaidWei;

        // Pay each group.
        for (uint8 groupIdx; groupIdx < 4; ) {
            // Winner targets per group (scaled by epoch step).
            uint256 wantWinners =
                groupIdx == 0 ? 30 * multiplier :
                groupIdx == 1 ? 20 * multiplier :
                groupIdx == 2 ? 10 * multiplier : 1;

            // Sample winners from the trait’s ticket pool.
            address[] memory winners = JackpotUtils._randTraitTicket(
                ticketsByTrait,
                randWord,
                winningTraits[groupIdx],
                uint8(wantWinners),
                uint8(69 + groupIdx)
            );

            // Equal split for this group.
            uint256 winnersLen = winners.length;
            if (winnersLen != 0) {
                uint256 prizeEachWei = perGroupWei / winnersLen;
                if (prizeEachWei != 0) {
                    unchecked { totalPaidWei += prizeEachWei * winnersLen; }
                    for (uint256 k; k < winnersLen; ) {
                        _addClaimableEth(winners[k], prizeEachWei);
                        unchecked { ++k; }
                    }
                }
            }

            unchecked { ++groupIdx; }
        }

        // Emit packed winning traits (kind=4).
        emit Jackpot(
            (uint256(4) << 248)
            | uint256(winningTraits[0])
            | (uint256(winningTraits[1]) << 8)
            | (uint256(winningTraits[2]) << 16)
            | (uint256(winningTraits[3]) << 24)
        );

        // Bookkeeping by mode.
        if (isDaily) {
            unchecked { ++jackpotCounter; }
            prizePool -= totalPaidWei;
            if (jackpotCounter >= 15 || (lvl % 100 == 0 && jackpotCounter == 14)) { _endLevel(420); return; } // End-of-cycle sweep
            _clearDailyPurgeCount();
        } else {
            unchecked { carryoverForNextLevel -= totalPaidWei; --jackpotCounter; }
        }
    }

    function _endJackpot(uint24 lvl, uint32 cap, uint48 dayIdx, bool bonusFlip)
        private
        returns (bool ok)
    {
        ok = IPurgeCoinInterface(_coin).processCoinflipPayouts(lvl, cap, bonusFlip);
        if (!ok) return false;
        dailyIdx = dayIdx;
        rngConsumed = true;
    }

    /// @notice Track early‑purge jackpot thresholds as the prize pool grows during purchase phase.
    /// @dev
    /// - Compares current `prizePool` to `lastPrizePool` as a percentage and sets a 6‑bit mask for
    ///   thresholds {10,20,30,40,50,75}%.
    /// - Increments `jackpotCounter` by the number of newly crossed thresholds since the last call
    ///   using a popcount trick on the delta mask.
    function _updateEarlyPurgeJackpots(uint24 lvl) internal {
        if (lvl < 10 || lvl == 99) return;

        uint256 prevPoolWei = lastPrizePool;
        uint256 pctOfLast = (prizePool * 100) / prevPoolWei; // requires non‑zero `lastPrizePool`

        // Build target mask for crossed thresholds.
        uint8 targetMask =
            (pctOfLast >= 10 ? uint8(1)  : 0) |
            (pctOfLast >= 20 ? uint8(2)  : 0) |
            (pctOfLast >= 30 ? uint8(4)  : 0) |
            (pctOfLast >= 40 ? uint8(8)  : 0) |
            (pctOfLast >= 50 ? uint8(16) : 0) |
            (pctOfLast >= 75 ? uint8(32) : 0);

        // Count bits that are newly set compared to the last paid mask.
        uint8 paidMask = earlyPurgeJackpotPaidMask;
        uint8 addMask  = targetMask & ~paidMask;
        uint8 newCountBits = addMask;
        unchecked {
            newCountBits = newCountBits - ((newCountBits >> 1) & 0x55);
            newCountBits = (newCountBits & 0x33) + ((newCountBits >> 2) & 0x33);
            newCountBits = (newCountBits + (newCountBits >> 4)) & 0x0F; // popcount
        }

        earlyPurgeJackpotPaidMask = targetMask;
        jackpotCounter += newCountBits;
    }
    // --- Flips, VRF, payments, rarity ----------------------------------------------------------------

    /// @notice Arm a new VRF request and reset RNG state.
    /// @param ts Current block timestamp (48-bit truncated).
    /// @param dontStopFlips If true, keeps coinflip betting running on the coin contract during VRF wait.
    function _requestVrf(uint48 ts, bool dontStopFlips) internal {
        rngFulfilled = false;
        rngWord      = 0;
        rngTs        = ts;
        rngConsumed  = false;
        IPurgeCoinInterface(_coin).requestRngPurgeGame(dontStopFlips);
    }

    /// @notice Handle ETH payments for purchases; forwards affiliate rewards in Purgecoin.
    /// @param scaledQty Quantity scaled by 100 (to keep integer math with `price`).
    /// @param affiliateCode Affiliate/referral code provided by the buyer.
    function _ethReceive(uint256 scaledQty, bytes32 affiliateCode, uint24 lvl) private {
        uint256 expectedWei = (price * scaledQty) / 100;
        if (msg.value != expectedWei) revert WrongValue();

        unchecked { prizePool += msg.value; }

        IPurgeCoinInterface coin = IPurgeCoinInterface(_coin);
        // Affiliate mint uses Purgecoin units; 0.1% of scaledQty (scaled by 100) -> /1000
        unchecked { coin.payAffiliate((scaledQty * PRICE_PURGECOIN) / 1000, affiliateCode, msg.sender, lvl); }
    }

    /// @notice Handle Purgecoin coin payments for purchases; 
    /// @dev 1.5x cost on steps where `level % 20 == 13`. 10% discount on 18
    function _coinReceive(uint256 amount, uint24 lvl) private {
        if (lvl % 20 == 13) amount = (amount * 3) / 2;
        else if (lvl % 20 == 18) amount = (amount * 9) / 10;
        IPurgeCoinInterface(_coin).burnInGame(msg.sender, amount);
    }

    // --- Map / NFT airdrop batching ------------------------------------------------------------------

    /// @notice Process a batch of “map” mints for players awaiting symbol credits.
    /// @dev
    /// - Iterates contiguous `pendingMapMints` starting at `airdropIndex`.
    /// - For each player, consumes up to `cap` total symbols across players in this call,
    ///   preserving progress via `airdropMapsProcessedCount`.
    /// - Transitions `phase` from 0→1 the first time any symbols are produced.
    /// @param capHint If non‑zero, use this per‑call symbol cap; otherwise use defaults based on `phase`.
    /// @return finished True if all pending map mints have been fully processed.
    function _processMapBatch(uint32 capHint) private returns (bool finished) {
        uint256 totalPlayers = pendingMapMints.length;
        if (airdropIndex >= totalPlayers) return true;
        uint8 ph = phase;

        // Select cap: phase 0 uses a smaller default, otherwise larger. Unclamped when caller provides a hint.
        uint32 cap = (capHint == 0)
            ? (ph < 2 ? uint32(100 + ph * 50) : uint32(325))
            : capHint;

        uint32 processed = 0;
        uint256 entropyWord = rngWord;

        while (airdropIndex < totalPlayers && processed < cap) {
            address player = pendingMapMints[airdropIndex];
            uint32 owed = playerMapMintsOwed[player];
            if (owed == 0) {
                unchecked { ++airdropIndex; }
                airdropMapsProcessedCount = 0;
                continue;
            }

            uint32 room = cap - processed;
            uint32 take = owed > room ? room : owed;

            // Derive a per‑player base key to diversify symbol generation deterministically.
            uint256 baseKey = baseTokenId + (uint256(airdropIndex) << 20);
            _raritySymbolBatch(player, baseKey, airdropMapsProcessedCount, take, entropyWord);

            

            uint32 left = owed - take;
            playerMapMintsOwed[player] = left;
            airdropMapsProcessedCount += take;
            processed += take;

            if (left == 0) {
                unchecked { ++airdropIndex; }
                airdropMapsProcessedCount = 0;
            }
        }
        if (ph < 2) phase += 1;
        finished = (airdropIndex >= totalPlayers);
    }

    /// @notice Mint a batch of NFTs owed to players, bounded by the number of players and a token cap.
    /// @dev
    /// - Processes up to `playersToProcess` players starting at `airdropIndex`.
    /// - Mints up to `NFT_AIRDROP_TOKEN_CAP` tokens in total for this transaction.
    /// @param playersToProcess If zero, defaults to `NFT_AIRDROP_PLAYER_BATCH_SIZE`.
    /// @return finished True if all pending NFT mints have been fully processed.
    function _processNftBatch(uint32 playersToProcess) private returns (bool finished) {
        uint256 totalPlayers = pendingNftMints.length;
        if (airdropIndex >= totalPlayers) return true;

        uint32 players = (playersToProcess == 0) ? NFT_AIRDROP_PLAYER_BATCH_SIZE : playersToProcess;
        uint256 endIdx = airdropIndex + players;
        if (endIdx > totalPlayers) endIdx = totalPlayers;

        // Hard cap on total NFTs minted per call to control work.
        uint32 tokenCap = NFT_AIRDROP_TOKEN_CAP;
        uint32 minted = 0;

        while (airdropIndex < endIdx) {
            address player = pendingNftMints[airdropIndex];
            uint32 owed = playerTokensOwed[player];
            if (owed == 0) { unchecked { ++airdropIndex; } continue; }

            uint32 room = tokenCap - minted;
            if (room == 0) return false;

            uint32 chunk = owed > room ? room : owed;
            _mint(player, chunk);

            minted += chunk;
            owed   -= chunk;
            playerTokensOwed[player] = owed;

            if (owed == 0) { unchecked { ++airdropIndex; } }
        }
        return airdropIndex >= totalPlayers;
    }

    /// @notice Generate `count` random “map symbols” for `player`, record tickets & contributions.
    /// @dev
    /// - Uses a xorshift* PRNG seeded per 16‑symbol group from `(baseKey + group, entropyWord)`.
    /// - Each symbol maps to one of 256 trait buckets (0..63 | 64..127 | 128..191 | 192..255) via `_getTrait`.
    /// - After counting occurrences per trait in memory, appends `player` into the purge ticket list
    ///   for each touched trait exactly `occurrences` times and increases contribution counters.
    /// @param player      Recipient whose tickets/contributions are updated.
    /// @param baseKey     Per‑player base key (e.g., derived from `baseTokenId` and player index).
    /// @param startIndex  Starting symbol index for this player (resume support).
    /// @param count       Number of symbols to generate in this call.
    /// @param entropyWord Global RNG word for this level-step; combined with `baseKey` for per-group seeds.
    function _raritySymbolBatch(
        address player,
        uint256 baseKey,
        uint32  startIndex,
        uint32  count,
        uint256 entropyWord
    ) private {
        uint32[256] memory counts;           // per-trait symbol counts in this batch
        uint8[256]  memory touchedTraits;    // compact list of traits touched
        uint16      touchedLen;

        uint32 endIndex = startIndex + count;
        uint32 i = startIndex;

        // Generate symbols in groups of 16 using a per-group seed; xorshift* inside the group.
        while (i < endIndex) {
            uint32 groupIdx = i >> 4; // i / 16
            uint256 seed = uint256(keccak256(abi.encodePacked(baseKey + groupIdx, entropyWord)));
            uint64 s = uint64(seed) | 1;

            // align to the correct intra-group position
            uint8 offset = uint8(i & 15);
            for (uint8 skip; skip < offset; ) {
                s ^= (s >> 12); s ^= (s << 25); s ^= (s >> 27);
                s *= 2685821657736338717;
                unchecked { ++skip; }
            }

            // now produce symbols from the aligned position
            for (uint8 j = offset; j < 16 && i < endIndex; ) {
                s ^= (s >> 12); s ^= (s << 25); s ^= (s >> 27);
                uint64 rnd64 = s * 2685821657736338717;

                uint8 quadrant = uint8(i & 3);
                uint8 traitId  = _getTrait(rnd64) + (quadrant << 6);

                if (counts[traitId]++ == 0) { touchedTraits[touchedLen++] = traitId; }
                unchecked { ++i; ++j; }
            }
        }


        // Persist results: update contributions and append tickets.
        LevelData storage curr = L[level];
        mapping(uint8 => address[]) storage tickets = curr.traitPurgeTicket;

        for (uint16 u; u < touchedLen; ) {
            uint8  traitId      = touchedTraits[u];
            uint32 occurrences  = counts[traitId];

            unchecked { curr.traitContributions[traitId][player] += occurrences; }

            // Append `player` `occurrences` times to the dynamic array tickets[traitId].
            assembly {
                mstore(0x00, traitId)
                mstore(0x20, tickets.slot)
                let arr := keccak256(0x00, 0x40)     // storage slot of tickets[traitId]
                let len := sload(arr)                // current array length
                sstore(arr, add(len, occurrences))   // increase length by `occurrences`
                mstore(0x00, arr)
                let data := keccak256(0x00, 0x20)    // base slot for array elements
                let addr := player
                // Write `player` sequentially `occurrences` times
                for { let k := 0 } lt(k, occurrences) { k := add(k, 1) } {
                    sstore(add(data, add(len, k)), addr)
                }
            }

            unchecked { ++u; }
        }
    }
    // --- Trait weighting / helpers -------------------------------------------------------------------

    /// @notice Map a 32-bit random input to an 0..7 bucket with a fixed piecewise distribution.
    /// @dev Distribution over 75 slots: [10,10,10,10,9,9,9,8] → buckets 0..7 respectively.
    function _w8(uint32 rnd) private pure returns (uint8) {
        unchecked {
            uint32 scaled = uint32((uint64(rnd) * 75) >> 32);
            if (scaled < 10) return 0;
            if (scaled < 20) return 1;
            if (scaled < 30) return 2;
            if (scaled < 40) return 3;
            if (scaled < 49) return 4;
            if (scaled < 58) return 5;
            if (scaled < 67) return 6;
            return 7;
        }
    }

    /// @notice Produce a 6-bit trait id from a 64-bit random value.
    /// @dev High‑level: two weighted 3‑bit values (category, sub) → pack to a single 6‑bit trait.
    function _getTrait(uint64 rnd) private pure returns (uint8) {
        uint8 category = _w8(uint32(rnd));
        uint8 sub      = _w8(uint32(rnd >> 32));
        return (category << 3) | sub;
    }

    /// @notice Reset the per‑day purge counters (80 buckets).
    function _clearDailyPurgeCount() internal {
        for (uint8 i; i < 80; ) {
            dailyPurgeCount[i] = 0;
            unchecked { ++i; }
        }
    }

    // --- External jackpot coordination ---------------------------------------------------------------

    /// @notice Progress an external jackpot (BAF / Decimator) and account winners locally.
    /// @param kind  External jackpot kind (0 = BAF, 1 = Decimator).
    /// @param poolWei Total wei allocated for this external jackpot stage.
    /// @param cap   Step cap forwarded to the external processor.
    /// @return finished True if the external stage reports completion.
    function _progressExternal(uint8 kind, uint256 poolWei, uint32 cap, uint24 lvl) internal returns (bool finished) {
        if (poolWei == 0) return true;
        if (kind == 0 && (rngWord & 1) == 0) return true;

        (bool isFinished, address[] memory winnersArr, uint256[] memory amountsArr, uint256 returnWei) =
            IPurgeCoinInterface(_coin).runExternalJackpot(kind, poolWei, cap, lvl);

        uint256 n = winnersArr.length;
        for (uint256 i; i < n; ) {
            _addClaimableEth(winnersArr[i], amountsArr[i]);
            unchecked { ++i; }
        }

        if (isFinished) {
            // Decrease carryover by the spent portion (poolWei - returnWei).
            carryoverForNextLevel -= (poolWei - returnWei);
        }
        return isFinished;
    }

    // --- Views / overrides ---------------------------------------------------------------------------

    /// @notice Read‑only quote of pending winnings for `msg.sender` without modifying state.
    /// @dev Uses a staticcall to reuse the accounting in `claimWinnings(p,false)`.
    function quoteWinnings() external view returns (uint256 amt) {
        address player = msg.sender;
        (bool ok, bytes memory out) =
            address(this).staticcall(abi.encodeWithSelector(this.claimWinnings.selector, player, false));
        if (!ok || out.length < 32) revert E();
        amt = abi.decode(out, (uint256));
    }

    /// @inheritdoc ERC721A
    /// @dev Disallows reading ownership for pre‑trophy era tokens unless they are trophies.
    function ownerOf(uint256 tokenId) public view override returns (address) {
        if (tokenId < baseTokenId - 1 && trophyData[tokenId] == 0) revert E();
        return super.ownerOf(tokenId);
    }
    // --- Metadata / views --------------------------------------------------------------------------------

    /// @inheritdoc ERC721A
    /// @dev
    /// - Trophy tokens (trophyData[tokenId] != 0) are always valid across eras.
    /// - Non‑trophy tokens from prior eras (tokenId < baseTokenId - 1) are invalidated.
    /// - For regular tokens, returns metadata encoded as:
    ///     bits [63:48] : last exterminated trait (uint16)
    ///     bits [47:24] : current level (uint24)
    ///     bits [23:00] : packed 4×6‑bit traits (uint24)
    function tokenURI(uint256 tokenId) public view override returns (string memory) {
        uint256 trophyInfo = trophyData[tokenId];

        // Disallow old non‑trophies from previous eras.
        if (tokenId < baseTokenId - 1 && trophyInfo == 0) revert E();

        // Trophy: delegate to renderer with the packed trophy info; remaining array is zeroed.
        uint32[4] memory remaining;
        if (trophyInfo != 0) {
            return IPurgeRenderer(_renderer).tokenURI(tokenId, trophyInfo, remaining);
        }



        // Load packed traits and compute the four trait IDs in their 0..255 namespaces.
        uint24 traitsPacked = tokenTraits[tokenId];
        uint256 lastExterminated = L[level - 1].exterminatedTrait;

        uint8 t0 = uint8(traitsPacked);                 // 0..63
        uint8 t1 = uint8(traitsPacked >>  6) |  64;     // 64..127
        uint8 t2 = uint8(traitsPacked >> 12) | 128;     // 128..191
        uint8 t3 = uint8(traitsPacked >> 18) | 192;     // 192..255

        // Pack high‑level game info + traits for the renderer.
        uint256 metaPacked = (lastExterminated << 48) | (uint256(level) << 24) | uint256(traitsPacked);

        // Provide remaining counts per trait namespace (live view of current level state).
        remaining[0] = traitRemaining[t0];
        remaining[1] = traitRemaining[t1];
        remaining[2] = traitRemaining[t2];
        remaining[3] = traitRemaining[t3];

        return IPurgeRenderer(_renderer).tokenURI(tokenId, metaPacked, remaining);
    }

    /// @notice Return pending mints/maps owed to a player (airdrop queues).
    function getPlayerPurchases(address player)
        external
        view
        returns (uint32 mints, uint32 maps)
    {
        mints = playerTokensOwed[player];
        maps  = playerMapMintsOwed[player];
    }

    /// @notice Return the caller’s ticket counts per trait for the *current* level.
    /// @dev tickets[i] is the number of contributions for trait `i` (0..255).
    function getTickets(address player) external view returns (uint32[256] memory tickets) {
        LevelData storage curr = L[level];
        for (uint256 i; i < 256; ) {
            tickets[i] = curr.traitContributions[uint8(i)][player];
            unchecked { ++i; }
        }
    }
}