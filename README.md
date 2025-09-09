# 🎯 Prediction Market Smart Contract

A decentralized prediction market built on Stacks blockchain where users can bet on future events and earn rewards based on outcomes! 📈

## 🚀 Features

- 📊 **Create Markets**: Anyone can create prediction markets for future events
- 💰 **Place Bets**: Users can bet on YES or NO outcomes with STX tokens
- 🏆 **Resolve Markets**: Market creators resolve outcomes when events conclude
- 💎 **Claim Winnings**: Winners receive proportional payouts from the total pool
- 📈 **Market Stats**: Real-time statistics and odds for all markets

## 🎮 How It Works

1. **Market Creation** 🏗️
   - Users create markets with title, description, and duration
   - Markets automatically close after specified block duration
   - Only creators can resolve their markets

2. **Betting System** 🎲
   - Users bet STX on YES (outcome = 1) or NO (outcome = 0)
   - Multiple bets allowed per user on same market
   - Bets locked in contract until resolution

3. **Payout Mechanism** 💸
   - Winners share the entire betting pool proportionally
   - Payout = (Your Winning Bet / Total Winning Bets) × Total Pool
   - Losers forfeit their bets to winners

## 📋 Contract Functions

### Public Functions

| Function | Description | Parameters |
|----------|-------------|------------|
| `create-market` | Create a new prediction market | `title`, `description`, `duration-blocks` |
| `place-bet` | Bet on market outcome | `market-id`, `outcome` (0/1), `amount` |
| `resolve-market` | Resolve market outcome (creator only) | `market-id`, `winning-outcome` (0/1) |
| `claim-winnings` | Claim winnings from resolved market | `market-id` |

### Read-Only Functions

| Function | Description | Returns |
|----------|-------------|---------|
| `get-market` | Get market details | Market info |
| `get-market-stats` | Get market statistics and odds | Stats with percentages |
| `get-user-position` | Get user's betting position | Bet amounts and potential payout |
| `calculate-payout` | Calculate potential winnings | Payout amount |
| `is-market-active` | Check if market accepts bets | Boolean |

## 🛠️ Usage Examples

### Creating a Market
```clarity
(contract-call? .prediction-market create-market 
  "Will Bitcoin reach $100k by 2024?" 
  "Prediction on Bitcoin price reaching $100,000 USD" 
  u1000) ;; 1000 blocks duration
```

### Placing a Bet
```clarity
;; Bet 1000 microSTX on YES
(contract-call? .prediction-market place-bet u1 u1 u1000)

;; Bet 500 microSTX on NO  
(contract-call? .prediction-market place-bet u1 u0 u500)
```

### Resolving a Market
```clarity
;; Resolve market #1 with YES outcome
(contract-call? .prediction-market resolve-market u1 u1)
```

### Claiming Winnings
```clarity
(contract-call? .prediction-market claim-winnings u1)
```

## 🔍 Market States

- **Active** 🟢: Accepting bets, before end-block
- **Closed** 🟡: Past end-block, awaiting resolution  
- **Resolved** 🔴: Outcome decided, winnings claimable

## ⚠️ Error Codes

| Code | Error | Description |
|------|-------|-------------|
| u100 | `err-owner-only` | Only market creator allowed |
| u101 | `err-not-found` | Market doesn't exist |
| u102 | `err-already-exists` | Resource already exists |
| u103 | `err-invalid-outcome` | Outcome must be 0 or 1 |
| u104 | `err-market-closed` | Market no longer accepts bets |
| u105 | `err-market-resolved` | Market already resolved |
| u106 | `err-insufficient-funds` | Not enough STX balance |
| u107 | `err-no-bet` | User has no bet in market |
| u108 | `err-market-active` | Market still active |
| u109 | `err-invalid-amount` | Amount must be greater than 0 |
| u110 | `err-already-claimed` | Winnings already claimed |

## 🧪 Testing

Deploy with Clarinet and test core functionality:

```bash
clarinet console
```

````clarity
;; Create test market
(contract-call? .prediction-market create-market "Test Market" "Test Description" u100)

;; Place test bets

