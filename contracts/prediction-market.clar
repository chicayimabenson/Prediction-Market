(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-outcome (err u103))
(define-constant err-market-closed (err u104))
(define-constant err-market-resolved (err u105))
(define-constant err-insufficient-funds (err u106))
(define-constant err-no-bet (err u107))
(define-constant err-market-active (err u108))

(define-data-var next-market-id uint u1)

(define-map markets
  { market-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    creator: principal,
    end-block: uint,
    resolved: bool,
    winning-outcome: (optional uint),
    total-yes-bets: uint,
    total-no-bets: uint
  }
)

(define-map user-bets
  { market-id: uint, user: principal }
  {
    yes-amount: uint,
    no-amount: uint,
    claimed: bool
  }
)

(define-map market-participants
  { market-id: uint, user: principal }
  { exists: bool }
)

(define-read-only (get-market (market-id uint))
  (map-get? markets { market-id: market-id })
)

(define-read-only (get-user-bet (market-id uint) (user principal))
  (map-get? user-bets { market-id: market-id, user: user })
)

(define-read-only (get-next-market-id)
  (var-get next-market-id)
)

(define-read-only (calculate-payout (market-id uint) (user principal))
  (let (
    (market (unwrap! (get-market market-id) (err err-not-found)))
    (user-bet (unwrap! (get-user-bet market-id user) (err err-no-bet)))
    (winning-outcome (unwrap! (get winning-outcome market) (err err-market-active)))
    (total-pool (+ (get total-yes-bets market) (get total-no-bets market)))
  )
    (if (is-eq winning-outcome u1)
      (if (> (get yes-amount user-bet) u0)
        (ok (/ (* (get yes-amount user-bet) total-pool) (get total-yes-bets market)))
        (ok u0)
      )
      (if (> (get no-amount user-bet) u0)
        (ok (/ (* (get no-amount user-bet) total-pool) (get total-no-bets market)))
        (ok u0)
      )
    )
  )
)

(define-public (create-market (title (string-ascii 100)) (description (string-ascii 500)) (duration-blocks uint))
  (let (
    (market-id (var-get next-market-id))
    (end-block (+ stacks-block-height duration-blocks))
  )
    (map-set markets
      { market-id: market-id }
      {
        title: title,
        description: description,
        creator: tx-sender,
        end-block: end-block,
        resolved: false,
        winning-outcome: none,
        total-yes-bets: u0,
        total-no-bets: u0
      }
    )
    (var-set next-market-id (+ market-id u1))
    (ok market-id)
  )
)

(define-public (place-bet (market-id uint) (outcome uint) (amount uint))
  (let (
    (market (unwrap! (get-market market-id) err-not-found))
    (existing-bet (default-to 
      { yes-amount: u0, no-amount: u0, claimed: false }
      (get-user-bet market-id tx-sender)
    ))
  )
    (asserts! (or (is-eq outcome u1) (is-eq outcome u0)) err-invalid-outcome)
    (asserts! (< stacks-block-height (get end-block market)) err-market-closed)
    (asserts! (not (get resolved market)) err-market-resolved)
    (asserts! (>= (stx-get-balance tx-sender) amount) err-insufficient-funds)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (if (is-eq outcome u1)
      (begin
        (map-set user-bets
          { market-id: market-id, user: tx-sender }
          {
            yes-amount: (+ (get yes-amount existing-bet) amount),
            no-amount: (get no-amount existing-bet),
            claimed: false
          }
        )
        (map-set markets
          { market-id: market-id }
          (merge market { total-yes-bets: (+ (get total-yes-bets market) amount) })
        )
      )
      (begin
        (map-set user-bets
          { market-id: market-id, user: tx-sender }
          {
            yes-amount: (get yes-amount existing-bet),
            no-amount: (+ (get no-amount existing-bet) amount),
            claimed: false
          }
        )
        (map-set markets
          { market-id: market-id }
          (merge market { total-no-bets: (+ (get total-no-bets market) amount) })
        )
      )
    )
    
    (map-set market-participants
      { market-id: market-id, user: tx-sender }
      { exists: true }
    )
    
    (ok true)
  )
)

(define-public (resolve-market (market-id uint) (winning-outcome uint))
  (let (
    (market (unwrap! (get-market market-id) err-not-found))
  )
    (asserts! (is-eq tx-sender (get creator market)) err-owner-only)
    (asserts! (>= stacks-block-height (get end-block market)) err-market-active)
    (asserts! (not (get resolved market)) err-market-resolved)
    (asserts! (or (is-eq winning-outcome u1) (is-eq winning-outcome u0)) err-invalid-outcome)
    
    (map-set markets
      { market-id: market-id }
      (merge market {
        resolved: true,
        winning-outcome: (some winning-outcome)
      })
    )
    
    (ok true)
  )
)

(define-public (claim-winnings (market-id uint))
  (let (
    (market (unwrap! (get-market market-id) err-not-found))
    (user-bet (unwrap! (get-user-bet market-id tx-sender) err-no-bet))
    (payout (unwrap! (calculate-payout market-id tx-sender) err-market-active))
  )
    (asserts! (get resolved market) err-market-active)
    (asserts! (not (get claimed user-bet)) err-already-exists)
    (asserts! (> payout u0) err-insufficient-funds)
    
    (map-set user-bets
      { market-id: market-id, user: tx-sender }
      (merge user-bet { claimed: true })
    )
    
    (as-contract (stx-transfer? payout tx-sender tx-sender))
  )
)

(define-read-only (get-market-stats (market-id uint))
  (let (
    (market (unwrap! (get-market market-id) err-not-found))
    (total-pool (+ (get total-yes-bets market) (get total-no-bets market)))
  )
    (ok {
      market: market,
      total-pool: total-pool,
      yes-odds: (if (> total-pool u0) (/ (* (get total-yes-bets market) u100) total-pool) u0),
      no-odds: (if (> total-pool u0) (/ (* (get total-no-bets market) u100) total-pool) u0)
    })
  )
)

(define-read-only (is-market-active (market-id uint))
  (match (get-market market-id)
    market (and 
      (< stacks-block-height (get end-block market))
      (not (get resolved market))
    )
    false
  )
)

(define-read-only (get-user-position (market-id uint) (user principal))
  (match (get-user-bet market-id user)
    bet (ok {
      yes-amount: (get yes-amount bet),
      no-amount: (get no-amount bet),
      total-bet: (+ (get yes-amount bet) (get no-amount bet)),
      claimed: (get claimed bet)
    })
    err-no-bet
  )
)