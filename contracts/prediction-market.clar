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
    (creator-rep (get-creator-reputation tx-sender))
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
    
    (map-set creator-reputation
      { creator: tx-sender }
      (merge creator-rep { markets-created: (+ (get markets-created creator-rep) u1) })
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
    (creator-rep (get-creator-reputation (get creator market)))
    (resolution-time (- stacks-block-height (get end-block market)))
    (total-volume (+ (get total-yes-bets market) (get total-no-bets market)))
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
    
    (let (
      (new-resolved-count (+ (get markets-resolved creator-rep) u1))
      (new-total-volume (+ (get total-volume-created creator-rep) total-volume))
      (current-avg-time (get avg-resolution-time creator-rep))
      (new-avg-time (if (> new-resolved-count u1)
        (/ (+ (* current-avg-time (- new-resolved-count u1)) resolution-time) new-resolved-count)
        resolution-time))
      (new-reputation-score (calculate-reputation-score (get markets-created creator-rep) new-resolved-count new-avg-time))
    )
      (map-set creator-reputation
        { creator: (get creator market) }
        (merge creator-rep {
          markets-resolved: new-resolved-count,
          total-volume-created: new-total-volume,
          avg-resolution-time: new-avg-time,
          reputation-score: new-reputation-score
        })
      )
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


(define-constant err-invalid-category (err u109))
(define-constant err-invalid-limit (err u110))
(define-constant err-invalid-coverage (err u111))
(define-constant err-insurance-exists (err u112))
(define-constant err-no-insurance (err u113))
(define-constant err-claim-processed (err u114))
(define-constant err-insufficient-pool (err u115))
(define-constant err-already-rated (err u116))
(define-constant err-invalid-rating (err u117))
(define-constant err-self-rating (err u118))

(define-data-var next-category-id uint u1)
(define-data-var insurance-pool uint u0)

(define-map categories
  { category-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    market-count: uint,
    total-volume: uint
  }
)

(define-map category-names
  { name: (string-ascii 50) }
  { category-id: uint }
)

(define-map market-categories
  { market-id: uint }
  { category-id: uint }
)

(define-map category-markets
  { category-id: uint, market-id: uint }
  { exists: bool }
)

(define-map market-keywords
  { market-id: uint, keyword: (string-ascii 30) }
  { exists: bool }
)

(define-map keyword-markets
  { keyword: (string-ascii 30), market-id: uint }
  { exists: bool }
)

(define-map market-activity
  { market-id: uint }
  {
    bet-count: uint,
    last-activity-block: uint,
    participant-count: uint
  }
)

(define-map insurance-policies
  { market-id: uint, user: principal }
  {
    coverage-amount: uint,
    premium-paid: uint,
    bet-outcome: uint,
    bet-amount: uint,
    claim-processed: bool
  }
)

(define-map market-insurance-stats
  { market-id: uint }
  {
    total-policies: uint,
    total-premiums: uint,
    total-coverage: uint
  }
)

(define-map creator-reputation
  { creator: principal }
  {
    markets-created: uint,
    markets-resolved: uint,
    total-volume-created: uint,
    avg-resolution-time: uint,
    reputation-score: uint
  }
)

(define-map market-ratings
  { market-id: uint, rater: principal }
  {
    rating: uint,
    comment: (string-ascii 200)
  }
)

(define-map market-rating-summary
  { market-id: uint }
  {
    total-ratings: uint,
    rating-sum: uint,
    avg-rating: uint
  }
)

(define-map user-rating-count
  { user: principal }
  { ratings-given: uint }
)

(define-read-only (get-category (category-id uint))
  (map-get? categories { category-id: category-id })
)

(define-read-only (get-category-by-name (name (string-ascii 50)))
  (match (map-get? category-names { name: name })
    entry (get-category (get category-id entry))
    none
  )
)

(define-read-only (get-market-category (market-id uint))
  (match (map-get? market-categories { market-id: market-id })
    entry (get-category (get category-id entry))
    none
  )
)

(define-read-only (get-next-category-id)
  (var-get next-category-id)
)

(define-read-only (get-insurance-policy (market-id uint) (user principal))
  (map-get? insurance-policies { market-id: market-id, user: user })
)

(define-read-only (get-insurance-pool)
  (var-get insurance-pool)
)

(define-read-only (calculate-insurance-premium (bet-amount uint) (coverage-percentage uint))
  (let (
    (base-rate u5)
    (coverage-ratio (/ (* coverage-percentage u100) u100))
    (premium (/ (* bet-amount base-rate coverage-ratio) u1000))
  )
    (if (< premium u1) u1 premium)
  )
)

(define-read-only (get-market-insurance-stats (market-id uint))
  (default-to 
    { total-policies: u0, total-premiums: u0, total-coverage: u0 }
    (map-get? market-insurance-stats { market-id: market-id })
  )
)

(define-read-only (get-creator-reputation (creator principal))
  (default-to 
    { markets-created: u0, markets-resolved: u0, total-volume-created: u0, avg-resolution-time: u0, reputation-score: u500 }
    (map-get? creator-reputation { creator: creator })
  )
)

(define-read-only (get-market-rating (market-id uint) (rater principal))
  (map-get? market-ratings { market-id: market-id, rater: rater })
)

(define-read-only (get-market-rating-summary (market-id uint))
  (default-to 
    { total-ratings: u0, rating-sum: u0, avg-rating: u0 }
    (map-get? market-rating-summary { market-id: market-id })
  )
)

(define-read-only (calculate-reputation-score (markets-created uint) (markets-resolved uint) (avg-resolution-time uint))
  (let (
    (resolution-rate (if (> markets-created u0) (/ (* markets-resolved u100) markets-created) u0))
    (time-bonus (if (and (> avg-resolution-time u0) (< avg-resolution-time u1000)) u100 u50))
    (volume-bonus (if (> markets-created u10) u50 u0))
  )
    (+ u500 resolution-rate time-bonus volume-bonus)
  )
)

(define-public (create-category (name (string-ascii 50)) (description (string-ascii 200)))
  (let (
    (category-id (var-get next-category-id))
  )
    (asserts! (is-none (map-get? category-names { name: name })) err-invalid-category)
    
    (map-set categories
      { category-id: category-id }
      {
        name: name,
        description: description,
        market-count: u0,
        total-volume: u0
      }
    )
    
    (map-set category-names
      { name: name }
      { category-id: category-id }
    )
    
    (var-set next-category-id (+ category-id u1))
    (ok category-id)
  )
)

(define-public (assign-market-category (market-id uint) (category-id uint))
  (let (
    (category (unwrap! (get-category category-id) err-not-found))
  )
    (map-set market-categories
      { market-id: market-id }
      { category-id: category-id }
    )
    
    (map-set category-markets
      { category-id: category-id, market-id: market-id }
      { exists: true }
    )
    
    (map-set categories
      { category-id: category-id }
      (merge category { market-count: (+ (get market-count category) u1) })
    )
    
    (ok true)
  )
)

(define-public (add-market-keywords (market-id uint) (keyword1 (string-ascii 30)) (keyword2 (optional (string-ascii 30))) (keyword3 (optional (string-ascii 30))))
  (begin
    (map-set market-keywords
      { market-id: market-id, keyword: keyword1 }
      { exists: true }
    )
    (map-set keyword-markets
      { keyword: keyword1, market-id: market-id }
      { exists: true }
    )
    
    (match keyword2
      kw2 (begin
        (map-set market-keywords
          { market-id: market-id, keyword: kw2 }
          { exists: true }
        )
        (map-set keyword-markets
          { keyword: kw2, market-id: market-id }
          { exists: true }
        )
      )
      true
    )
    
    (match keyword3
      kw3 (begin
        (map-set market-keywords
          { market-id: market-id, keyword: kw3 }
          { exists: true }
        )
        (map-set keyword-markets
          { keyword: kw3, market-id: market-id }
          { exists: true }
        )
      )
      true
    )
    
    (ok true)
  )
)

(define-public (update-market-activity (market-id uint) (bet-amount uint))
  (let (
    (current-activity (default-to 
      { bet-count: u0, last-activity-block: u0, participant-count: u0 }
      (map-get? market-activity { market-id: market-id })
    ))
  )
    (map-set market-activity
      { market-id: market-id }
      {
        bet-count: (+ (get bet-count current-activity) u1),
        last-activity-block: stacks-block-height,
        participant-count: (get participant-count current-activity)
      }
    )
    (ok true)
  )
)

(define-read-only (get-markets-by-category (category-id uint) (limit uint))
  (begin
    (asserts! (and (> limit u0) (<= limit u50)) err-invalid-limit)
    (ok category-id)
  )
)

(define-read-only (search-markets-by-keyword (keyword (string-ascii 30)))
  (ok (is-some (map-get? keyword-markets { keyword: keyword, market-id: u1 })))
)

(define-read-only (get-trending-categories (limit uint))
  (begin
    (asserts! (and (> limit u0) (<= limit u20)) err-invalid-limit)
    (ok limit)
  )
)

(define-read-only (get-market-activity-score (market-id uint))
  (match (map-get? market-activity { market-id: market-id })
    activity (let (
      (blocks-since-activity (- stacks-block-height (get last-activity-block activity)))
      (recency-score (if (< blocks-since-activity u1000) (- u1000 blocks-since-activity) u0))
      (activity-score (* (get bet-count activity) u10))
    )
      (ok (+ recency-score activity-score))
    )
    (ok u0)
  )
)

(define-read-only (get-category-stats (category-id uint))
  (match (get-category category-id)
    category (ok {
      category: category,
      market-count: (get market-count category),
      total-volume: (get total-volume category)
    })
    err-not-found
  )
)

(define-read-only (is-market-in-category (market-id uint) (category-id uint))
  (is-some (map-get? category-markets { category-id: category-id, market-id: market-id }))
)

(define-read-only (has-market-keyword (market-id uint) (keyword (string-ascii 30)))
  (is-some (map-get? market-keywords { market-id: market-id, keyword: keyword }))
)

(define-public (update-category-volume (category-id uint) (volume-increase uint))
  (let (
    (category (unwrap! (get-category category-id) err-not-found))
  )
    (map-set categories
      { category-id: category-id }
      (merge category { total-volume: (+ (get total-volume category) volume-increase) })
    )
    (ok true)
  )
)

(define-public (purchase-insurance (market-id uint) (bet-outcome uint) (bet-amount uint) (coverage-percentage uint))
  (let (
    (market (unwrap! (get-market market-id) err-not-found))
    (premium (calculate-insurance-premium bet-amount coverage-percentage))
    (coverage-amount (/ (* bet-amount coverage-percentage) u100))
    (current-stats (get-market-insurance-stats market-id))
  )
    (asserts! (and (> coverage-percentage u0) (<= coverage-percentage u100)) err-invalid-coverage)
    (asserts! (is-none (get-insurance-policy market-id tx-sender)) err-insurance-exists)
    (asserts! (>= (stx-get-balance tx-sender) premium) err-insufficient-funds)
    (asserts! (< stacks-block-height (get end-block market)) err-market-closed)
    (asserts! (not (get resolved market)) err-market-resolved)
    
    (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
    (var-set insurance-pool (+ (var-get insurance-pool) premium))
    
    (map-set insurance-policies
      { market-id: market-id, user: tx-sender }
      {
        coverage-amount: coverage-amount,
        premium-paid: premium,
        bet-outcome: bet-outcome,
        bet-amount: bet-amount,
        claim-processed: false
      }
    )
    
    (map-set market-insurance-stats
      { market-id: market-id }
      {
        total-policies: (+ (get total-policies current-stats) u1),
        total-premiums: (+ (get total-premiums current-stats) premium),
        total-coverage: (+ (get total-coverage current-stats) coverage-amount)
      }
    )
    
    (ok true)
  )
)

(define-public (claim-insurance (market-id uint))
  (let (
    (market (unwrap! (get-market market-id) err-not-found))
    (policy (unwrap! (get-insurance-policy market-id tx-sender) err-no-insurance))
    (winning-outcome (unwrap! (get winning-outcome market) err-market-active))
    (coverage-amount (get coverage-amount policy))
  )
    (asserts! (get resolved market) err-market-active)
    (asserts! (not (get claim-processed policy)) err-claim-processed)
    (asserts! (not (is-eq (get bet-outcome policy) winning-outcome)) err-invalid-outcome)
    (asserts! (>= (var-get insurance-pool) coverage-amount) err-insufficient-pool)
    
    (map-set insurance-policies
      { market-id: market-id, user: tx-sender }
      (merge policy { claim-processed: true })
    )
    
    (var-set insurance-pool (- (var-get insurance-pool) coverage-amount))
    (as-contract (stx-transfer? coverage-amount tx-sender tx-sender))
  )
)

(define-public (fund-insurance-pool (amount uint))
  (begin
    (asserts! (>= (stx-get-balance tx-sender) amount) err-insufficient-funds)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set insurance-pool (+ (var-get insurance-pool) amount))
    (ok true)
  )
)

(define-public (rate-market (market-id uint) (rating uint) (comment (string-ascii 200)))
  (let (
    (market (unwrap! (get-market market-id) err-not-found))
    (current-summary (get-market-rating-summary market-id))
    (current-user-count (default-to { ratings-given: u0 } (map-get? user-rating-count { user: tx-sender })))
  )
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
    (asserts! (not (is-eq tx-sender (get creator market))) err-self-rating)
    (asserts! (is-none (get-market-rating market-id tx-sender)) err-already-rated)
    (asserts! (get resolved market) err-market-active)
    
    (map-set market-ratings
      { market-id: market-id, rater: tx-sender }
      { rating: rating, comment: comment }
    )
    
    (let (
      (new-total-ratings (+ (get total-ratings current-summary) u1))
      (new-rating-sum (+ (get rating-sum current-summary) rating))
      (new-avg-rating (/ (* new-rating-sum u100) new-total-ratings))
    )
      (map-set market-rating-summary
        { market-id: market-id }
        {
          total-ratings: new-total-ratings,
          rating-sum: new-rating-sum,
          avg-rating: new-avg-rating
        }
      )
    )
    
    (map-set user-rating-count
      { user: tx-sender }
      { ratings-given: (+ (get ratings-given current-user-count) u1) }
    )
    
    (ok true)
  )
)

(define-read-only (get-top-creators (limit uint))
  (begin
    (asserts! (and (> limit u0) (<= limit u10)) err-invalid-limit)
    (ok limit)
  )
)

(define-read-only (get-creator-stats (creator principal))
  (let (
    (rep (get-creator-reputation creator))
  )
    (ok {
      reputation: rep,
      resolution-rate: (if (> (get markets-created rep) u0)
        (/ (* (get markets-resolved rep) u100) (get markets-created rep))
        u0),
      avg-market-volume: (if (> (get markets-created rep) u0)
        (/ (get total-volume-created rep) (get markets-created rep))
        u0)
    })
  )
)

(define-read-only (is-trusted-creator (creator principal))
  (let (
    (rep (get-creator-reputation creator))
  )
    (and 
      (>= (get markets-created rep) u5)
      (>= (get reputation-score rep) u600)
      (>= (/ (* (get markets-resolved rep) u100) (get markets-created rep)) u80)
    )
  )
)