
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_LOAN_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_FUNDS (err u102))
(define-constant ERR_LOAN_ALREADY_FUNDED (err u103))
(define-constant ERR_LOAN_NOT_ACTIVE (err u104))
(define-constant ERR_PAYMENT_OVERDUE (err u105))
(define-constant ERR_INVALID_AMOUNT (err u106))
(define-constant ERR_LOAN_COMPLETED (err u107))
(define-constant ERR_FUNDING_PERIOD_ENDED (err u108))
(define-constant ERR_INSURANCE_NOT_FOUND (err u109))
(define-constant ERR_INSURANCE_ALREADY_EXISTS (err u110))
(define-constant ERR_INSURANCE_EXPIRED (err u111))
(define-constant ERR_CLAIM_ALREADY_PROCESSED (err u112))
(define-constant ERR_INSUFFICIENT_INSURANCE_POOL (err u113))
(define-constant ERR_INVALID_COVERAGE_PERCENTAGE (err u114))
(define-constant ERR_LOAN_NOT_DEFAULTED (err u115))
(define-constant ERR_AUCTION_NOT_FOUND (err u116))
(define-constant ERR_AUCTION_ALREADY_EXISTS (err u117))
(define-constant ERR_AUCTION_ENDED (err u118))
(define-constant ERR_AUCTION_NOT_ENDED (err u119))
(define-constant ERR_BID_TOO_LOW (err u120))
(define-constant ERR_INVALID_BID_AMOUNT (err u121))
(define-constant ERR_BIDDER_IS_BORROWER (err u122))
(define-constant ERR_AUCTION_NOT_ACTIVE (err u123))

(define-data-var loan-counter uint u0)
(define-data-var platform-fee-rate uint u250)
(define-data-var insurance-policy-counter uint u0)
(define-data-var insurance-pool-balance uint u0)
(define-data-var base-insurance-rate uint u500)
(define-data-var auction-counter uint u0)
(define-data-var default-auction-duration uint u1440)

(define-map loans
  uint
  {
    borrower: principal,
    amount: uint,
    interest-rate: uint,
    duration-blocks: uint,
    funded-amount: uint,
    repaid-amount: uint,
    created-at: uint,
    funded-at: (optional uint),
    status: (string-ascii 20),
    description: (string-ascii 500)
  }
)

(define-map loan-funders
  { loan-id: uint, funder: principal }
  { amount: uint, repaid: uint }
)

(define-map user-stats
  principal
  {
    loans-requested: uint,
    loans-funded: uint,
    total-borrowed: uint,
    total-lent: uint,
    reputation-score: uint
  }
)

(define-map repayment-schedule
  { loan-id: uint, payment-number: uint }
  {
    due-block: uint,
    amount: uint,
    paid: bool
  }
)

(define-map insurance-policies
  uint
  {
    policy-holder: principal,
    loan-id: uint,
    coverage-amount: uint,
    premium-paid: uint,
    coverage-percentage: uint,
    created-at: uint,
    expires-at: uint,
    is-active: bool,
    claim-processed: bool
  }
)

(define-map loan-insurance-mapping
  uint
  { policy-id: uint, has-insurance: bool }
)

(define-map insurance-claims
  uint
  {
    policy-id: uint,
    claim-amount: uint,
    claimed-at: uint,
    processed: bool,
    approved: bool
  }
)

(define-map loan-auctions
  uint
  {
    loan-id: uint,
    borrower: principal,
    loan-amount: uint,
    max-interest-rate: uint,
    duration-blocks: uint,
    auction-start: uint,
    auction-end: uint,
    status: (string-ascii 20),
    winning-bid-id: (optional uint),
    total-bids: uint
  }
)

(define-map auction-bids
  uint
  {
    auction-id: uint,
    bidder: principal,
    interest-rate: uint,
    funding-amount: uint,
    bid-timestamp: uint,
    is-winning: bool,
    is-withdrawn: bool
  }
)

(define-map auction-bid-mapping
  { auction-id: uint, bidder: principal }
  { bid-id: uint, has-bid: bool }
)

(define-public (make-repayment (loan-id uint) (payment-amount uint))
  (let
    (
      (loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
      (total-with-interest (calculate-total-repayment (get amount loan) (get interest-rate loan)))
      (current-repaid (get repaid-amount loan))
      (remaining-debt (- total-with-interest current-repaid))
      (actual-payment (if (<= payment-amount remaining-debt) payment-amount remaining-debt))
      (new-repaid-amount (+ current-repaid actual-payment))
    )
    (asserts! (is-eq (get borrower loan) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status loan) "active") ERR_LOAN_NOT_ACTIVE)
    (asserts! (> payment-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (< current-repaid total-with-interest) ERR_LOAN_COMPLETED)
    
    (try! (stx-transfer? actual-payment tx-sender (as-contract tx-sender)))
    
    (map-set loans loan-id
      (merge loan { 
        repaid-amount: new-repaid-amount,
        status: (if (>= new-repaid-amount total-with-interest) "completed" "active")
      })
    )
    

    
    (ok actual-payment)
  )
)

(define-public (claim-overdue-collateral (loan-id uint))
  (let
    (
      (loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
      (funded-at-block (unwrap! (get funded-at loan) ERR_LOAN_NOT_ACTIVE))
      (due-block (+ funded-at-block (get duration-blocks loan)))
    )
    (asserts! (is-eq (get status loan) "active") ERR_LOAN_NOT_ACTIVE)
    (asserts! (> stacks-block-height due-block) ERR_PAYMENT_OVERDUE)
    
    (map-set loans loan-id (merge loan { status: "defaulted" }))
    (ok true)
  )
)

(define-private (create-repayment-schedule (loan-id uint))
  (let
    (
      (loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
      (total-amount (calculate-total-repayment (get amount loan) (get interest-rate loan)))
      (duration (get duration-blocks loan))
      (funded-at (unwrap! (get funded-at loan) ERR_LOAN_NOT_ACTIVE))
      (payment-amount (/ total-amount u4))
    )
    (map-set repayment-schedule { loan-id: loan-id, payment-number: u1 }
      { due-block: (+ funded-at (/ duration u4)), amount: payment-amount, paid: false })
    (map-set repayment-schedule { loan-id: loan-id, payment-number: u2 }
      { due-block: (+ funded-at (/ duration u2)), amount: payment-amount, paid: false })
    (map-set repayment-schedule { loan-id: loan-id, payment-number: u3 }
      { due-block: (+ funded-at (* duration u3) (/ u4)), amount: payment-amount, paid: false })
    (map-set repayment-schedule { loan-id: loan-id, payment-number: u4 }
      { due-block: (+ funded-at duration), amount: (- total-amount (* payment-amount u3)), paid: false })
    (ok true)
  )
)

(define-private (distribute-repayments (loan-id uint) (total-repaid uint))
  (let
    (
      (loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
      (platform-fee (/ (* total-repaid (var-get platform-fee-rate)) u10000))
      (distributable-amount (- total-repaid platform-fee))
    )
    (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT_OWNER)))
    (ok distributable-amount)
  )
)

(define-private (calculate-total-repayment (principal-amount uint) (interest-rate uint))
  (+ principal-amount (/ (* principal-amount interest-rate) u10000))
)

(define-private (update-user-stats (user principal) (loans-req uint) (loans-fund uint) (borrowed uint) (lent uint))
  (let
    (
      (current-stats (default-to 
        { loans-requested: u0, loans-funded: u0, total-borrowed: u0, total-lent: u0, reputation-score: u100 }
        (map-get? user-stats user)
      ))
    )
    (map-set user-stats user
      {
        loans-requested: (+ (get loans-requested current-stats) loans-req),
        loans-funded: (+ (get loans-funded current-stats) loans-fund),
        total-borrowed: (+ (get total-borrowed current-stats) borrowed),
        total-lent: (+ (get total-lent current-stats) lent),
        reputation-score: (get reputation-score current-stats)
      }
    )
    (ok true)
  )
)

(define-read-only (get-loan (loan-id uint))
  (map-get? loans loan-id)
)

(define-read-only (get-loan-funder (loan-id uint) (funder principal))
  (map-get? loan-funders { loan-id: loan-id, funder: funder })
)

(define-read-only (get-user-stats (user principal))
  (map-get? user-stats user)
)

(define-read-only (get-repayment-info (loan-id uint) (payment-number uint))
  (map-get? repayment-schedule { loan-id: loan-id, payment-number: payment-number })
)

(define-read-only (get-total-loans)
  (var-get loan-counter)
)

(define-read-only (calculate-loan-total (loan-id uint))
  (match (map-get? loans loan-id)
    loan (ok (calculate-total-repayment (get amount loan) (get interest-rate loan)))
    ERR_LOAN_NOT_FOUND
  )
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

(define-public (set-platform-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-rate u1000) ERR_INVALID_AMOUNT)
    (var-set platform-fee-rate new-rate)
    (ok true)
  )
)

(define-public (purchase-loan-insurance (loan-id uint) (coverage-percentage uint))
  (let
    (
      (loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
      (policy-id (+ (var-get insurance-policy-counter) u1))
      (loan-amount (get amount loan))
      (coverage-amount (/ (* loan-amount coverage-percentage) u100))
      (premium-amount (calculate-insurance-premium loan-amount coverage-percentage (get interest-rate loan)))
      (expiry-block (+ stacks-block-height u144000))
    )
    (asserts! (> coverage-percentage u0) ERR_INVALID_COVERAGE_PERCENTAGE)
    (asserts! (<= coverage-percentage u100) ERR_INVALID_COVERAGE_PERCENTAGE)
    (asserts! (is-none (map-get? loan-insurance-mapping loan-id)) ERR_INSURANCE_ALREADY_EXISTS)
    (asserts! (is-eq (get status loan) "pending") ERR_LOAN_NOT_FOUND)
    
    (try! (stx-transfer? premium-amount tx-sender (as-contract tx-sender)))
    (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) premium-amount))
    
    (map-set insurance-policies policy-id
      {
        policy-holder: tx-sender,
        loan-id: loan-id,
        coverage-amount: coverage-amount,
        premium-paid: premium-amount,
        coverage-percentage: coverage-percentage,
        created-at: stacks-block-height,
        expires-at: expiry-block,
        is-active: true,
        claim-processed: false
      }
    )
    
    (map-set loan-insurance-mapping loan-id
      { policy-id: policy-id, has-insurance: true }
    )
    
    (var-set insurance-policy-counter policy-id)
    (ok policy-id)
  )
)

(define-public (file-insurance-claim (policy-id uint))
  (let
    (
      (policy (unwrap! (map-get? insurance-policies policy-id) ERR_INSURANCE_NOT_FOUND))
      (loan-id (get loan-id policy))
      (loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
      (coverage-amount (get coverage-amount policy))
    )
    (asserts! (is-eq (get policy-holder policy) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active policy) ERR_INSURANCE_EXPIRED)
    (asserts! (not (get claim-processed policy)) ERR_CLAIM_ALREADY_PROCESSED)
    (asserts! (is-eq (get status loan) "defaulted") ERR_LOAN_NOT_DEFAULTED)
    (asserts! (>= (var-get insurance-pool-balance) coverage-amount) ERR_INSUFFICIENT_INSURANCE_POOL)
    
    (try! (as-contract (stx-transfer? coverage-amount tx-sender (get policy-holder policy))))
    (var-set insurance-pool-balance (- (var-get insurance-pool-balance) coverage-amount))
    
    (map-set insurance-policies policy-id
      (merge policy { claim-processed: true, is-active: false })
    )
    
    (ok coverage-amount)
  )
)

(define-public (contribute-to-insurance-pool (amount uint))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) amount))
    (ok true)
  )
)

(define-public (withdraw-from-insurance-pool (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (var-get insurance-pool-balance) amount) ERR_INSUFFICIENT_INSURANCE_POOL)
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
    (var-set insurance-pool-balance (- (var-get insurance-pool-balance) amount))
    (ok true)
  )
)

(define-public (update-insurance-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-rate u2000) ERR_INVALID_AMOUNT)
    (var-set base-insurance-rate new-rate)
    (ok true)
  )
)

(define-public (cancel-insurance-policy (policy-id uint))
  (let
    (
      (policy (unwrap! (map-get? insurance-policies policy-id) ERR_INSURANCE_NOT_FOUND))
      (loan-id (get loan-id policy))
      (refund-amount (/ (get premium-paid policy) u2))
    )
    (asserts! (is-eq (get policy-holder policy) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active policy) ERR_INSURANCE_EXPIRED)
    (asserts! (not (get claim-processed policy)) ERR_CLAIM_ALREADY_PROCESSED)
    (asserts! (>= (var-get insurance-pool-balance) refund-amount) ERR_INSUFFICIENT_INSURANCE_POOL)
    
    (try! (as-contract (stx-transfer? refund-amount tx-sender (get policy-holder policy))))
    (var-set insurance-pool-balance (- (var-get insurance-pool-balance) refund-amount))
    
    (map-set insurance-policies policy-id
      (merge policy { is-active: false })
    )
    
    (map-set loan-insurance-mapping loan-id
      { policy-id: policy-id, has-insurance: false }
    )
    
    (ok refund-amount)
  )
)

(define-private (calculate-insurance-premium (loan-amount uint) (coverage-percentage uint) (interest-rate uint))
  (let
    (
      (base-premium (/ (* loan-amount (var-get base-insurance-rate)) u10000))
      (coverage-multiplier (/ coverage-percentage u20))
      (risk-multiplier (/ interest-rate u1000))
    )
    (+ base-premium (* base-premium (+ coverage-multiplier risk-multiplier)))
  )
)

(define-read-only (get-insurance-policy (policy-id uint))
  (map-get? insurance-policies policy-id)
)

(define-read-only (get-loan-insurance (loan-id uint))
  (map-get? loan-insurance-mapping loan-id)
)

(define-read-only (get-insurance-pool-balance)
  (var-get insurance-pool-balance)
)

(define-read-only (get-insurance-premium-quote (loan-amount uint) (coverage-percentage uint) (interest-rate uint))
  (calculate-insurance-premium loan-amount coverage-percentage interest-rate)
)

(define-read-only (get-total-insurance-policies)
  (var-get insurance-policy-counter)
)

(define-read-only (get-base-insurance-rate)
  (var-get base-insurance-rate)
)

(define-public (create-loan-auction (loan-id uint) (max-interest-rate uint) (auction-duration uint))
  (let
    (
      (loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
      (auction-id (+ (var-get auction-counter) u1))
      (auction-start stacks-block-height)
      (auction-end (+ stacks-block-height auction-duration))
    )
    (asserts! (is-eq (get borrower loan) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status loan) "pending") ERR_LOAN_NOT_FOUND)
    (asserts! (> max-interest-rate u0) ERR_INVALID_AMOUNT)
    (asserts! (> auction-duration u0) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? loan-auctions loan-id)) ERR_AUCTION_ALREADY_EXISTS)
    
    (map-set loan-auctions auction-id
      {
        loan-id: loan-id,
        borrower: tx-sender,
        loan-amount: (get amount loan),
        max-interest-rate: max-interest-rate,
        duration-blocks: (get duration-blocks loan),
        auction-start: auction-start,
        auction-end: auction-end,
        status: "active",
        winning-bid-id: none,
        total-bids: u0
      }
    )
    
    (var-set auction-counter auction-id)
    (ok auction-id)
  )
)

(define-public (place-auction-bid (auction-id uint) (interest-rate uint) (funding-amount uint))
  (let
    (
      (auction (unwrap! (map-get? loan-auctions auction-id) ERR_AUCTION_NOT_FOUND))
      (bid-id (+ (var-get auction-counter) (get total-bids auction) u1))
      (loan-id (get loan-id auction))
      (loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
    )
    (asserts! (not (is-eq (get borrower auction) tx-sender)) ERR_BIDDER_IS_BORROWER)
    (asserts! (is-eq (get status auction) "active") ERR_AUCTION_NOT_ACTIVE)
    (asserts! (<= stacks-block-height (get auction-end auction)) ERR_AUCTION_ENDED)
    (asserts! (<= interest-rate (get max-interest-rate auction)) ERR_BID_TOO_LOW)
    (asserts! (>= funding-amount (get loan-amount auction)) ERR_INVALID_BID_AMOUNT)
    (asserts! (> interest-rate u0) ERR_INVALID_AMOUNT)
    
    (try! (stx-transfer? funding-amount tx-sender (as-contract tx-sender)))
    
    (map-set auction-bids bid-id
      {
        auction-id: auction-id,
        bidder: tx-sender,
        interest-rate: interest-rate,
        funding-amount: funding-amount,
        bid-timestamp: stacks-block-height,
        is-winning: false,
        is-withdrawn: false
      }
    )
    
    (map-set auction-bid-mapping { auction-id: auction-id, bidder: tx-sender }
      { bid-id: bid-id, has-bid: true }
    )
    
    (map-set loan-auctions auction-id
      (merge auction { total-bids: (+ (get total-bids auction) u1) })
    )
    
    (ok bid-id)
  )
)

(define-public (finalize-auction (auction-id uint))
  (let
    (
      (auction (unwrap! (map-get? loan-auctions auction-id) ERR_AUCTION_NOT_FOUND))
      (loan-id (get loan-id auction))
      (loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
      (winning-bid-result (find-winning-bid auction-id))
    )
    (asserts! (is-eq (get borrower auction) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status auction) "active") ERR_AUCTION_NOT_ACTIVE)
    (asserts! (> stacks-block-height (get auction-end auction)) ERR_AUCTION_NOT_ENDED)
    
    (match winning-bid-result
      winning-bid-id
      (let
        (
          (winning-bid (unwrap! (map-get? auction-bids winning-bid-id) ERR_AUCTION_NOT_FOUND))
          (winning-rate (get interest-rate winning-bid))
          (funding-amount (get funding-amount winning-bid))
        )
        (map-set auction-bids winning-bid-id
          (merge winning-bid { is-winning: true })
        )
        
        (map-set loan-auctions auction-id
          (merge auction { 
            status: "completed",
            winning-bid-id: (some winning-bid-id)
          })
        )
        
        (map-set loans loan-id
          (merge loan {
            interest-rate: winning-rate,
            funded-amount: funding-amount,
            funded-at: (some stacks-block-height),
            status: "active"
          })
        )
        
        (try! (as-contract (stx-transfer? funding-amount tx-sender (get borrower auction))))
        (ok winning-bid-id)
      )
      (begin
        (map-set loan-auctions auction-id
          (merge auction { status: "failed" })
        )
        (ok u0)
      )
    )
  )
)

(define-public (withdraw-bid (auction-id uint))
  (let
    (
      (auction (unwrap! (map-get? loan-auctions auction-id) ERR_AUCTION_NOT_FOUND))
      (bid-mapping (unwrap! (map-get? auction-bid-mapping { auction-id: auction-id, bidder: tx-sender }) ERR_AUCTION_NOT_FOUND))
      (bid-id (get bid-id bid-mapping))
      (bid (unwrap! (map-get? auction-bids bid-id) ERR_AUCTION_NOT_FOUND))
    )
    (asserts! (is-eq (get bidder bid) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (not (get is-withdrawn bid)) ERR_INVALID_AMOUNT)
    (asserts! (> stacks-block-height (get auction-end auction)) ERR_AUCTION_NOT_ENDED)
    (asserts! (not (get is-winning bid)) ERR_INVALID_AMOUNT)
    
    (try! (as-contract (stx-transfer? (get funding-amount bid) tx-sender (get bidder bid))))
    
    (map-set auction-bids bid-id
      (merge bid { is-withdrawn: true })
    )
    
    (ok true)
  )
)

(define-public (cancel-auction (auction-id uint))
  (let
    (
      (auction (unwrap! (map-get? loan-auctions auction-id) ERR_AUCTION_NOT_FOUND))
    )
    (asserts! (is-eq (get borrower auction) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status auction) "active") ERR_AUCTION_NOT_ACTIVE)
    (asserts! (<= stacks-block-height (get auction-end auction)) ERR_AUCTION_ENDED)
    
    (map-set loan-auctions auction-id
      (merge auction { status: "cancelled" })
    )
    (ok true)
  )
)

(define-private (refund-single-bid (bid-id uint))
  (let
    (
      (bid (unwrap! (map-get? auction-bids bid-id) (ok false)))
    )
    (if (not (get is-withdrawn bid))
      (begin
        (try! (as-contract (stx-transfer? (get funding-amount bid) tx-sender (get bidder bid))))
        (map-set auction-bids bid-id (merge bid { is-withdrawn: true }))
        (ok true)
      )
      (ok false)
    )
  )
)

(define-private (find-winning-bid (auction-id uint))
  (let
    (
      (auction (unwrap! (map-get? loan-auctions auction-id) (some u0)))
      (total-bids (get total-bids auction))
    )
    (if (> total-bids u0)
      (some u1)
      none
    )
  )
)

(define-read-only (get-auction (auction-id uint))
  (map-get? loan-auctions auction-id)
)

(define-read-only (get-auction-bid (bid-id uint))
  (map-get? auction-bids bid-id)
)

(define-read-only (get-bidder-info (auction-id uint) (bidder principal))
  (map-get? auction-bid-mapping { auction-id: auction-id, bidder: bidder })
)

(define-read-only (get-total-auctions)
  (var-get auction-counter)
)

(define-read-only (get-auction-duration)
  (var-get default-auction-duration)
)



