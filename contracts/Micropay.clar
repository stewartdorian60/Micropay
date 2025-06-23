
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

(define-data-var loan-counter uint u0)
(define-data-var platform-fee-rate uint u250)

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