;; Disaster Relief Fund Smart Contract
;; Emergency response funding with transparent aid distribution

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-response-ended (err u103))
(define-constant err-fund-active (err u104))
(define-constant err-relief-incomplete (err u105))
(define-constant err-relief-complete (err u106))
(define-constant err-no-donation (err u107))
(define-constant err-insufficient-balance (err u108))
(define-constant err-response-active (err u109))

;; Data Variables
(define-data-var next-relief-id uint u1)

;; Relief Status
(define-constant status-emergency-active u1)
(define-constant status-relief-complete u2)
(define-constant status-emergency-ended u3)

;; Data Maps
(define-map relief-campaigns 
  uint 
  {
    coordinator: principal,
    disaster-type: (string-ascii 100),
    affected-area: (string-ascii 500),
    aid-target: uint,
    response-deadline: uint,
    total-donations: uint,
    status: uint,
    emergency-declared: uint
  })

(define-map donor-contributions 
  { relief-id: uint, donor: principal } 
  uint)

(define-map campaign-donors
  uint
  (list 200 principal))

;; Private Functions
(define-private (is-relief-coordinator (relief-id uint) (user principal))
  (match (map-get? relief-campaigns relief-id)
    campaign (is-eq (get coordinator campaign) user)
    false))

(define-private (get-current-height)
  block-height)

;; Read-only Functions
(define-read-only (get-relief-campaign (relief-id uint))
  (map-get? relief-campaigns relief-id))

(define-read-only (get-donor-contribution (relief-id uint) (donor principal))
  (default-to u0 (map-get? donor-contributions { relief-id: relief-id, donor: donor })))

(define-read-only (get-campaign-donors (relief-id uint))
  (default-to (list) (map-get? campaign-donors relief-id)))

(define-read-only (get-next-relief-id)
  (var-get next-relief-id))

;; Public Functions

;; Declare emergency relief campaign
(define-public (declare-emergency (disaster-type (string-ascii 100)) (affected-area (string-ascii 500)) (aid-target uint) (response-window uint))
  (let ((relief-id (var-get next-relief-id))
        (response-deadline (+ (get-current-height) response-window)))
    (map-set relief-campaigns relief-id {
      coordinator: tx-sender,
      disaster-type: disaster-type,
      affected-area: affected-area,
      aid-target: aid-target,
      response-deadline: response-deadline,
      total-donations: u0,
      status: status-emergency-active,
      emergency-declared: (get-current-height)
    })
    (var-set next-relief-id (+ relief-id u1))
    (ok relief-id)))

;; Donate to disaster relief
(define-public (donate-relief (relief-id uint) (amount uint))
  (match (map-get? relief-campaigns relief-id)
    campaign 
    (if (> (get-current-height) (get response-deadline campaign))
      err-response-ended
      (if (not (is-eq (get status campaign) status-emergency-active))
        err-fund-active
        (begin
          (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
          
          (let ((current-donation (get-donor-contribution relief-id tx-sender)))
            (map-set donor-contributions 
              { relief-id: relief-id, donor: tx-sender }
              (+ current-donation amount))
            
            (if (is-eq current-donation u0)
              (let ((donors (get-campaign-donors relief-id)))
                (map-set campaign-donors relief-id 
                  (unwrap-panic (as-max-len? (append donors tx-sender) u200))))
              true)
            
            (map-set relief-campaigns relief-id 
              (merge campaign { total-donations: (+ (get total-donations campaign) amount) }))
            
            (ok true)))))
    err-not-found))

;; Finalize relief campaign status
(define-public (finalize-relief-campaign (relief-id uint))
  (match (map-get? relief-campaigns relief-id)
    campaign
    (if (<= (get-current-height) (get response-deadline campaign))
      err-response-active
      (if (not (is-eq (get status campaign) status-emergency-active))
        err-fund-active
        (let ((new-status (if (>= (get total-donations campaign) (get aid-target campaign))
                           status-relief-complete
                           status-emergency-ended)))
          (map-set relief-campaigns relief-id 
            (merge campaign { status: new-status }))
          (ok new-status))))
    err-not-found))

;; Distribute relief funds
(define-public (distribute-relief-funds (relief-id uint))
  (match (map-get? relief-campaigns relief-id)
    campaign
    (if (not (is-relief-coordinator relief-id tx-sender))
      err-unauthorized
      (if (not (is-eq (get status campaign) status-relief-complete))
        err-relief-complete
        (begin
          (try! (as-contract (stx-transfer? (get total-donations campaign) tx-sender (get coordinator campaign))))
          (ok true))))
    err-not-found))

;; Refund donations for incomplete relief
(define-public (claim-donation-refund (relief-id uint))
  (match (map-get? relief-campaigns relief-id)
    campaign
    (if (not (is-eq (get status campaign) status-emergency-ended))
      err-relief-incomplete
      (let ((donation-amount (get-donor-contribution relief-id tx-sender)))
        (if (is-eq donation-amount u0)
          err-no-donation
          (begin
            (map-delete donor-contributions { relief-id: relief-id, donor: tx-sender })
            (try! (as-contract (stx-transfer? donation-amount tx-sender tx-sender)))
            (ok donation-amount)))))
    err-not-found))