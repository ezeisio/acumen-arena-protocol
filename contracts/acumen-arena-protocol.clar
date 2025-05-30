;; Acumen Arena - Wisdom-for-Value Protocol
;;
;; This smart contract enables users to exchange wisdom tokens in a peer-to-peer environment
;; Users can offer their knowledge, propose exchanges, and build reputation through a decentralized ecosystem that values intellectual contribution and community growth.
;;
;; Wisdom tokens represent time-based knowledge sharing, where users can exchange tokens for collaborative learning experiences across various domains.

;; ==========================================
;; Primary Contract Configuration Parameters
;; ==========================================

;; Contract ownership and governance parameters
(define-constant nexus-steward tx-sender)
(define-constant err-steward-only (err u200))

;; Balance and transaction error codes
(define-constant err-wisdom-deficiency (err u201))
(define-constant err-invalid-wisdom-quantity (err u202))
(define-constant err-invalid-token-valuation (err u203))
(define-constant err-ecosystem-capacity-exceeded (err u204))
(define-constant err-forbidden-operation (err u205))

;; Governance Variables
;; ===================

;; Base valuation for wisdom tokens in microstacks
(define-data-var wisdom-token-valuation uint u10)

;; Maximum wisdom allocation per participant
(define-data-var wisdom-allocation-ceiling uint u100)

;; Platform sustainability contribution percentage (out of 100)
(define-data-var ecosystem-contribution-rate uint u10)

;; Current total wisdom tokens offered in ecosystem
(define-data-var ecosystem-wisdom-pool uint u0)

;; Maximum capacity of the ecosystem wisdom pool
(define-data-var ecosystem-capacity-threshold uint u1000)

;; ==========================================
;; Participant Data Storage
;; ==========================================

;; Tracks participant's available wisdom tokens
(define-map participant-wisdom-treasury principal uint)

;; Tracks participant's liquid token balance
(define-map participant-token-treasury principal uint)

;; Records wisdom tokens available for exchange by participant
(define-map wisdom-offerings {participant: principal} {wisdom-units: uint, token-value: uint})

;; Reputation Management System
;; ===========================

;; Individual reputation assessments
(define-map wisdom-quality-assessments {contributor: principal, assessor: principal} uint)

;; Total number of reputation assessments received
(define-map contributor-assessment-count principal uint)

;; Cumulative reputation score
(define-map contributor-assessment-sum principal uint)

;; Exchange Proposal Management
;; ===========================

;; Storage for exchange proposals between participants
(define-map wisdom-exchange-proposals 
  {proposal-identifier: uint} 
  {
    requester: principal,
    contributor: principal,
    wisdom-units: uint,
    token-value: uint,
    proposal-state: uint, ;; 0=awaiting, 1=accepted, 2=declined, 3=fulfilled
    timestamp: uint
  }
)

;; Tracks the next available proposal identifier
(define-data-var proposal-sequence-counter uint u1)

;; ==========================================
;; Private Utility Functions
;; ==========================================

;; Calculate ecosystem contribution amount
(define-private (determine-ecosystem-contribution (amount uint))
  (/ (* amount (var-get ecosystem-contribution-rate)) u100))

;; Manage ecosystem wisdom pool levels
(define-private (adjust-ecosystem-wisdom-pool (delta int))
  (let (
    (current-pool (var-get ecosystem-wisdom-pool))
    (updated-pool (if (< delta 0)
                     (if (>= current-pool (to-uint (- 0 delta)))
                         (- current-pool (to-uint (- 0 delta)))
                         u0)
                     (+ current-pool (to-uint delta))))
  )
    (asserts! (<= updated-pool (var-get ecosystem-capacity-threshold)) err-ecosystem-capacity-exceeded)
    (var-set ecosystem-wisdom-pool updated-pool)
    (ok true)))

;; ==========================================
;; Public Interface Functions
;; ==========================================

;; System Configuration Functions
;; =============================

;; Update system parameters (restricted to nexus steward)
;; @param new-token-value: updated base token valuation
;; @param new-allocation-ceiling: updated maximum wisdom allocation per participant
;; @param new-contribution-rate: updated ecosystem contribution percentage
;; @param new-capacity-threshold: updated ecosystem capacity threshold
(define-public (reconfigure-ecosystem-parameters 
                (new-token-value uint) 
                (new-allocation-ceiling uint) 
                (new-contribution-rate uint) 
                (new-capacity-threshold uint))
  (begin
    (asserts! (is-eq tx-sender nexus-steward) err-steward-only)
    (asserts! (<= new-contribution-rate u100) (err u212)) ;; Ensure contribution rate doesn't exceed 100%
    (asserts! (> new-token-value u0) err-invalid-token-valuation) ;; Ensure token value is positive
    (asserts! (> new-allocation-ceiling u0) (err u213)) ;; Ensure allocation ceiling is positive
    (asserts! (>= new-capacity-threshold (var-get ecosystem-wisdom-pool)) (err u214)) ;; Ensure capacity threshold accommodates current pool

    (var-set wisdom-token-valuation new-token-value)
    (var-set wisdom-allocation-ceiling new-allocation-ceiling)
    (var-set ecosystem-contribution-rate new-contribution-rate)
    (var-set ecosystem-capacity-threshold new-capacity-threshold)

    (ok true)))

;; Wisdom Token Management Functions
;; ================================

;; Register new wisdom units to participant account
;; @param units: the number of wisdom units to register
(define-public (contribute-wisdom-units (units uint))
  (let (
    (current-treasury (default-to u0 (map-get? participant-wisdom-treasury tx-sender)))
    (max-allocation (var-get wisdom-allocation-ceiling))
    (updated-treasury (+ current-treasury units))
  )
    (asserts! (> units u0) err-invalid-wisdom-quantity) ;; Ensure units are positive
    (asserts! (<= updated-treasury max-allocation) (err u211)) ;; Check against allocation ceiling
    (map-set participant-wisdom-treasury tx-sender updated-treasury)
    (ok updated-treasury)))

;; Publish wisdom units for exchange
;; @param units: number of wisdom units to offer
;; @param token-rate: requested token rate per wisdom unit
(define-public (publish-wisdom-offering (units uint) (token-rate uint))
  (let (
    (treasury-balance (default-to u0 (map-get? participant-wisdom-treasury tx-sender)))
    (current-offering (get wisdom-units (default-to {wisdom-units: u0, token-value: u0} 
                                        (map-get? wisdom-offerings {participant: tx-sender}))))
    (combined-offering (+ units current-offering))
  )
    (asserts! (> units u0) err-invalid-wisdom-quantity) ;; Ensure units are positive
    (asserts! (> token-rate u0) err-invalid-token-valuation) ;; Ensure token rate is positive
    (asserts! (>= treasury-balance combined-offering) err-wisdom-deficiency)
    (try! (adjust-ecosystem-wisdom-pool (to-int units)))
    (map-set wisdom-offerings {participant: tx-sender} 
             {wisdom-units: combined-offering, token-value: token-rate})
    (ok true)))

;; Remove wisdom units from exchange offerings
;; @param units: number of wisdom units to withdraw from offering
(define-public (withdraw-wisdom-offering (units uint))
  (let (
    (current-offering (get wisdom-units (default-to {wisdom-units: u0, token-value: u0} 
                                        (map-get? wisdom-offerings {participant: tx-sender}))))
  )
    (asserts! (>= current-offering units) err-wisdom-deficiency)
    (try! (adjust-ecosystem-wisdom-pool (to-int (- units))))
    (map-set wisdom-offerings {participant: tx-sender} 
             {wisdom-units: (- current-offering units), 
              token-value: (get token-value (default-to {wisdom-units: u0, token-value: u0} 
                                            (map-get? wisdom-offerings {participant: tx-sender})))})
    (ok true)))

;; Token Management Functions
;; =========================

;; Deposit tokens into participant's balance
;; @param amount: amount of tokens to deposit (in ustx)
(define-public (deposit-tokens (amount uint))
  (let (
    (participant tx-sender)
    (current-balance (default-to u0 (map-get? participant-token-treasury participant)))
    (new-balance (+ current-balance amount))
  )
    (asserts! (> amount u0) (err u210)) ;; Ensure deposit amount is positive
    (try! (stx-transfer? amount participant (as-contract tx-sender)))
    (map-set participant-token-treasury participant new-balance)
    (ok new-balance)))

;; Exchange Functions
;; ================

;; Direct exchange of wisdom units for tokens
;; @param contributor: principal of the wisdom contributor
;; @param units: number of wisdom units requested
(define-public (acquire-wisdom (contributor principal) (units uint))
  (let (
    (offering-data (default-to {wisdom-units: u0, token-value: u0} 
                   (map-get? wisdom-offerings {participant: contributor})))
    (exchange-cost (* units (get token-value offering-data)))
    (platform-fee (determine-ecosystem-contribution exchange-cost))
    (total-cost (+ exchange-cost platform-fee))
    (contributor-treasury (default-to u0 (map-get? participant-wisdom-treasury contributor)))
    (requester-balance (default-to u0 (map-get? participant-token-treasury tx-sender)))
    (contributor-balance (default-to u0 (map-get? participant-token-treasury contributor)))
  )
    (asserts! (not (is-eq tx-sender contributor)) err-forbidden-operation)
    (asserts! (> units u0) err-invalid-wisdom-quantity)
    (asserts! (>= (get wisdom-units offering-data) units) err-wisdom-deficiency)
    (asserts! (>= contributor-treasury units) err-wisdom-deficiency)
    (asserts! (>= requester-balance total-cost) err-wisdom-deficiency)

    ;; Update contributor's wisdom treasury and offering
    (map-set participant-wisdom-treasury contributor (- contributor-treasury units))
    (map-set wisdom-offerings {participant: contributor} 
             {wisdom-units: (- (get wisdom-units offering-data) units), 
              token-value: (get token-value offering-data)})

    ;; Update requester's token and wisdom balance
    (map-set participant-token-treasury tx-sender (- requester-balance total-cost))
    (map-set participant-wisdom-treasury tx-sender (+ (default-to u0 
                                                    (map-get? participant-wisdom-treasury tx-sender)) units))

    ;; Update contributor's token balance
    (map-set participant-token-treasury contributor (+ contributor-balance exchange-cost))

    ;; Update steward's balance with the platform fee
    (map-set participant-token-treasury nexus-steward 
            (+ (default-to u0 (map-get? participant-token-treasury nexus-steward)) platform-fee))

    (ok true)))

;; Proposal Management Functions
;; ===========================

;; Create an exchange proposal
;; @param contributor: principal of the wisdom contributor
;; @param units: number of wisdom units requested
;; @param proposed-rate: token rate proposed for the exchange
(define-public (submit-exchange-proposal (contributor principal) (units uint) (proposed-rate uint))
  (let (
    (requester tx-sender)
    (proposal-id (var-get proposal-sequence-counter))
    (offering-data (default-to {wisdom-units: u0, token-value: u0} 
                   (map-get? wisdom-offerings {participant: contributor})))
    (exchange-cost (* units proposed-rate))
    (platform-fee (determine-ecosystem-contribution exchange-cost))
    (total-cost (+ exchange-cost platform-fee))
    (requester-balance (default-to u0 (map-get? participant-token-treasury requester)))
  )
    (asserts! (not (is-eq requester contributor)) err-forbidden-operation)
    (asserts! (> units u0) err-invalid-wisdom-quantity)
    (asserts! (>= (get wisdom-units offering-data) units) err-wisdom-deficiency)
    (asserts! (> proposed-rate u0) err-invalid-token-valuation)
    (asserts! (>= requester-balance total-cost) err-wisdom-deficiency)

    ;; Create proposal record
    (map-set wisdom-exchange-proposals
      {proposal-identifier: proposal-id}
      {
        requester: requester,
        contributor: contributor,
        wisdom-units: units,
        token-value: proposed-rate,
        proposal-state: u0, ;; awaiting response
        timestamp: block-height
      }
    )

    ;; Reserve tokens for the proposal
    (map-set participant-token-treasury requester (- requester-balance total-cost))

    ;; Increment proposal identifier
    (var-set proposal-sequence-counter (+ proposal-id u1))

    (ok proposal-id)))

;; Reputation Functions
;; ==================

;; Submit quality assessment for a contributor
;; @param contributor: the principal of the contributor being assessed
;; @param quality-score: the score (1-5) given to the contributor
(define-public (assess-contributor-quality (contributor principal) (quality-score uint))
  (let (
    (assessor tx-sender)
    (current-assessment (default-to u0 
                        (map-get? wisdom-quality-assessments 
                                 {contributor: contributor, assessor: assessor})))
    (assessment-count (default-to u0 (map-get? contributor-assessment-count contributor)))
    (assessment-sum (default-to u0 (map-get? contributor-assessment-sum contributor)))
    (updated-count (if (is-eq current-assessment u0) (+ assessment-count u1) assessment-count))
    (updated-sum (+ (- assessment-sum current-assessment) quality-score))
  )
    (asserts! (not (is-eq assessor contributor)) err-forbidden-operation)
    (asserts! (and (>= quality-score u1) (<= quality-score u5)) (err u215))

    ;; Update assessment records
    (map-set wisdom-quality-assessments {contributor: contributor, assessor: assessor} quality-score)
    (map-set contributor-assessment-count contributor updated-count)
    (map-set contributor-assessment-sum contributor updated-sum)

    (ok true)))

;; ==========================================
;; Getter Functions (Read-Only)
;; ==========================================

;; Retrieve participant's wisdom treasury balance
(define-read-only (get-wisdom-treasury-balance (participant principal))
  (default-to u0 (map-get? participant-wisdom-treasury participant)))

;; Retrieve participant's token treasury balance
(define-read-only (get-token-treasury-balance (participant principal))
  (default-to u0 (map-get? participant-token-treasury participant)))

;; Retrieve participant's wisdom offering details
(define-read-only (get-wisdom-offering (participant principal))
  (default-to {wisdom-units: u0, token-value: u0} 
             (map-get? wisdom-offerings {participant: participant})))

;; Calculate participant's average quality assessment
(define-read-only (get-contributor-rating (participant principal))
  (let (
    (assessment-count (default-to u0 (map-get? contributor-assessment-count participant)))
    (assessment-sum (default-to u0 (map-get? contributor-assessment-sum participant)))
  )
    (if (> assessment-count u0)
        (/ assessment-sum assessment-count)
        u0)))

;; Retrieve proposal details
(define-read-only (get-exchange-proposal (proposal-id uint))
  (map-get? wisdom-exchange-proposals {proposal-identifier: proposal-id}))

;; Retrieve current ecosystem parameters
(define-read-only (get-system-parameters)
  {
    token-valuation: (var-get wisdom-token-valuation),
    allocation-ceiling: (var-get wisdom-allocation-ceiling),
    contribution-rate: (var-get ecosystem-contribution-rate),
    ecosystem-pool: (var-get ecosystem-wisdom-pool),
    capacity-threshold: (var-get ecosystem-capacity-threshold)
  })

;; ==========================================
;; Future Expansion Functions
;; ==========================================

;; These function stubs represent planned future functionality

;; Accept an exchange proposal (placeholder)
(define-public (accept-exchange-proposal (proposal-id uint))
  (begin
    ;; Implementation pending
    (ok true)))

;; Reject an exchange proposal (placeholder)
(define-public (reject-exchange-proposal (proposal-id uint))
  (begin
    ;; Implementation pending
    (ok true)))

;; Mark an exchange as completed (placeholder)
(define-public (complete-exchange (proposal-id uint))
  (begin
    ;; Implementation pending
    (ok true)))

;; Withdraw tokens from treasury (placeholder)
(define-public (withdraw-tokens (amount uint))
  (begin
    ;; Implementation pending
    (ok true)))

;; Implement wisdom token transfer between participants (placeholder)
(define-public (transfer-wisdom (recipient principal) (units uint))
  (begin
    ;; Implementation pending
    (ok true)))

;; ==========================================
;; Emergency Functions
;; ==========================================

;; Emergency pause of ecosystem (restricted to steward)
(define-public (emergency-pause)
  (begin
    (asserts! (is-eq tx-sender nexus-steward) err-steward-only)
    ;; Implementation pending
    (ok true)))

;; Resume ecosystem operations (restricted to steward)
(define-public (resume-operations)
  (begin
    (asserts! (is-eq tx-sender nexus-steward) err-steward-only)
    ;; Implementation pending
    (ok true)))

