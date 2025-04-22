;; Blockchain Validator Network Reputation System
;; v1.0 - A protocol for tracking validator performance, rewarding accurate block validation, and maintaining network integrity

;; Constants
(define-constant ERR-NOT-NETWORK-CONTROLLER (err u1))
(define-constant ERR-NETWORK-PAUSED (err u2))
(define-constant ERR-INVALID-VALIDATION (err u3))
(define-constant ERR-VALIDATION-ALREADY-VERIFIED (err u4))
(define-constant ERR-INCORRECT-VALIDATION-PROOF (err u5))
(define-constant ERR-VERIFICATION-WINDOW-ACTIVE (err u6))
(define-constant ERR-INSUFFICIENT-STAKE (err u7))
(define-constant ERR-INVALID-PARAMETER (err u8))
(define-constant ERR-VALIDATION-EXISTS (err u9))
(define-constant MAX-VALIDATION-ID u100) ;; Maximum allowed validation task ID

;; Data Variables
(define-data-var network-controller principal tx-sender)
(define-data-var network-status bool false)
(define-data-var current-epoch uint u0)
(define-data-var staking-requirement uint u1000000) ;; 1 STX
(define-data-var total-rewards-pool uint u0)
(define-data-var latest-network-block uint u0) ;; Block height tracking for verification windows

;; Validation Task Structure
(define-map validation-tasks
    uint
    {
        block-hash: (string-utf8 256),
        expected-result-hash: (buff 32),  ;; SHA256 hash of the expected validation result
        verification-time: uint,          ;; Verification time (block height)
        reward: uint,
        verified: bool
    }
)

;; Validator Performance Tracking
(define-map validator-records
    principal
    {
        active-task: uint,
        correct-validations: (list 20 uint),
        last-validation: uint,
        total-correct: uint
    }
)

;; Validation History
(define-map task-validations
    {task-id: uint, validator: principal}
    {
        submissions: uint,
        verified-at: (optional uint)
    }
)

;; Events
(define-map verification-history
    uint
    (list 10 {validator: principal, verified-block: uint})
)

;; Authorization
(define-private (is-controller)
    (is-eq tx-sender (var-get network-controller)))

;; Block Height Management
(define-public (update-network-block (new-block uint))
    (begin
        (asserts! (is-controller) ERR-NOT-NETWORK-CONTROLLER)
        ;; Validate block is not less than current
        (asserts! (>= new-block (var-get latest-network-block)) ERR-INVALID-PARAMETER)
        (var-set latest-network-block new-block)
        (ok true)))

;; Protocol Management Functions
(define-public (activate-network)
    (begin
        (asserts! (is-controller) ERR-NOT-NETWORK-CONTROLLER)
        (var-set network-status true)
        (var-set current-epoch u0)
        (var-set total-rewards-pool u0)
        (ok true)))

(define-public (pause-network)
    (begin
        (asserts! (is-controller) ERR-NOT-NETWORK-CONTROLLER)
        (var-set network-status false)
        (ok true)))

(define-public (create-validation-task
    (task-id uint)
    (block-hash (string-utf8 256))
    (expected-result-hash (buff 32))
    (verification-time uint)
    (reward uint))
    (begin
        (asserts! (is-controller) ERR-NOT-NETWORK-CONTROLLER)
        
        ;; Validate task-id is within acceptable range
        (asserts! (<= task-id MAX-VALIDATION-ID) ERR-INVALID-PARAMETER)
        
        ;; Check if task already exists to prevent overwriting
        (asserts! (is-none (map-get? validation-tasks task-id)) ERR-VALIDATION-EXISTS)
        
        ;; Validate verification time is in the future
        (asserts! (>= verification-time (var-get latest-network-block)) ERR-INVALID-PARAMETER)
        
        ;; Validate expected result hash is not empty
        (asserts! (> (len expected-result-hash) u0) ERR-INVALID-PARAMETER)
        
        ;; Validate block hash is not empty
        (asserts! (> (len block-hash) u0) ERR-INVALID-PARAMETER)
        
        ;; Validate reward is a positive amount
        (asserts! (> reward u0) ERR-INVALID-PARAMETER)
        
        ;; Set the task data
        (map-set validation-tasks task-id
            {
                block-hash: block-hash,
                expected-result-hash: expected-result-hash,
                verification-time: verification-time,
                reward: reward,
                verified: false
            })
            
        ;; Calculate new rewards pool safely
        (let ((new-pool (+ (var-get total-rewards-pool) reward)))
            ;; Make sure the addition doesn't overflow
            (asserts! (>= new-pool (var-get total-rewards-pool)) ERR-INVALID-PARAMETER)
            ;; Update the total rewards pool
            (var-set total-rewards-pool new-pool))
        (ok true)))

;; Validator Registration
(define-public (register-as-validator)
    (begin
        (asserts! (var-get network-status) ERR-NETWORK-PAUSED)
        ;; Require staking requirement
        (try! (stx-transfer? (var-get staking-requirement) tx-sender (var-get network-controller)))
        
        (map-set validator-records tx-sender
            {
                active-task: u0,
                correct-validations: (list),
                last-validation: u0,
                total-correct: u0
            })
        (ok true)))

;; Validation Submission Functions
(define-public (submit-validation-result
    (task-id uint)
    (validation-proof (buff 32)))
    (let (
        (task (unwrap! (map-get? validation-tasks task-id) ERR-INVALID-VALIDATION))
        (validator (unwrap! (map-get? validator-records tx-sender) ERR-INVALID-VALIDATION))
        (current-block (var-get latest-network-block))
        )
        ;; Check task availability
        (asserts! (var-get network-status) ERR-NETWORK-PAUSED)
        (asserts! (>= current-block (get verification-time task)) ERR-VERIFICATION-WINDOW-ACTIVE)
        (asserts! (not (get verified task)) ERR-VALIDATION-ALREADY-VERIFIED)
        
        ;; Verify validation proof - directly compare the hashes
        (if (is-eq validation-proof (get expected-result-hash task))
            (begin
                ;; Update task status
                (map-set validation-tasks task-id
                    (merge task {verified: true}))
                
                ;; Update validator record
                (map-set validator-records tx-sender
                    (merge validator {
                        active-task: (+ task-id u1),
                        correct-validations: (unwrap! (as-max-len? 
                            (append (get correct-validations validator) task-id) u20)
                            ERR-INVALID-VALIDATION),
                        last-validation: current-block,
                        total-correct: (+ (get total-correct validator) u1)
                    }))
                
                ;; Record validation
                (map-set task-validations
                    {task-id: task-id, validator: tx-sender}
                    {
                        submissions: u1,
                        verified-at: (some current-block)
                    })
                
                ;; Distribute reward
                (try! (stx-transfer? (get reward task) (var-get network-controller) tx-sender))
                
                ;; Record verification
                (match (map-get? verification-history task-id)
                    history (map-set verification-history task-id
                        (unwrap! (as-max-len?
                            (append history {validator: tx-sender, verified-block: current-block})
                            u10)
                            ERR-INVALID-VALIDATION))
                    (map-set verification-history task-id
                        (list {validator: tx-sender, verified-block: current-block})))
                
                (ok true))
            ERR-INCORRECT-VALIDATION-PROOF)))

;; Network Management Functions
(define-public (update-staking-requirement (new-requirement uint))
    (begin
        (asserts! (is-controller) ERR-NOT-NETWORK-CONTROLLER)
        (var-set staking-requirement new-requirement)
        (ok true)))

(define-public (advance-epoch)
    (begin
        (asserts! (is-controller) ERR-NOT-NETWORK-CONTROLLER)
        (var-set current-epoch (+ (var-get current-epoch) u1))
        (ok true)))

;; Read-only functions
(define-read-only (get-task-details (task-id uint))
    (match (map-get? validation-tasks task-id)
        task (if (>= (var-get latest-network-block) (get verification-time task))
            (ok (get block-hash task))
            ERR-VERIFICATION-WINDOW-ACTIVE)
        ERR-INVALID-VALIDATION))

(define-read-only (get-validator-profile (validator principal))
    (map-get? validator-records validator))

(define-read-only (get-verification-data (task-id uint))
    (map-get? verification-history task-id))

(define-read-only (get-current-block)
    (var-get latest-network-block))

(define-read-only (get-network-metrics)
    {
        active: (var-get network-status),
        current-epoch: (var-get current-epoch),
        total-rewards-pool: (var-get total-rewards-pool),
        staking-requirement: (var-get staking-requirement),
        latest-network-block: (var-get latest-network-block)
    })