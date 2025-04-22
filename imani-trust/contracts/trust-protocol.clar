;; Blockchain Validator Network Reputation System
;; v0.1 - Basic network controller and validator registration

;; Constants
(define-constant ERR-NOT-NETWORK-CONTROLLER (err u1))
(define-constant ERR-NETWORK-PAUSED (err u2))
(define-constant ERR-INVALID-VALIDATION (err u3))
(define-constant ERR-INSUFFICIENT-STAKE (err u7))

;; Data Variables
(define-data-var network-controller principal tx-sender)
(define-data-var network-status bool false)
(define-data-var current-epoch uint u0)
(define-data-var staking-requirement uint u1000000) ;; 1 STX

;; Validator Performance Tracking
(define-map validator-records
    principal
    {
        active: bool,
        last-validation: uint,
        total-correct: uint
    }
)

;; Authorization
(define-private (is-controller)
    (is-eq tx-sender (var-get network-controller)))

;; Protocol Management Functions
(define-public (activate-network)
    (begin
        (asserts! (is-controller) ERR-NOT-NETWORK-CONTROLLER)
        (var-set network-status true)
        (var-set current-epoch u0)
        (ok true)))

(define-public (pause-network)
    (begin
        (asserts! (is-controller) ERR-NOT-NETWORK-CONTROLLER)
        (var-set network-status false)
        (ok true)))

;; Validator Registration
(define-public (register-as-validator)
    (begin
        (asserts! (var-get network-status) ERR-NETWORK-PAUSED)
        ;; Require staking requirement
        (try! (stx-transfer? (var-get staking-requirement) tx-sender (var-get network-controller)))
        
        (map-set validator-records tx-sender
            {
                active: true,
                last-validation: u0,
                total-correct: u0
            })
        (ok true)))

;; Read-only functions
(define-read-only (get-validator-profile (validator principal))
    (map-get? validator-records validator))

(define-read-only (get-network-status)
    (var-get network-status))

(define-read-only (get-current-epoch)
    (var-get current-epoch))

(define-read-only (get-staking-requirement)
    (var-get staking-requirement))

(define-public (update-staking-requirement (new-requirement uint))
    (begin
        (asserts! (is-controller) ERR-NOT-NETWORK-CONTROLLER)
        (var-set staking-requirement new-requirement)
        (ok true)))