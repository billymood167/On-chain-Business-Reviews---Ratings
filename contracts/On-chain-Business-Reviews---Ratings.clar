(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-RATING (err u101))
(define-constant ERR-BUSINESS-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-REVIEWED (err u103))
(define-constant ERR-REVIEW-NOT-FOUND (err u104))
(define-constant ERR-NOT-CUSTOMER (err u105))

(define-data-var dao-address principal 'SP000000000000000000002Q6VF78)
(define-data-var min-purchase-amount uint u100)

(define-map Businesses
    { business-id: uint }
    {
        owner: principal,
        name: (string-ascii 50),
        total-ratings: uint,
        avg-rating: uint,
    }
)

(define-map Reviews
    { review-id: uint }
    {
        business-id: uint,
        reviewer: principal,
        rating: uint,
        review-text: (string-ascii 500),
        timestamp: uint,
        flagged: bool,
        verified: bool,
    }
)

(define-map CustomerPurchases
    {
        customer: principal,
        business-id: uint,
    }
    {
        amount: uint,
        timestamp: uint,
    }
)

(define-data-var review-counter uint u0)
(define-data-var business-counter uint u0)

(define-public (register-business (name (string-ascii 50)))
    (let ((new-id (+ (var-get business-counter) u1)))
        (map-insert Businesses { business-id: new-id } {
            owner: tx-sender,
            name: name,
            total-ratings: u0,
            avg-rating: u0,
        })
        (var-set business-counter new-id)
        (ok new-id)
    )
)

(define-public (record-purchase
        (business-id uint)
        (amount uint)
    )
    (begin
        (asserts! (>= amount (var-get min-purchase-amount)) ERR-NOT-AUTHORIZED)
        (map-set CustomerPurchases {
            customer: tx-sender,
            business-id: business-id,
        } {
            amount: amount,
            timestamp: burn-block-height,
        })
        (ok true)
    )
)

(define-public (submit-review
        (business-id uint)
        (rating uint)
        (review-text (string-ascii 500))
    )
    (let ((new-review-id (+ (var-get review-counter) u1)))
        (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
        (asserts! (is-some (map-get? Businesses { business-id: business-id }))
            ERR-BUSINESS-NOT-FOUND
        )
        (asserts!
            (is-some (map-get? CustomerPurchases {
                customer: tx-sender,
                business-id: business-id,
            }))
            ERR-NOT-CUSTOMER
        )
        (map-set Reviews { review-id: new-review-id } {
            business-id: business-id,
            reviewer: tx-sender,
            rating: rating,
            review-text: review-text,
            timestamp: burn-block-height,
            flagged: false,
            verified: true,
        })
        (var-set review-counter new-review-id)
        (try! (update-business-rating business-id rating))
        (ok new-review-id)
    )
)

(define-private (update-business-rating
        (business-id uint)
        (new-rating uint)
    )
    (match (map-get? Businesses { business-id: business-id })
        business (ok (let (
                (current-total (get total-ratings business))
                (current-avg (get avg-rating business))
            )
            (map-set Businesses { business-id: business-id } {
                owner: (get owner business),
                name: (get name business),
                total-ratings: (+ current-total u1),
                avg-rating: (/ (+ (* current-avg current-total) new-rating)
                    (+ current-total u1)
                ),
            })
        ))
        ERR-BUSINESS-NOT-FOUND
    )
)

(define-public (flag-review (review-id uint))
    (let ((review (unwrap! (map-get? Reviews { review-id: review-id }) ERR-REVIEW-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get dao-address)) ERR-NOT-AUTHORIZED)
        (map-set Reviews { review-id: review-id }
            (merge review { flagged: true })
        )
        (ok true)
    )
)

(define-public (remove-flagged-review (review-id uint))
    (let ((review (unwrap! (map-get? Reviews { review-id: review-id }) ERR-REVIEW-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get dao-address)) ERR-NOT-AUTHORIZED)
        (asserts! (get flagged review) ERR-NOT-AUTHORIZED)
        (map-delete Reviews { review-id: review-id })
        (ok true)
    )
)

(define-read-only (get-business-details (business-id uint))
    (map-get? Businesses { business-id: business-id })
)

(define-read-only (get-review-details (review-id uint))
    (map-get? Reviews { review-id: review-id })
)

(define-read-only (get-customer-purchase-history (business-id uint))
    (map-get? CustomerPurchases {
        customer: tx-sender,
        business-id: business-id,
    })
)

(define-public (update-dao-address (new-address principal))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-address)) ERR-NOT-AUTHORIZED)
        (var-set dao-address new-address)
        (ok true)
    )
)
