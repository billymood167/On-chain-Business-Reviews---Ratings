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
(define-constant REPUTATION-DECAY-FACTOR u95)
(define-constant CREDIBILITY-THRESHOLD u3)
(define-constant HIGH-VALUE-PURCHASE-MULTIPLIER u150)
(define-constant RECENT-REVIEW-BONUS u110)
(define-constant BLOCKS-PER-MONTH u4320)

(define-map BusinessReputation
    { business-id: uint }
    {
        reputation-score: uint,
        last-updated: uint,
        review-velocity: uint,
        credibility-score: uint,
    }
)

(define-map ReviewerCredibility
    { reviewer: principal }
    {
        total-reviews: uint,
        helpful-votes: uint,
        credibility-score: uint,
    }
)

(define-public (calculate-business-reputation (business-id uint))
    (let (
            (business (unwrap! (map-get? Businesses { business-id: business-id })
                ERR-BUSINESS-NOT-FOUND
            ))
            (current-block burn-block-height)
            (reputation-data (default-to {
                reputation-score: u0,
                last-updated: u0,
                review-velocity: u0,
                credibility-score: u0,
            }
                (map-get? BusinessReputation { business-id: business-id })
            ))
        )
        (let (
                (base-score (* (get avg-rating business) u20))
                (time-weighted-score (calculate-time-weighted-score business-id current-block))
                (credibility-weighted-score (calculate-credibility-weighted-score business-id))
                (final-score (/ (+ base-score time-weighted-score credibility-weighted-score)
                    u3
                ))
            )
            (map-set BusinessReputation { business-id: business-id } {
                reputation-score: final-score,
                last-updated: current-block,
                review-velocity: (calculate-review-velocity business-id current-block),
                credibility-score: credibility-weighted-score,
            })
            (ok final-score)
        )
    )
)

(define-private (calculate-time-weighted-score
        (business-id uint)
        (current-block uint)
    )
    (let (
            (recent-cutoff (- current-block BLOCKS-PER-MONTH))
            (business (unwrap-panic (map-get? Businesses { business-id: business-id })))
        )
        (fold calculate-review-time-weight (list u1 u2 u3 u4 u5) u0)
    )
)

(define-private (calculate-review-time-weight
        (review-id uint)
        (acc uint)
    )
    (match (map-get? Reviews { review-id: review-id })
        review (let (
                (age-factor (if (> (get timestamp review)
                        (- burn-block-height BLOCKS-PER-MONTH)
                    )
                    RECENT-REVIEW-BONUS
                    u100
                ))
                (rating-contribution (* (get rating review) age-factor))
            )
            (+ acc (/ rating-contribution u100))
        )
        acc
    )
)

(define-private (calculate-credibility-weighted-score (business-id uint))
    (fold calculate-reviewer-credibility-weight (list u1 u2 u3 u4 u5) u0)
)

(define-private (calculate-reviewer-credibility-weight
        (review-id uint)
        (acc uint)
    )
    (match (map-get? Reviews { review-id: review-id })
        review (let (
                (reviewer-cred (default-to {
                    total-reviews: u0,
                    helpful-votes: u0,
                    credibility-score: u100,
                }
                    (map-get? ReviewerCredibility { reviewer: (get reviewer review) })
                ))
                (credibility-multiplier (if (>= (get credibility-score reviewer-cred) u120)
                    u120
                    u100
                ))
                (weighted-rating (* (get rating review) credibility-multiplier))
            )
            (+ acc (/ weighted-rating u100))
        )
        acc
    )
)

(define-private (calculate-review-velocity
        (business-id uint)
        (current-block uint)
    )
    (let (
            (recent-cutoff (- current-block BLOCKS-PER-MONTH))
            (recent-reviews (fold count-recent-reviews (list u1 u2 u3 u4 u5) u0))
        )
        recent-reviews
    )
)

(define-private (count-recent-reviews
        (review-id uint)
        (acc uint)
    )
    (match (map-get? Reviews { review-id: review-id })
        review (if (> (get timestamp review) (- burn-block-height BLOCKS-PER-MONTH))
            (+ acc u1)
            acc
        )
        acc
    )
)

(define-public (update-reviewer-credibility (reviewer principal))
    (let (
            (current-cred (default-to {
                total-reviews: u0,
                helpful-votes: u0,
                credibility-score: u100,
            }
                (map-get? ReviewerCredibility { reviewer: reviewer })
            ))
            (total-reviews (+ (get total-reviews current-cred) u1))
            (new-credibility-score (if (>= total-reviews CREDIBILITY-THRESHOLD)
                (+ u100 (* (get helpful-votes current-cred) u5))
                u100
            ))
        )
        (map-set ReviewerCredibility { reviewer: reviewer } {
            total-reviews: total-reviews,
            helpful-votes: (get helpful-votes current-cred),
            credibility-score: new-credibility-score,
        })
        (ok true)
    )
)

(define-read-only (get-business-reputation (business-id uint))
    (map-get? BusinessReputation { business-id: business-id })
)

(define-read-only (get-reviewer-credibility (reviewer principal))
    (map-get? ReviewerCredibility { reviewer: reviewer })
)
(define-fungible-token review-token)

(define-constant REVIEW-REWARD u10)
(define-constant QUALITY-REVIEW-BONUS u5)
(define-constant BUSINESS-EXCELLENCE-REWARD u50)
(define-constant LOYALTY-MULTIPLIER u2)
(define-constant MIN-REVIEW-LENGTH u50)
(define-constant EXCELLENCE-RATING-THRESHOLD u450)

(define-data-var total-token-supply uint u0)
(define-data-var reward-pool uint u1000000)

(define-map ReviewBounties
    { business-id: uint }
    {
        bounty-amount: uint,
        sponsor: principal,
        active: bool,
        claimed-count: uint,
        max-claims: uint,
    }
)

(define-map CustomerLoyalty
    { customer: principal }
    {
        total-reviews: uint,
        quality-reviews: uint,
        loyalty-tier: uint,
        total-rewards: uint,
    }
)

(define-map BusinessIncentives
    { business-id: uint }
    {
        excellence-streak: uint,
        last-reward-block: uint,
        total-incentives-earned: uint,
    }
)

(define-public (mint-initial-tokens (amount uint))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-address)) ERR-NOT-AUTHORIZED)
        (try! (ft-mint? review-token amount tx-sender))
        (var-set total-token-supply (+ (var-get total-token-supply) amount))
        (ok true)
    )
)

(define-public (create-review-bounty
        (business-id uint)
        (bounty-amount uint)
        (max-claims uint)
    )
    (begin
        (asserts! (is-some (map-get? Businesses { business-id: business-id }))
            ERR-BUSINESS-NOT-FOUND
        )
        (try! (ft-transfer? review-token bounty-amount tx-sender
            (as-contract tx-sender)
        ))
        (map-set ReviewBounties { business-id: business-id } {
            bounty-amount: bounty-amount,
            sponsor: tx-sender,
            active: true,
            claimed-count: u0,
            max-claims: max-claims,
        })
        (ok true)
    )
)

(define-public (claim-review-reward (review-id uint))
    (let (
            (review (unwrap! (map-get? Reviews { review-id: review-id })
                ERR-REVIEW-NOT-FOUND
            ))
            (reviewer (get reviewer review))
            (business-id (get business-id review))
            (bounty-data (map-get? ReviewBounties { business-id: business-id }))
        )
        (begin
            (asserts! (is-eq reviewer tx-sender) ERR-NOT-AUTHORIZED)
            (let (
                    (base-reward REVIEW-REWARD)
                    (quality-bonus (if (>= (len (get review-text review)) MIN-REVIEW-LENGTH)
                        QUALITY-REVIEW-BONUS
                        u0
                    ))
                    (loyalty-data (default-to {
                        total-reviews: u0,
                        quality-reviews: u0,
                        loyalty-tier: u1,
                        total-rewards: u0,
                    }
                        (map-get? CustomerLoyalty { customer: reviewer })
                    ))
                    (loyalty-bonus (* base-reward (- (get loyalty-tier loyalty-data) u1)))
                    (bounty-reward (match bounty-data
                        bounty (if (and
                                (get active bounty)
                                (< (get claimed-count bounty)
                                    (get max-claims bounty)
                                )
                            )
                            (get bounty-amount bounty)
                            u0
                        )
                        u0
                    ))
                    (total-reward (+ base-reward quality-bonus loyalty-bonus bounty-reward))
                )
                (try! (as-contract (ft-transfer? review-token total-reward tx-sender reviewer)))
                (unwrap-panic (update-customer-loyalty reviewer (> quality-bonus u0)))
                (match bounty-data
                    bounty (map-set ReviewBounties { business-id: business-id }
                        (merge bounty { claimed-count: (+ (get claimed-count bounty) u1) })
                    )
                    true
                )
                (ok total-reward)
            )
        )
    )
)

(define-public (reward-business-excellence (business-id uint))
    (let (
            (business (unwrap! (map-get? Businesses { business-id: business-id })
                ERR-BUSINESS-NOT-FOUND
            ))
            (incentive-data (default-to {
                excellence-streak: u0,
                last-reward-block: u0,
                total-incentives-earned: u0,
            }
                (map-get? BusinessIncentives { business-id: business-id })
            ))
        )
        (asserts! (>= (get avg-rating business) EXCELLENCE-RATING-THRESHOLD)
            ERR-NOT-AUTHORIZED
        )
        (asserts! (>= (get total-ratings business) u5) ERR-NOT-AUTHORIZED)
        (let (
                (new-streak (+ (get excellence-streak incentive-data) u1))
                (streak-multiplier (if (>= new-streak u3)
                    u2
                    u1
                ))
                (reward-amount (* BUSINESS-EXCELLENCE-REWARD streak-multiplier))
            )
            (try! (as-contract (ft-transfer? review-token reward-amount tx-sender
                (get owner business)
            )))
            (map-set BusinessIncentives { business-id: business-id } {
                excellence-streak: new-streak,
                last-reward-block: burn-block-height,
                total-incentives-earned: (+ (get total-incentives-earned incentive-data) reward-amount),
            })
            (ok reward-amount)
        )
    )
)

(define-private (update-customer-loyalty
        (customer principal)
        (is-quality-review bool)
    )
    (let (
            (loyalty-data (default-to {
                total-reviews: u0,
                quality-reviews: u0,
                loyalty-tier: u1,
                total-rewards: u0,
            }
                (map-get? CustomerLoyalty { customer: customer })
            ))
            (new-total-reviews (+ (get total-reviews loyalty-data) u1))
            (new-quality-reviews (if is-quality-review
                (+ (get quality-reviews loyalty-data) u1)
                (get quality-reviews loyalty-data)
            ))
            (new-tier (calculate-loyalty-tier new-total-reviews new-quality-reviews))
        )
        (map-set CustomerLoyalty { customer: customer } {
            total-reviews: new-total-reviews,
            quality-reviews: new-quality-reviews,
            loyalty-tier: new-tier,
            total-rewards: (get total-rewards loyalty-data),
        })
        (ok true)
    )
)

(define-private (calculate-loyalty-tier
        (total-reviews uint)
        (quality-reviews uint)
    )
    (if (>= total-reviews u20)
        u4
        (if (>= total-reviews u10)
            u3
            (if (>= total-reviews u5)
                u2
                u1
            )
        )
    )
)

(define-read-only (get-review-bounty (business-id uint))
    (map-get? ReviewBounties { business-id: business-id })
)

(define-read-only (get-customer-loyalty (customer principal))
    (map-get? CustomerLoyalty { customer: customer })
)

(define-read-only (get-business-incentives (business-id uint))
    (map-get? BusinessIncentives { business-id: business-id })
)

(define-read-only (get-token-balance (account principal))
    (ft-get-balance review-token account)
)
