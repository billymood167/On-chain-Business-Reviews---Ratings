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

(define-constant ERR-ALREADY-RESPONDED (err u106))
(define-constant ERR-RESPONSE-TOO-LONG (err u107))
(define-constant RESPONSE-COOLDOWN u144)
(define-constant MAX-RESPONSE-LENGTH u300)

(define-data-var response-counter uint u0)

(define-map BusinessResponses
    { response-id: uint }
    {
        review-id: uint,
        business-id: uint,
        responder: principal,
        response-text: (string-ascii 300),
        timestamp: uint,
        is-public: bool,
    }
)

(define-map ReviewResponseMap
    { review-id: uint }
    { response-id: uint }
)

(define-map ResponseMetrics
    { business-id: uint }
    {
        total-responses: uint,
        average-response-time: uint,
        last-response-block: uint,
    }
)

(define-public (respond-to-review
        (review-id uint)
        (response-text (string-ascii 300))
        (is-public bool)
    )
    (let (
            (review (unwrap! (map-get? Reviews { review-id: review-id })
                ERR-REVIEW-NOT-FOUND
            ))
            (business-id (get business-id review))
            (business (unwrap! (map-get? Businesses { business-id: business-id })
                ERR-BUSINESS-NOT-FOUND
            ))
            (new-response-id (+ (var-get response-counter) u1))
        )
        (asserts! (is-eq tx-sender (get owner business)) ERR-NOT-AUTHORIZED)
        (asserts! (<= (len response-text) MAX-RESPONSE-LENGTH)
            ERR-RESPONSE-TOO-LONG
        )
        (asserts! (is-none (map-get? ReviewResponseMap { review-id: review-id }))
            ERR-ALREADY-RESPONDED
        )
        (let (
                (response-time (- burn-block-height (get timestamp review)))
                (current-metrics (default-to {
                    total-responses: u0,
                    average-response-time: u0,
                    last-response-block: u0,
                }
                    (map-get? ResponseMetrics { business-id: business-id })
                ))
            )
            (map-set BusinessResponses { response-id: new-response-id } {
                review-id: review-id,
                business-id: business-id,
                responder: tx-sender,
                response-text: response-text,
                timestamp: burn-block-height,
                is-public: is-public,
            })
            (map-set ReviewResponseMap { review-id: review-id } { response-id: new-response-id })
            (map-set ResponseMetrics { business-id: business-id } {
                total-responses: (+ (get total-responses current-metrics) u1),
                average-response-time: (calculate-avg-response-time
                    (get total-responses current-metrics)
                    (get average-response-time current-metrics)
                    response-time
                ),
                last-response-block: burn-block-height,
            })
            (var-set response-counter new-response-id)
            (ok new-response-id)
        )
    )
)

(define-public (update-response
        (response-id uint)
        (new-response-text (string-ascii 300))
    )
    (let ((response (unwrap! (map-get? BusinessResponses { response-id: response-id })
            ERR-REVIEW-NOT-FOUND
        )))
        (asserts! (is-eq tx-sender (get responder response)) ERR-NOT-AUTHORIZED)
        (asserts! (<= (len new-response-text) MAX-RESPONSE-LENGTH)
            ERR-RESPONSE-TOO-LONG
        )
        (asserts!
            (>= (- burn-block-height (get timestamp response)) RESPONSE-COOLDOWN)
            ERR-NOT-AUTHORIZED
        )
        (map-set BusinessResponses { response-id: response-id }
            (merge response {
                response-text: new-response-text,
                timestamp: burn-block-height,
            })
        )
        (ok true)
    )
)

(define-private (calculate-avg-response-time
        (total-responses uint)
        (current-avg uint)
        (new-response-time uint)
    )
    (if (is-eq total-responses u0)
        new-response-time
        (/ (+ (* current-avg total-responses) new-response-time)
            (+ total-responses u1)
        )
    )
)

(define-read-only (get-response-to-review (review-id uint))
    (match (map-get? ReviewResponseMap { review-id: review-id })
        response-map (map-get? BusinessResponses { response-id: (get response-id response-map) })
        none
    )
)

(define-read-only (get-response-details (response-id uint))
    (map-get? BusinessResponses { response-id: response-id })
)

(define-read-only (get-business-response-metrics (business-id uint))
    (map-get? ResponseMetrics { business-id: business-id })
)

(define-read-only (get-business-responsiveness-score (business-id uint))
    (match (map-get? ResponseMetrics { business-id: business-id })
        metrics (let (
                (business-data (unwrap-panic (map-get? Businesses { business-id: business-id })))
                (total-ratings (get total-ratings business-data))
                (response-rate (if (> (get total-responses metrics) u0)
                    (/ (* (get total-responses metrics) u100)
                        (if (> total-ratings u0)
                            total-ratings
                            u1
                        ))
                    u0
                ))
                (speed-score (if (> (get average-response-time metrics) u0)
                    (/ u14400 (get average-response-time metrics))
                    u0
                ))
            )
            (some (/ (+ response-rate speed-score) u2))
        )
        none
    )
)

(define-constant ERR-ALREADY-VOTED (err u108))
(define-constant ERR-SELF-VOTE (err u109))
(define-constant HELPFULNESS-REWARD u3)
(define-constant HELPFUL-VOTE-THRESHOLD u5)

(define-map ReviewHelpfulness
    { review-id: uint }
    {
        helpful-votes: uint,
        unhelpful-votes: uint,
        total-votes: uint,
        helpfulness-score: uint,
    }
)

(define-map UserVotes
    {
        voter: principal,
        review-id: uint,
    }
    {
        vote-type: bool,
        timestamp: uint,
    }
)

(define-map ReviewerHelpfulnessStats
    { reviewer: principal }
    {
        reviews-with-helpful-votes: uint,
        total-helpful-votes-received: uint,
        helpfulness-ranking: uint,
    }
)

(define-public (vote-on-review-helpfulness
        (review-id uint)
        (is-helpful bool)
    )
    (let (
            (review (unwrap! (map-get? Reviews { review-id: review-id })
                ERR-REVIEW-NOT-FOUND
            ))
            (reviewer (get reviewer review))
            (current-helpfulness (default-to {
                helpful-votes: u0,
                unhelpful-votes: u0,
                total-votes: u0,
                helpfulness-score: u0,
            }
                (map-get? ReviewHelpfulness { review-id: review-id })
            ))
        )
        (asserts! (not (is-eq tx-sender reviewer)) ERR-SELF-VOTE)
        (asserts!
            (is-none (map-get? UserVotes {
                voter: tx-sender,
                review-id: review-id,
            }))
            ERR-ALREADY-VOTED
        )
        (let (
                (new-helpful-votes (if is-helpful
                    (+ (get helpful-votes current-helpfulness) u1)
                    (get helpful-votes current-helpfulness)
                ))
                (new-unhelpful-votes (if is-helpful
                    (get unhelpful-votes current-helpfulness)
                    (+ (get unhelpful-votes current-helpfulness) u1)
                ))
                (new-total-votes (+ (get total-votes current-helpfulness) u1))
                (new-helpfulness-score (calculate-helpfulness-score new-helpful-votes new-total-votes))
            )
            (map-set ReviewHelpfulness { review-id: review-id } {
                helpful-votes: new-helpful-votes,
                unhelpful-votes: new-unhelpful-votes,
                total-votes: new-total-votes,
                helpfulness-score: new-helpfulness-score,
            })
            (map-set UserVotes {
                voter: tx-sender,
                review-id: review-id,
            } {
                vote-type: is-helpful,
                timestamp: burn-block-height,
            })
            (if is-helpful
                (unwrap-panic (update-reviewer-helpfulness-stats reviewer))
                true
            )
            (if (and is-helpful (>= new-helpful-votes HELPFUL-VOTE-THRESHOLD))
                (unwrap-panic (as-contract (ft-transfer? review-token HELPFULNESS-REWARD tx-sender reviewer)))
                false
            )
            (ok new-helpfulness-score)
        )
    )
)

(define-public (change-helpfulness-vote
        (review-id uint)
        (new-is-helpful bool)
    )
    (let (
            (existing-vote (unwrap!
                (map-get? UserVotes {
                    voter: tx-sender,
                    review-id: review-id,
                })
                ERR-REVIEW-NOT-FOUND
            ))
            (current-helpfulness (unwrap! (map-get? ReviewHelpfulness { review-id: review-id })
                ERR-REVIEW-NOT-FOUND
            ))
            (old-vote-type (get vote-type existing-vote))
        )
        (asserts! (not (is-eq old-vote-type new-is-helpful)) ERR-ALREADY-VOTED)
        (let (
                (helpful-adjustment (if new-is-helpful
                    u1
                    (- u0 u1)
                ))
                (unhelpful-adjustment (if new-is-helpful
                    (- u0 u1)
                    u1
                ))
                (new-helpful-votes (+ (get helpful-votes current-helpfulness) helpful-adjustment))
                (new-unhelpful-votes (+ (get unhelpful-votes current-helpfulness) unhelpful-adjustment))
                (new-helpfulness-score (calculate-helpfulness-score new-helpful-votes
                    (get total-votes current-helpfulness)
                ))
            )
            (map-set ReviewHelpfulness { review-id: review-id } {
                helpful-votes: new-helpful-votes,
                unhelpful-votes: new-unhelpful-votes,
                total-votes: (get total-votes current-helpfulness),
                helpfulness-score: new-helpfulness-score,
            })
            (map-set UserVotes {
                voter: tx-sender,
                review-id: review-id,
            } {
                vote-type: new-is-helpful,
                timestamp: burn-block-height,
            })
            (ok new-helpfulness-score)
        )
    )
)

(define-private (calculate-helpfulness-score
        (helpful-votes uint)
        (total-votes uint)
    )
    (if (> total-votes u0)
        (/ (* helpful-votes u100) total-votes)
        u50
    )
)

(define-private (update-reviewer-helpfulness-stats (reviewer principal))
    (let (
            (current-stats (default-to {
                reviews-with-helpful-votes: u0,
                total-helpful-votes-received: u0,
                helpfulness-ranking: u0,
            }
                (map-get? ReviewerHelpfulnessStats { reviewer: reviewer })
            ))
            (new-total-votes (+ (get total-helpful-votes-received current-stats) u1))
        )
        (map-set ReviewerHelpfulnessStats { reviewer: reviewer } {
            reviews-with-helpful-votes: (+ (get reviews-with-helpful-votes current-stats) u1),
            total-helpful-votes-received: new-total-votes,
            helpfulness-ranking: (calculate-helpfulness-ranking new-total-votes),
        })
        (ok true)
    )
)

(define-private (calculate-helpfulness-ranking (total-helpful-votes uint))
    (if (>= total-helpful-votes u50)
        u5
        (if (>= total-helpful-votes u25)
            u4
            (if (>= total-helpful-votes u10)
                u3
                (if (>= total-helpful-votes u5)
                    u2
                    u1
                )
            )
        )
    )
)

(define-read-only (get-review-helpfulness (review-id uint))
    (map-get? ReviewHelpfulness { review-id: review-id })
)

(define-read-only (get-user-vote
        (voter principal)
        (review-id uint)
    )
    (map-get? UserVotes {
        voter: voter,
        review-id: review-id,
    })
)

(define-read-only (get-reviewer-helpfulness-stats (reviewer principal))
    (map-get? ReviewerHelpfulnessStats { reviewer: reviewer })
)

(define-read-only (get-top-helpful-reviews (business-id uint))
    (list)
)
