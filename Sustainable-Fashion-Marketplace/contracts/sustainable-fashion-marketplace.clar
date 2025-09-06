;; Sustainable Fashion Marketplace Smart Contract
;; A decentralized marketplace for sustainable fashion items with sustainability scoring

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-insufficient-payment (err u103))
(define-constant err-item-not-available (err u104))
(define-constant err-invalid-sustainability-score (err u105))
(define-constant err-invalid-price (err u106))
(define-constant err-seller-cannot-buy (err u107))

;; Data Variables
(define-data-var next-item-id uint u1)
(define-data-var marketplace-fee-percentage uint u250) ;; 2.5%
(define-data-var min-sustainability-score uint u1)
(define-data-var max-sustainability-score uint u100)

;; Data Maps
(define-map items
  uint
  {
    seller: principal,
    name: (string-ascii 50),
    description: (string-ascii 200),
    price: uint,
    category: (string-ascii 20),
    sustainability-score: uint,
    materials: (string-ascii 100),
    carbon-footprint: uint,
    is-available: bool,
    created-at: uint,
    certifications: (list 5 (string-ascii 30))
  }
)

(define-map sellers
  principal
  {
    name: (string-ascii 50),
    reputation-score: uint,
    total-sales: uint,
    sustainability-rating: uint,
    is-verified: bool
  }
)

(define-map purchases
  uint
  {
    item-id: uint,
    buyer: principal,
    seller: principal,
    price: uint,
    purchase-date: uint,
    is-reviewed: bool
  }
)

(define-map reviews
  uint
  {
    item-id: uint,
    reviewer: principal,
    rating: uint,
    sustainability-feedback: uint,
    review-text: (string-ascii 200)
  }
)

(define-data-var next-purchase-id uint u1)
(define-data-var next-review-id uint u1)

;; Private Functions
(define-private (calculate-marketplace-fee (price uint))
  (/ (* price (var-get marketplace-fee-percentage)) u10000)
)

(define-private (update-seller-stats (seller principal) (sale-amount uint))
  (let ((current-stats (default-to 
    {name: "", reputation-score: u50, total-sales: u0, sustainability-rating: u50, is-verified: false}
    (map-get? sellers seller))))
    (map-set sellers seller
      (merge current-stats {
        total-sales: (+ (get total-sales current-stats) sale-amount)
      })
    )
    (ok true)
  )
)

;; Public Functions

;; Register as a seller
(define-public (register-seller (name (string-ascii 50)))
  (begin
    (map-set sellers tx-sender {
      name: name,
      reputation-score: u50,
      total-sales: u0,
      sustainability-rating: u50,
      is-verified: false
    })
    (ok true)
  )
)

;; List a new sustainable fashion item
(define-public (list-item 
  (name (string-ascii 50))
  (description (string-ascii 200))
  (price uint)
  (category (string-ascii 20))
  (sustainability-score uint)
  (materials (string-ascii 100))
  (carbon-footprint uint)
  (certifications (list 5 (string-ascii 30))))
  (let ((item-id (var-get next-item-id)))
    (asserts! (> price u0) err-invalid-price)
    (asserts! (and (>= sustainability-score (var-get min-sustainability-score))
                   (<= sustainability-score (var-get max-sustainability-score)))
              err-invalid-sustainability-score)
    (map-set items item-id {
      seller: tx-sender,
      name: name,
      description: description,
      price: price,
      category: category,
      sustainability-score: sustainability-score,
      materials: materials,
      carbon-footprint: carbon-footprint,
      is-available: true,
      created-at: block-height,
      certifications: certifications
    })
    (var-set next-item-id (+ item-id u1))
    (ok item-id)
  )
)

;; Purchase an item
(define-public (purchase-item (item-id uint))
  (let ((item (unwrap! (map-get? items item-id) err-not-found))
        (purchase-id (var-get next-purchase-id))
        (marketplace-fee (calculate-marketplace-fee (get price item)))
        (seller-amount (- (get price item) marketplace-fee)))
    (asserts! (get is-available item) err-item-not-available)
    (asserts! (not (is-eq tx-sender (get seller item))) err-seller-cannot-buy)
    
    ;; Transfer payment to seller
    (try! (stx-transfer? seller-amount tx-sender (get seller item)))
    
    ;; Transfer marketplace fee to contract owner
    (try! (stx-transfer? marketplace-fee tx-sender contract-owner))
    
    ;; Update item availability
    (map-set items item-id (merge item {is-available: false}))
    
    ;; Record purchase
    (map-set purchases purchase-id {
      item-id: item-id,
      buyer: tx-sender,
      seller: (get seller item),
      price: (get price item),
      purchase-date: block-height,
      is-reviewed: false
    })
    
    ;; Update seller stats
    (try! (update-seller-stats (get seller item) (get price item)))
    
    (var-set next-purchase-id (+ purchase-id u1))
    (ok purchase-id)
  )
)

;; Submit a review for a purchased item
(define-public (submit-review 
  (item-id uint)
  (rating uint)
  (sustainability-feedback uint)
  (review-text (string-ascii 200)))
  (let ((review-id (var-get next-review-id)))
    (asserts! (and (>= rating u1) (<= rating u5)) (err u108))
    (asserts! (and (>= sustainability-feedback u1) (<= sustainability-feedback u5)) (err u109))
    
    (map-set reviews review-id {
      item-id: item-id,
      reviewer: tx-sender,
      rating: rating,
      sustainability-feedback: sustainability-feedback,
      review-text: review-text
    })
    
    (var-set next-review-id (+ review-id u1))
    (ok review-id)
  )
)

;; Verify a seller (only contract owner)
(define-public (verify-seller (seller principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (let ((seller-data (unwrap! (map-get? sellers seller) err-not-found)))
      (map-set sellers seller (merge seller-data {is-verified: true}))
      (ok true)
    )
  )
)

;; Update marketplace fee (only contract owner)
(define-public (update-marketplace-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u1000) (err u110)) ;; Max 10%
    (var-set marketplace-fee-percentage new-fee)
    (ok true)
  )
)

;; Update item price (only seller)
(define-public (update-item-price (item-id uint) (new-price uint))
  (let ((item (unwrap! (map-get? items item-id) err-not-found)))
    (asserts! (is-eq tx-sender (get seller item)) err-unauthorized)
    (asserts! (get is-available item) err-item-not-available)
    (asserts! (> new-price u0) err-invalid-price)
    
    (map-set items item-id (merge item {price: new-price}))
    (ok true)
  )
)

;; Remove item from marketplace (only seller)
(define-public (remove-item (item-id uint))
  (let ((item (unwrap! (map-get? items item-id) err-not-found)))
    (asserts! (is-eq tx-sender (get seller item)) err-unauthorized)
    
    (map-set items item-id (merge item {is-available: false}))
    (ok true)
  )
)

;; Read-only Functions

;; Get item details
(define-read-only (get-item (item-id uint))
  (map-get? items item-id)
)

;; Get seller information
(define-read-only (get-seller (seller principal))
  (map-get? sellers seller)
)

;; Get purchase details
(define-read-only (get-purchase (purchase-id uint))
  (map-get? purchases purchase-id)
)

;; Get review details
(define-read-only (get-review (review-id uint))
  (map-get? reviews review-id)
)

;; Get current marketplace fee percentage
(define-read-only (get-marketplace-fee)
  (var-get marketplace-fee-percentage)
)

;; Get next item ID
(define-read-only (get-next-item-id)
  (var-get next-item-id)
)

;; Check if item is available
(define-read-only (is-item-available (item-id uint))
  (match (map-get? items item-id)
    item (get is-available item)
    false
  )
)

;; Get sustainability score range
(define-read-only (get-sustainability-score-range)
  {
    min: (var-get min-sustainability-score),
    max: (var-get max-sustainability-score)
  }
)

;; Calculate total cost including marketplace fee
(define-read-only (calculate-total-cost (item-id uint))
  (match (map-get? items item-id)
    item (ok (get price item))
    err-not-found
  )
)