;; cryptp-card_contract
;; A comprehensive smart contract for managing crypto cards with ownership, metadata, trading,
;; marketplace functionality, rarity tiers, card upgrades, and rewards system


;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-not-authorized (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-card-locked (err u105))
(define-constant err-invalid-rarity (err u106))
(define-constant err-upgrade-requirements (err u107))
(define-constant err-cooldown-active (err u108))
(define-constant err-marketplace-disabled (err u109))


;; rarity tiers
(define-constant rarity-common u1)
(define-constant rarity-uncommon u2)
(define-constant rarity-rare u3)
(define-constant rarity-epic u4)
(define-constant rarity-legendary u5)
(define-constant rarity-mythic u6)


;; data maps and vars
(define-non-fungible-token crypto-card uint)


(define-map card-details uint
 {
   name: (string-ascii 64),
   description: (string-ascii 256),
   image-uri: (string-utf8 256),
   rarity: uint,
   attributes: (list 10 {trait: (string-ascii 32), value: (string-ascii 32)}),
   created-at: uint,
   level: uint,
   experience: uint,
   edition: uint,
   series: (string-ascii 32)
 }
)


(define-map card-ownership uint principal)
(define-map card-status uint
 {
   locked: bool,
   cooldown-until: uint,
   last-action: uint,
   upgrade-count: uint
 }
)


(define-map marketplace-listings uint
 {
   seller: principal,
   price: uint,
   listed-at: uint,
   expires-at: uint
 }
)


(define-map user-stats principal
 {
   cards-owned: uint,
   cards-created: uint,
   cards-sold: uint,
   cards-purchased: uint,
   total-spent: uint,
   total-earned: uint
 }
)


(define-map rarity-requirements uint
 {
   base-price: uint,
   max-supply: uint,
   current-supply: uint
 }
)


(define-map series-info (string-ascii 32)
 {
   name: (string-ascii 64),
   description: (string-ascii 256),
   creator: principal,
   created-at: uint,
   card-count: uint,
   is-limited: bool,
   max-cards: uint
 }
)


(define-data-var last-card-id uint u0)
(define-data-var marketplace-fee uint u25) ;; 2.5% fee (in basis points)
(define-data-var marketplace-enabled bool true)
(define-data-var total-cards-created uint u0)
(define-data-var total-cards-traded uint u0)
(define-data-var total-marketplace-volume uint u0)


;; private functions
(define-private (is-owner (card-id uint))
 (let ((owner (unwrap! (map-get? card-ownership card-id) false)))
   (is-eq tx-sender owner)
 )
)


(define-private (is-contract-owner)
 (is-eq tx-sender contract-owner)
)


(define-private (calculate-fee (amount uint))
 (/ (* amount (var-get marketplace-fee)) u1000)
)


(define-private (update-user-stat (user principal) (stat-key (string-ascii 20)) (value uint))
 (let (
   (current-stats (default-to
     {
       cards-owned: u0,
       cards-created: u0,
       cards-sold: u0,
       cards-purchased: u0,
       total-spent: u0,
       total-earned: u0
     }
     (map-get? user-stats user)))
   (updated-stats
     (if (is-eq stat-key "cards-owned")
       (merge current-stats {cards-owned: value})
       (if (is-eq stat-key "cards-created")
         (merge current-stats {cards-created: value})
         (if (is-eq stat-key "cards-sold")
           (merge current-stats {cards-sold: value})
           (if (is-eq stat-key "cards-purchased")
             (merge current-stats {cards-purchased: value})
             (if (is-eq stat-key "total-spent")
               (merge current-stats {total-spent: value})
               (if (is-eq stat-key "total-earned")
                 (merge current-stats {total-earned: value})
                 current-stats
               )
             )
           )
         )
       )
     )
   )
 )
   (map-set user-stats user updated-stats)
 )
)


(define-private (increment-user-stat (user principal) (stat-key (string-ascii 20)))
 (let (
   (current-stats (default-to
     {
       cards-owned: u0,
       cards-created: u0,
       cards-sold: u0,
       cards-purchased: u0,
       total-spent: u0,
       total-earned: u0
     }
     (map-get? user-stats user)))
   (updated-stats
     (if (is-eq stat-key "cards-owned")
       (merge current-stats {cards-owned: (+ (get cards-owned current-stats) u1)})
       (if (is-eq stat-key "cards-created")
         (merge current-stats {cards-created: (+ (get cards-created current-stats) u1)})
         (if (is-eq stat-key "cards-sold")
           (merge current-stats {cards-sold: (+ (get cards-sold current-stats) u1)})
           (if (is-eq stat-key "cards-purchased")
             (merge current-stats {cards-purchased: (+ (get cards-purchased current-stats) u1)})
             current-stats
           )
         )
       )
     )
   )
 )
   (map-set user-stats user updated-stats)
 )
)


(define-private (is-valid-rarity (rarity uint))
 (and (>= rarity rarity-common) (<= rarity rarity-mythic))
)


(define-private (check-card-not-locked (card-id uint))
 (let (
   (status (default-to {locked: false, cooldown-until: u0, last-action: u0, upgrade-count: u0}
            (map-get? card-status card-id)))
 )
   (not (get locked status))
 )
)


;; public functions
(define-public (create-card (name (string-ascii 64))
                          (description (string-ascii 256))
                          (image-uri (string-utf8 256))
                          (rarity uint)
                          (attributes (list 10 {trait: (string-ascii 32), value: (string-ascii 32)}))
                          (series (string-ascii 32)))
 (let
   (
     (new-id (+ (var-get last-card-id) u1))
   )
   (asserts! (is-valid-rarity rarity) err-invalid-rarity)
   (try! (nft-mint? crypto-card new-id tx-sender))
  
   (map-set card-details new-id
     {
       name: name,
       description: description,
       image-uri: image-uri,
       rarity: rarity,
       attributes: attributes,
       created-at: block-height,
       level: u1,
       experience: u0,
       edition: u1,
       series: series
     }
   )
  
   (map-set card-ownership new-id tx-sender)
   (map-set card-status new-id
     {
       locked: false,
       cooldown-until: u0,
       last-action: block-height,
       upgrade-count: u0
     }
   )
  
   (var-set last-card-id new-id)
   (var-set total-cards-created (+ (var-get total-cards-created) u1))
  
   (increment-user-stat tx-sender "cards-owned")
   (increment-user-stat tx-sender "cards-created")
  
   (ok new-id)
 )
)


(define-public (transfer-card (card-id uint) (recipient principal))
 (begin
   (asserts! (is-owner card-id) err-not-authorized)
   (asserts! (check-card-not-locked card-id) err-card-locked)
  
   (try! (nft-transfer? crypto-card card-id tx-sender recipient))
   (map-set card-ownership card-id recipient)
  
   (increment-user-stat recipient "cards-owned")
  
   (ok true)
 )
)


(define-public (list-card-for-sale (card-id uint) (price uint) (expires-in uint))
 (let (
   (expires-at (+ block-height expires-in))
 )
   (asserts! (var-get marketplace-enabled) err-marketplace-disabled)
   (asserts! (is-owner card-id) err-not-authorized)
   (asserts! (check-card-not-locked card-id) err-card-locked)
  
   (map-set marketplace-listings card-id
     {
       seller: tx-sender,
       price: price,
       listed-at: block-height,
       expires-at: expires-at
     }
   )
  
   (map-set card-status card-id
     (merge
       (default-to
         {locked: false, cooldown-until: u0, last-action: block-height, upgrade-count: u0}
         (map-get? card-status card-id)
       )
       {locked: true}
     )
   )
  
   (ok true)
 )
)


(define-public (cancel-listing (card-id uint))
 (let (
   (listing (unwrap! (map-get? marketplace-listings card-id) err-not-found))
 )
   (asserts! (is-eq (get seller listing) tx-sender) err-not-authorized)
  
   (map-delete marketplace-listings card-id)
   (map-set card-status card-id
     (merge
       (default-to
         {locked: true, cooldown-until: u0, last-action: block-height, upgrade-count: u0}
         (map-get? card-status card-id)
       )
       {locked: false}
     )
   )
  
   (ok true)
 )
)


(define-public (buy-card (card-id uint))
 (let (
   (listing (unwrap! (map-get? marketplace-listings card-id) err-not-found))
   (seller (get seller listing))
   (price (get price listing))
   (fee (calculate-fee price))
   (seller-amount (- price fee))
 )
   (asserts! (var-get marketplace-enabled) err-marketplace-disabled)
   (asserts! (<= block-height (get expires-at listing)) err-not-found)
  
   ;; Transfer STX from buyer to seller and pay fee
   (try! (stx-transfer? price tx-sender seller))
  
   ;; Transfer NFT ownership
   (try! (nft-transfer? crypto-card card-id seller tx-sender))
   (map-set card-ownership card-id tx-sender)
  
   ;; Update card status
   (map-set card-status card-id
     (merge
       (default-to
         {locked: true, cooldown-until: u0, last-action: block-height, upgrade-count: u0}
         (map-get? card-status card-id)
       )
       {locked: false, last-action: block-height}
     )
   )
  
   ;; Remove listing
   (map-delete marketplace-listings card-id)
  
   ;; Update stats
   (increment-user-stat tx-sender "cards-owned")
   (increment-user-stat tx-sender "cards-purchased")
   (increment-user-stat seller "cards-sold")
   (var-set total-)
)

(define-public (add-card-experience (card-id uint) (amount uint))
 (let (
   (card (unwrap! (map-get? card-details card-id) err-not-found))
   (current-exp (get experience card))
   (new-exp (+ current-exp amount))
 )
   (asserts! (is-owner card-id) err-not-authorized)
   (asserts! (check-card-not-locked card-id) err-card-locked)
  
   (map-set card-details card-id (merge card {experience: new-exp}))
  
   (ok new-exp)
 )
)

(define-public (upgrade-card-level (card-id uint))
 (let (
   (card (unwrap! (map-get? card-details card-id) err-not-found))
   (current-level (get level card))
   (current-exp (get experience card))
   (required-exp (* current-level u100))
   (status (default-to
             {locked: false, cooldown-until: u0, last-action: u0, upgrade-count: u0}
             (map-get? card-status card-id)))
 )
   (asserts! (is-owner card-id) err-not-authorized)
   (asserts! (check-card-not-locked card-id) err-card-locked)
   (asserts! (>= current-exp required-exp) err-upgrade-requirements)
  
   (map-set card-details card-id
     (merge card
       {
         level: (+ current-level u1),
         experience: (- current-exp required-exp)
       }
     )
   )
  
   (map-set card-status card-id
     (merge status
       {
         upgrade-count: (+ (get upgrade-count status) u1),
         last-action: block-height
       }
     )
   )
  
   (ok (+ current-level u1))
 )
)

;; read-only functions
(define-read-only (get-card-details (card-id uint))
 (ok (unwrap! (map-get? card-details card-id) err-not-found))
)


(define-read-only (get-card-owner (card-id uint))
 (ok (unwrap! (map-get? card-ownership card-id) err-not-found))
)

(define-read-only (get-card-status (card-id uint))
 (ok (default-to
   {locked: false, cooldown-until: u0, last-action: u0, upgrade-count: u0}
   (map-get? card-status card-id)))
)

(define-read-only (get-user-stats (user principal))
 (ok (default-to
   {
     cards-owned: u0,
     cards-created: u0,
     cards-sold: u0,
     cards-purchased: u0,
     total-spent: u0,
     total-earned: u0
   }
   (map-get? user-stats user)))
)


(define-read-only (get-last-card-id)
 (ok (var-get last-card-id))
)
)
