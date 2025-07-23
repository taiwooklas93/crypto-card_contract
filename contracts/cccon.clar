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
