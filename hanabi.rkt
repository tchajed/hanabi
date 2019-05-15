#lang racket

(require racket/struct)

(define colors '(yellow red blue green rainbow))

(define-struct card (color number)
  ;; TODO: add contracts for color and number
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'card)
      (lambda (obj) (list (card-color obj) (card-number obj)))))])

(define yellow-5 (make-card 'yellow 5))

(define-struct state
  (deck played discarded misplayed player-hands ; disjoint subsets of cards
        hints-remaining
        hints-given ; knowledge about the game
                    ))

;; player-hands is a list of lists of cards
;; hints-remaining is an int in [0, 8]
;; hints-given is a list of hints
;;   hint is a receiving player and a number OR color
