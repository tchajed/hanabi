#lang racket

(require racket/struct)
(require lens)

(define colors '(yellow red blue green))

(struct/lens card (color number) #:transparent
             ;; TODO: add contracts for color and number
             )

(define empty-card-set '())
(define (playable? card-set c)
  (define (same-color? c2) (eq? (card-color c) (card-color c2)))
  (define highest-played (apply max 0 . (filter same-color? card-set)))
  (eq? (card-number c) (+ 1 highest-played)))
(define (add-played c card-set) (cons c card-set))
(define (num-played card-set) (length card-set))

(struct/lens state
             (deck played discarded misplayed player-hands ; disjoint subsets of cards
                   hints-remaining
                   current-player) #:transparent)

(define (hand-lens player)
  (lens-compose
   (list-ref-lens player)
   state-player-hands-lens))

(define (state-num-players s)
  (length (state-player-hands s)))

(define (hand-size-for-players num-players)
  (cond [(<= 2 num-players 3) 5]
        [(<= 4 num-players 5) 4]))

(define (state-hand-size s)
  (hand-size-for-players (state-num-players s)))

(define (game-over? s)
  (define expected-hand-size (state-hand-size s))
  (or
   (>= (length (state-misplayed s)) 3)
   (eq? (* 5 (length colors)) (num-played (state-played s)))
   (andmap (lambda (cards) (< (length cards) expected-hand-size))
           (state-player-hands s))))

;; player-hands is a list of lists of cards
;; hints-remaining is an int in [0, 8]

;; actions
(struct/lens play (card-idx) #:transparent)
(struct/lens discard (card-idx) #:transparent)
(struct/lens hint (number-or-color) #:transparent)

(struct/lens invalid-move (reason) #:transparent)
;; is card c playable in state s?
(define (state-playable? s c)
  (playable? (state-played s) c))

(define (make-move s p action)
  (let/cc return
    (define (bad-move reason) (return (invalid-move reason)))
    (define (done s) (return
                      (lens-set state-current-player-lens s
                                (modulo (+ p 1) (state-num-players s)))))
    (unless (eq? (state-current-player s) p)
      (bad-move "out-of-turn"))
    (when (game-over? s) (done "game is over"))
    (cond [(play? action)
           (let ([c-idx (play-card-idx action)]
                 [h-lens (hand-lens p)])
             (when (>= c-idx (length (lens-view h-lens s)))
               (bad-move "play index out-of-bounds"))
             (define c (list-ref (lens-view h-lens s) c-idx))
             (let* ([s (lens-transform h-lens s (curry remove c))]
                    [s (if (state-playable? s c)
                           (lens-transform state-played-lens s (curry add-played c))
                           (lens-transform state-misplayed-lens s (curry add-played c)))])
               s))]
          [(discard? action)
           ;; TODO: merge with play
           (let ([c-idx (discard-card-idx action)]
                 [h-lens (hand-lens p)])
             (when (>= c-idx (length (lens-view h-lens s)))
               (bad-move "discard index out-of-bounds"))
             (when (eq? 8 (state-hints-remaining s))
               (bad-move "cannot discard with full hints"))
             (define c (list-ref (lens-view h-lens s) c-idx))
             (let* ([s (lens-transform h-lens s (curry remove c))]
                    [s (lens-transform state-discarded-lens s (curry add-played c))]
                    [s (lens-transform state-hints-remaining-lens s (curry + 1))])
               s))]
          )))

(define (init-deck)
  (define (flatmap f . ls) (apply append (apply map f ls)))
  (define (n-cards n color number) (build-list n (lambda (_) (card color number))))
  (flatmap (lambda (c) (flatmap (lambda (number n) (n-cards n c number))
                                '(1 2 3 4 5)
                                '(3 2 2 2 1)
                                )) colors))

(define (random-deck) (shuffle (init-deck)))

(define (init-game num-players deck)
  (let*-values ([(hand-size) (hand-size-for-players num-players)]
                [(deck player-hands)
                 (for/fold ([deck deck] [hands '()])
                           ([_ (in-range num-players)])
                   (let-values ([(hand deck) (split-at deck hand-size)])
                     (values deck (cons hand hands))
                     ))]
                )
    (state deck
           ;; played discarded misplayed
           empty-card-set empty-card-set empty-card-set
           player-hands 8 0)))
