#lang racket

(require racket/struct)
(require lens)

(define colors '(yellow red blue green))

(struct/lens card (color number) #:transparent
  ;; TODO: add contracts for color and number
  )

(define yellow-5 (card 'yellow 5))

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
   (eq? (* 5 (length colors)) (length (state-played s)))
   (andmap (lambda (cards) (< (length cards) expected-hand-size))
           (state-player-hands s))))

;; player-hands is a list of lists of cards
;; hints-remaining is an int in [0, 8]

;; actions
(struct/lens play (card) #:transparent)
(struct/lens discard (card) #:transparent)
(struct/lens hint (number-or-color) #:transparent)

(struct/lens invalid-move (reason) #:transparent)

;; is card c playable in state s?
(define (playable? s c)
  (let ([played (state-played s)])
    #t ; TODO
    ))

(define (add-played c card-set) (cons c card-set))

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
           (let ([c (play-card action)]
                 [h-lens (hand-lens p)])
             (unless (member c (lens-view h-lens s))
               (bad-move "does not have card"))
             (let* ([s (lens-transform h-lens s (curry remove c))]
                    [s (if (playable? s c)
                           (lens-transform state-played-lens s (curry add-played c))
                           (lens-transform state-misplayed-lens s (curry add-played c)))])
               s))
           ])
    ))

(define (init-deck)
  (define (flatmap f . ls) (apply append (apply map f ls)))
  (define (n-cards n color number) (build-list n (lambda (_) (card color number))))
  (flatmap (lambda (c) (flatmap (lambda (number n) (n-cards n c number))
                                '(1 2 3 4 5)
                                '(3 2 2 2 1)
                                )) colors))

(define (init-game num-players)
  (let*-values ([(hand-size) (hand-size-for-players num-players)]
                [(deck player-hands)
                 (for/fold ([deck (shuffle (init-deck))] [hands '()])
                           ([_ (in-range num-players)])
                   (let-values ([(hand deck) (split-at deck hand-size)])
                     (values deck (cons hand hands))
                     ))]
                )
    (state deck '() '() '() player-hands 8 0)))
