#lang racket

(require racket/struct)
(require lens)

(define colors '(yellow red blue green))
(define (color? c) (member c colors))

(struct/lens card (color number) #:transparent
             ;; TODO: add contracts for color and number
             )

;; Lists as a data structure for a set of cards
(define empty-card-set '())
(define (playable? card-set c)
  (define (same-color? c2) (eq? (card-color c) (card-color c2)))
  (define color-stack (filter-map
                       (λ (c) (if (same-color? c) (card-number c) #f))
                       card-set))
  (define highest-played (apply max 0 color-stack))
  (eq? (card-number c) (+ 1 highest-played)))
(define (add-played c card-set) (cons c card-set))
(define (num-played card-set) (length card-set))

(module+ test
  (require rackunit)
  (check-true  (playable? empty-card-set (card 'yellow 1)))
  (check-true  (playable? empty-card-set (card 'blue 1)))
  (check-false (playable? empty-card-set (card 'blue 2)))
  (check-false (playable? empty-card-set (card 'blue 3)))

  (define cards
    (foldr add-played empty-card-set
           (list
            (card 'yellow 1)
            (card 'blue 1)
            (card 'red 1) (card 'yellow 2)
            (card 'red 2))))
  (define playable-cards
    (list
     (card 'yellow 3)
     (card 'blue 2)
     (card 'green 1)
     (card 'red 3)))
  (define all-cards (for/list ([color (in-list colors)]
                               [number (in-list '(1 2 3 4 5))])
                      (card color number)))
  (for ([c (in-list all-cards)])
    (check-eq? (playable? cards c) (member c playable-cards))))

(struct/lens state
             (deck played discarded misplayed player-hands ; disjoint subsets of cards
                   hints-remaining
                   current-player) #:transparent)

;; deck is a list of cards (in draw order)
;; played, discarded, misplayed, and player-hands are card-sets.
;;  They are represented with a list but access goes through the API above.
;; player-hands is a list of lists of cards
;; hints-remaining is an int in [0, 8]
;; current-player is a player index


(define (init-deck)
  (define (flatmap f . ls) (apply append (apply map f ls)))
  (define (n-cards n color number) (build-list n (lambda (_) (card color number))))
  (flatmap (lambda (c) (flatmap (lambda (number n) (n-cards n c number))
                                '(1 2 3 4 5)
                                '(3 2 2 2 1)
                                )) colors))

(define (hand-size-for-players num-players)
  (cond [(<= 2 num-players 3) 5]
        [(<= 4 num-players 5) 4]))

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

(module+ test
  (random-seed 0)
  (for ([num-players (in-list '(2 3 4 5))])
    (check-false (game-over? (init-game num-players (random-deck)))))
  )
(define (hand-lens player)
  (lens-compose
   (list-ref-lens player)
   state-player-hands-lens))

(define (state-hand-size s)
  (hand-size-for-players (state-num-players s)))

(define (state-num-players s)
  (length (state-player-hands s)))

;; is card c playable in state s?
(define (state-playable? s c)
  (playable? (state-played s) c))

(define (game-over? s)
  (define expected-hand-size (state-hand-size s))
  (or
   (>= (length (state-misplayed s)) 3)
   (eq? (* 5 (length colors)) (num-played (state-played s)))
   (andmap (lambda (cards) (< (length cards) expected-hand-size))
           (state-player-hands s))))

;; actions
(struct play (card-idx) #:transparent)
(struct discard (card-idx) #:transparent)
(struct hint (player number-or-color) #:transparent)

(struct invalid-move (reason) #:transparent)
(struct hint-result (hint matches) #:transparent)

(define (matching-indices hand number-or-color)
  (indexes-where hand (lambda (c)
                        (eq? number-or-color
                             (cond [(color? number-or-color) (card-color c)]
                                   [else (card-number c)])))))

(define (state-deck-draw s)
  (let ([deck (state-deck s)])
    (values (cdr deck) (car deck))))

(define (make-move s p action)
  (let/cc return
    (define (bad-move reason) (return (invalid-move reason)))
    (define (done s info)
      (return
       (list (lens-set state-current-player-lens s
                       (modulo (+ p 1) (state-num-players s)))) info))
    (unless (eq? (state-current-player s) p)
      (bad-move "out-of-turn"))
    (when (game-over? s) (done "game is over"))
    (define h-lens (hand-lens p))
    (match action
      [(play c-idx)
       (let ([h-lens (hand-lens p)])
         (when (>= c-idx (length (lens-view h-lens s)))
           (bad-move "play index out-of-bounds"))
         (define c (list-ref (lens-view h-lens s) c-idx))
         (let* ([s (lens-transform h-lens s (curry remove c))]
                [s (if (state-playable? s c)
                       (lens-transform state-played-lens s (curry add-played c))
                       (lens-transform state-misplayed-lens s (curry add-played c)))])
           (done s #f)))]
      [(discard c-idx)
       ;; TODO: merge with play
       (when (>= c-idx (length (lens-view h-lens s)))
         (bad-move "discard index out-of-bounds"))
       (when (eq? 8 (state-hints-remaining s))
         (bad-move "cannot discard with full hints"))
       (define c (list-ref (lens-view h-lens s) c-idx))
       (let* ([s (lens-transform h-lens s (curry remove c))]
              [s (lens-transform state-discarded-lens s (curry add-played c))]
              [s (lens-transform state-hints-remaining-lens s (curry + 1))])
         (done s #f))]
      [(struct* hint ((player hinted-player) (number-or-color hint-val)))
       (let* ([target-hand (lens-view (hand-lens hinted-player) s)]
              [indices (matching-indices target-hand hint-val)])
         (when (null? indices) (bad-move "hint that indicates no cards"))
         (done s (hint-result hint-val indices)))])))

(provide random-deck init-game make-move
         ;; types
         (struct-out state) ; TODO: don't export private information
         (struct-out card)
         (struct-out play) (struct-out discard) (struct-out hint)
         (struct-out hint-result)
         (struct-out invalid-move))
