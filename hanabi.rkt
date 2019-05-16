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
   (state-player-hands-lens)))

(define (state-num-players s)
  (length (state-player-hands s)))

(define (state-hand-size s)
  (let ([n (state-num-players s)])
    (cond [(<= 2 n 3) 5]
          [(<= 4 n 5) 4])))

(define (game-over s)
  (define expected-hand-size (state-hand-size s))
  (or
   (>= 3 (length (state-misplayed s)))
   (eq? (* 5 (length colors)) (length (state-played s)))
   (and (map (lambda (cards) (< (length cards) expected-hand-size))
             (state-player-hands s)))))

;; player-hands is a list of lists of cards
;; hints-remaining is an int in [0, 8]

;; actions
(struct/lens play (card) #:transparent)
(struct/lens discard (card) #:transparent)
(struct/lens hint (number-or-color) #:transparent)

(struct/lens invalid-move (reason) #:transparent)

(define (contains l e) #f ; TODO
  )

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
    (when (game-over s) (done "game is over"))
    (cond [(play? action)
           (let ([c (play-card action)]
                 [h-lens (hand-lens p)])
             (unless (contains (lens-view h-lens s))
               (bad-move "does not have card"))
             (let* ([s (lens-transform h-lens s (remove c))]
                    [s (if (playable? (state-played s))
                           (lens-transform state-played-lens s (add-played c))
                           (lens-transform state-misplayed-lens s (add-played c)))])
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

(define (init-game num-players) #f)
