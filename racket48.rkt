#lang typed/racket

(define-type (Grid T) (HashTable Coordinate T))
(define-type Tile (U 0 2 4 8 16 32 64 128 256 512 1024 2048))
(define-type Board (Grid Tile))
(define-type Direction (U 'up 'down 'left 'right))
(struct: Coordinate {[x : Integer]
                     [y : Integer]}
         #:transparent)
(define-predicate tile? Tile)
(define-predicate direction? Direction)

(: empty-board (-> Board))
(define (empty-board) (hash))

(: board-get (Board Integer Integer -> Tile))
(define (board-get board x y)
  (hash-ref board (Coordinate x y) (λ () 0)))

(: board-set (Board Integer Integer Tile -> Board))
(define (board-set board x y tile)
  (hash-set board (Coordinate x y) tile))

;; TODO
(: slide-tiles (Board Direction -> Board))
(define (slide-tiles board direction)
  board)

;; TODO
(: place-tile (Board -> Board))
(define (place-tile board)
  board)

;; TODO
(: step (Board -> Board))
(define (step board)
  board)

;; TODO
(: run (Board -> Board))
(define (run board)
  board)

(: play-2048 (-> Board))
(define (play-2048)
  (define board (empty-board))
  (run board))

(module+ main
  (play-2048))
