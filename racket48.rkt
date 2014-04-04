#lang typed/racket

(define-type (Grid T) (HashTable Coordinate T))
(define-type Tile (U 0 2 4 8 16 32 64 128 256 512 1024 2048))
(define-type Board (Grid Tile))
(struct: Coordinate {[x : Integer]
                     [y : Integer]}
         #:transparent)
(define-predicate tile? Tile)

(: empty-board (-> Board))
(define (empty-board) (hash))

(: board-get (Board Integer Integer -> Tile))
(define (board-get board x y)
  (hash-ref board (Coordinate x y) (λ () 0)))

(module+ main
  (display "Hello World\n"))
