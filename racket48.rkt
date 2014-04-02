#lang typed/racket

(define-type (Grid T) (Listof (Listof T)))
(struct: Coordinate {[x : Integer]
                     [y : Integer]}
         #:transparent)

(module+ main
  (display "Hello World\n"))
