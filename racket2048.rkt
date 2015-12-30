#lang racket/gui

(require math/matrix
         threading
         pict)

; Board : (List (List (or Integer #f) ...) ...)
; List of Lists
; '((#f #f #f #f)
;   (#f #f #f #f)
;   (#f #f #f #f)
;   (#f #f #f #f))

(define empty-board
  '((#f #f #f #f)
    (#f #f #f #f)
    (#f #f #f #f)
    (#f #f #f #f)))

; init board : -> board
(define (init-board)
  (~> empty-board
      spawn-random-tile
      spawn-random-tile))

; step : board direction -> (U board bool)
(define (step board [direction 'left])
  (let/ec return
    (define board* (collapse-board board direction))
    (when (and (check-full? board*) (no-moves? board*))
      (return #f))
    (when (equal? board board*)
      (return board))
    (define board** (spawn-random-tile board*))
    (cond [(check-win? board**) #t]
          [else board**])))

; check-win : board -> bool
(define (check-win? board)
  (for/fold ([won? #f])
            ([e (in-list (append* board))])
    (or won? (equal? e 2048))))

; check-full ; board -> bool
(define (check-full? board)
  (for/fold ([full? #t])
            ([e (in-list (append* board))])
    (and full? e)))

; no-moves ; board -> bool
(define (no-moves? board)
  (define can-move? #f)
  (for ([i (in-range (length board))])
    (for ([j (in-range (length (list-ref board 0)))])
      (define piece (get-piece board i j))
      (when (or (equal? piece (get-piece (- i 1) j))
                (equal? piece (get-piece (+ i 1) j))
                (equal? piece (get-piece i (- j 1)))
                (equal? piece (get-piece i (+ j 1))))
        (set! can-move? #t))))
  can-move?)

; get-piece : board int int -> int
(define (get-piece board x y)
  (and (x . >= . 0)
       (x . < . (length board))
       (y . >= . 0)
       (y . < . (length (list-ref board 0)))
       (~> board
           (list-ref y)
           (list-ref x))))
    
; spawn-random-tile : board -> board
(define (spawn-random-tile board)
  (let loop ([x (random 4)]
             [y (random 4)])
    (define element (* 2 (+ 1 (random 2)))) ;; TODO sometimes should be 4
    (define row (list-ref board y))
    (define pos (list-ref row x))
    (if pos
        (loop (random 4) (random 4)) ;; TODO change to be a traversal
        (append (take board y)
                (list (append (take row x)
                              (list element)
                              (drop row (+ x 1))))
                (drop board (+ y 1))))))

; collapse-board : board direction -> board
(define (collapse-board board [direction 'left])
  (match direction
    ['left (for/list ([row (in-list board)])
             (collapse row))]
    ['right (map reverse (collapse-board (map reverse board) 'left))]
    ['up (~> board
             list*->matrix
             matrix-transpose
             matrix->list*
             (collapse-board 'left)
             list*->matrix
             matrix-transpose
             matrix->list*)]
    ['down (~> board
               list*->matrix
               matrix-transpose
               matrix->list*
               (map reverse _)
               (collapse-board 'left) 
               (map reverse _)
               list*->matrix
               matrix-transpose
               matrix->list*)]
    [else (error "Not a direction")]))

; collapse : row -> row
; Collapses the row as if the left key is pressed
(define (collapse row)
  (define-values (ret _)
    (for/fold ([r row]
               [c #t])
              ([i (in-range (length row))])
      (collapse-step r i c)))
  ret)
  
;Collapse a piece in the row : row, piece -> row
(define (collapse-step row piece [collapse? #t])
  (cond [(equal? piece 0) (values row #t)]
        [(not (list-ref row (- piece 1)))
         (collapse-step
          (~> row
              (list-set (- piece 1) (list-ref row piece))
              (list-set piece #f))
          (- piece 1)
          collapse?)]
        [(and collapse?
              (equal? (list-ref row piece) (list-ref row (- piece 1))))
         (values (~> row
                     (list-set (- piece 1)
                               (+ (list-ref row (- piece 1))
                                  (list-ref row piece)))
                     (list-set piece #f))
                 #f)]
        [else (values row #t)]))

(define (render-board board)
  (cc-superimpose (filled-rectangle 230 230)
                  (apply vc-append 5 (map render-row empty-board))
                  (apply vc-append 5 (map render-row board))))

(define (render-row row)
  (apply hc-append 5 (map render-piece row)))

(define (render-piece piece)
  (define color
    (match piece
      [2 "GreenYellow"]
      [4 "Chartreuse"]
      [8 "LawnGreen"]
      [16 "Green"]
      [32 "YellowGreen"]
      [64 "OliveDrab"]
      [128 "Dark Olive Green"]
      [256 "Forest Green"]
      [512 "DarkGreen"]
      [1024 "Pink"]
      [2048 "Red"]
      [else "White"]))
  (cc-superimpose (filled-rectangle 50 50 #:color color)
                  (if (number? piece)
                      (text (number->string piece) null 24)
                      (blank))))

(define 2048-canvas%
  (class canvas%
    (field [board (init-board)])
    (define/public (get-board)
      board)
    (define/override (on-char event)
      (unless (or (equal? board #t)
                  (equal? board #f))
        (set! board
              (match (send event get-key-code)
                ['left (step board 'left)]
                ['right (step board 'right)]
                ['up (step board 'up)]
                ['down (step board 'down)]
                [else board]))
        (send this refresh-now)))
    (super-new)))

(define frame (new frame%
                   [label "2048"]
                   [width 228]
                   [height 250]))

(new 2048-canvas%
     [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (cond [(equal? (send canvas get-board) #t)
               (send dc set-scale 3 3)
               (send dc set-text-foreground "blue")
               (send dc draw-text "You Win!" 0 0)]
              [(equal? (send canvas get-board) #f)
               (equal? (send canvas get-board) #t)
               (send dc set-scale 3 3)
               (send dc set-text-foreground "red")
               (send dc draw-text "You Lose!" 0 0)]
              [else
               (draw-pict (render-board (send canvas get-board)) dc 0 0)]))])
  
(module+ main
  (send frame show #t))
