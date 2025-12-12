(use-modules (statprof)
             (ice-9 peg)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-42)
             (srfi srfi-71)
             (ice-9 textual-ports)
             (ice-9 string-fun)
             (aoc-2024-common))

(define *example-data*
  "0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2
")

(define *part-12-data*
  (call-with-input-file "./12.txt" get-string-all))

(define (parse-boards boards)
  (list-ec (:list board boards)
           (:let lr (string-split board #\:))
           (:let grid (map string->number (string-split (first lr) #\x)))
           (:let number-strs (remove string-null? (string-split (second lr) #\space)))
           (:let numbers (map string->number number-strs))
           (list grid numbers)))

(define (parse-shapes shapes)
  (list-ec (:list shape shapes)
           (:let ns (string-split shape #\:))
           (:let n (string->number (first ns)))
           (:let grid-shape (create-grid-dict (second ns)))
           (:let grid-keys (hash-fold (lambda (k v p)
                                        (if (char=? #\# v)
                                            (cons k p)
                                            p)) '() grid-shape))
           (cons n grid-keys)))

(define (parse-data data)
  (let* ([parsed (string-split (string-replace-substring data "\n\n" "?") #\?)]
         [board-str shapes (car+cdr (reverse parsed))]
         [boards (parse-boards (remove string-null? (string-split board-str #\newline)))]
         [shape-alist (parse-shapes shapes)])
    (values boards shape-alist)))

;; (define (can-place? grid shape pos rows cols)
;;   (every?-ec (:let pos shape)
;;              ()))

;; (define (heptomino-tiling p rows cols grid shapes failed-shapes)
;;   (let* ([r c (euclidean/ p cols)]
;;          [i (make-rectangular r c)])
;;     (cond
;;      [(null? shapes)
;;       (null? failed-shapes)]
;;      [(>= (* rows cols) p)
;;       #f]
;;      [(not (hash-ref grid i))
;;       (heptomino-tiling (1+ p) rows cols grid shapes failed-shapes)]
;;      [else
;;       (let* ([shape new-shapes (car+cdr shapes)])
;;         (or (first-ec #f (:list pos os)
;;                       (:let adjusted-pos (try-pos pos))
;;                       (begin
;;                         (hash-set! grid adjusted-pos #t)
;;                         (heptomino-tiling (1+ p) rows cols grid)))))])))

(define (populate-shapes shape-list shape-alist)
  (append-ec (:list c (index i) shape-list)
             (not (zero? c))
             (make-list c (assoc-ref shape-alist i))))

(define (solve-12 data)
  (statprof
   (lambda ()
     (let ([boards shape-alist (parse-data data)])
       (length (list-ec (:list board boards)
                        (:let board-shape (first board))
                        (:let area (apply * board-shape))
                        (:let shapes-to-fit (populate-shapes (second board) shape-alist))
                        (if (<= (apply + (map length shapes-to-fit)) area))
                        #t))))))

