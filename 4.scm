(use-modules (statprof)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-42)
             (srfi srfi-71)
             (ice-9 textual-ports)
             (aoc-2024-common))

(define *example-data*
  "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
")

(define *part-4-data*
  (call-with-input-file "./4.txt" get-string-all))

(define (count-safe-rolls grid-map)
  (hash-fold (lambda (k v acc)
               (if (char=? v #\.)
                   acc
                   (let* ([positions (map (cut + k <>) grid-directions-with-diagonals)]
                          [chars (filter-map (cut hash-ref grid-map <>) positions)]
                          [c (count (cut char=? <> #\@) chars)])
                     (if (< c 4) (cons k acc) acc))))
             '() grid-map))

(define (repeated-safe-rolls grid-map acc)
  (let* ([safe-pos (count-safe-rolls grid-map)]
         [n-safe-pos (length safe-pos)])
    (if (zero? n-safe-pos)
        acc
        (begin
          (for-each (cut hash-set! grid-map <> #\.) safe-pos)
          (repeated-safe-rolls grid-map (+ acc n-safe-pos))))))

(define (solve-4 data)
  (statprof
   (lambda ()
     (let ([grid-map (create-grid-dict data)])
       (values (length (count-safe-rolls grid-map))
               (repeated-safe-rolls grid-map 0))))))

