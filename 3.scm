(use-modules (statprof)
             (ice-9 peg)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-42)
             (srfi srfi-71)
             (ice-9 textual-ports)
             (aoc-2024-common))

(define *example-data*
  "987654321111111
811111111111119
234234234234278
818181911112111
")

(define *part-3-data*
  (call-with-input-file "./3.txt" get-string-all))


(define (parse-data dataset)
  (let* ([sls (remove string-null? (string-split dataset #\newline))]
         [ls (map string->list sls)])
    (map (cut map char->number <>) ls)))

;; (define (find-joltage js)
;;   (let* ([lj (length js)]
;;          [max-j (+ (* 10 (first js)) (second js))])
;;     (do-ec (:list j1 (index i1) js)
;;            (do-ec (:range i2 (1+ i1) lj)
;;                   (:let this-j (+ (* 10 j1) (list-ref js i2)))
;;                   (if (< max-j this-j))
;;                   (set! max-j this-j)))
;;     max-j))

(define (find-joltage acc js n)
  (if (or (zero? n) (not js))
      acc
      (let* ([rest-j (drop-right js (1- n))]
             [max-j (apply max rest-j)]
             [i-max (list-index (cut = max-j <>) js)])
        (find-joltage (+ (* 10 acc) max-j) (drop js (1+ i-max)) (1- n)))))


(define (solve-3 data)
  (statprof
   (lambda ()
     (let* ([js (parse-data data)])
       (values (sum-ec (:list j js)
                       (find-joltage 0 j 2))
               (sum-ec (:list j js)
                       (find-joltage 0 j 12)))))))
