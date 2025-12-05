(use-modules (statprof)
             (ice-9 peg)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-42)
             (srfi srfi-71)
             (ice-9 string-fun)
             (ice-9 textual-ports)
             (aoc-2024-common))

(define *example-data*
  "3-5
10-14
16-20
12-18

1
5
8
11
17
32
")

(define *part-5-data*
  (call-with-input-file "./5.txt" get-string-all))

(define (parse-data dataset)
  (let* ([sls (string-split (string-replace-substring dataset "\n\n" "*") #\*)]
         [ranges (list-ec (:list r (string-split (first sls) #\newline))
                          (:let s-e (string-split r #\-))
                          (map string->number s-e))]
         [items (map string->number (remove string-null?
                                            (string-split (second sls) #\newline)))])
    (values ranges items)))

(define (sort-range-list ranges)
  (sort ranges (lambda (a b) (< (first a) (first b)))))

(define (n-fresh-items items ranges)
  (sum-ec (:list i items)
          (if (any?-ec (:list r ranges)
                       (<= (first r) i (second r))))
          1))

(define (fresh-count range-list acc)
  (if (null? range-list)
      acc
      (match-let ([((s e) . r) range-list])
        (fresh-count r (+ acc 1 (- e s))))))

(define (merge-ranges ranges prev-start prev-end acc)
  (if (null? ranges)
      (reverse acc)
      (match-let ([((s e) . r) ranges])
        (if (<= s prev-end)
            (merge-ranges r
                          prev-start
                          (max e prev-end)
                          (cons (list prev-start (max e prev-end)) (cdr acc)))
            (merge-ranges r s e (cons (list s e) acc))))))

(define (solve-5 data)
  (statprof
   (lambda ()
     (let* ([ranges items (parse-data data)]
            [range-list (sort-range-list ranges)]
            [merged-ranges (merge-ranges range-list 0 0 '())])
       (values (n-fresh-items items merged-ranges)
               (fresh-count merged-ranges 0))))))

