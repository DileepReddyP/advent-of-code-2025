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
  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124
")

(define *part-2-data*
  (call-with-input-file "./2.txt" get-string-all))

(define-peg-pattern dataset body
  (and (* id-range) (ignore "\n")))
(define-peg-pattern id-range body
  (and num (ignore "-") num (? (ignore ","))))
(define-peg-pattern num all
  (+ (range #\0 #\9)))

(define (parse-data data)
  (peg:tree (match-pattern dataset data)))

(define-cached (e10 k)
  (expt 10 k))

(define-cached (proper-divisors n)
  (list-ec (:range i 1 (1+ (floor/ n 2)))
           (if (zero? (modulo n i)))
           i))

(define (is-double-num? n)
  (let* ([s-n (number->string n)]
         [l-n (string-length s-n)]
         [uh-n (substring s-n 0 (/ l-n 2))]
         [m-n (string->number (string-append uh-n uh-n))])
    (= m-n n)))

(define (construct-rep-num s-n l-n pd)
  (let* ([reps (euclidean-quotient l-n pd)]
         [s-l (substring s-n 0 pd)]
         [s-m (string-concatenate (list-ec (:range i reps) s-l))])
    (string->number s-m)))

(define (is-rep-num? n)
  (let* ([s-n (number->string n)]
         [l-n (string-length s-n)]
         [pdivs (proper-divisors l-n)])
    (any?-ec (:list pd pdivs)
             (= (construct-rep-num s-n l-n pd) n))))

(define (find-doubles range part2?)
  (match-let* ([(('num x) ('num y)) range]
               [l-x (string-length x)]
               [l-y (string-length y)]
               [n-x (string->number x)]
               [n-y (string->number y)])
    (if part2?
        (sum-ec (:range n n-x (1+ n-y))
                (if (is-rep-num? n))
                n)
        (if (and (odd? l-x) (odd? l-y) (= l-x l-y))
            0
            (let ([s-x (if (even? l-x) n-x (e10 l-x))]
                  [s-y (if (even? l-y) n-y (1- (e10 (1- l-y))))])
              (sum-ec (:range n s-x (1+ s-y))
                      (if (is-double-num? n))
                      n))))))

(define (solve-2 data)
  (statprof
   (lambda ()
     (let ([rs (parse-data data)])
       (values (sum-ec (:list r rs)
                       (find-doubles r #f))
               (sum-ec (:list r rs)
                       (find-doubles r #t)))))))
