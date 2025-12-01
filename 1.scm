(use-modules (statprof)
             (ice-9 peg)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-42)
             (srfi srfi-71)
             (ice-9 textual-ports))

(define *example-data*
  "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
")

(define *part-1-data*
  (call-with-input-file "./1.txt" get-string-all))

(define (modulo-ring insts part2?)
  (let ([steps (string-split insts #\newline)]
        [loc 50]
        [zeros 0])
    (do-ec (:list step steps)
           (not (string-null? step))
           (:let dir (if (char=? (string-ref step 0) #\R) + -))
           (:let turn (string->number (substring step 1)))
           (begin
             (if part2?
                 (do-ec (:range s turn)
                        (begin
                          (set! loc (modulo (dir loc 1) 100))
                          (when (zero? loc)
                            (set! zeros (1+ zeros)))))
                 (let ([m (modulo (dir loc turn) 100)])
                   (set! loc m)
                   (when (zero? loc)
                     (set! zeros (1+ zeros)))))))
    zeros))

(define (solve-1 data)
  (statprof
   (lambda ()
     (values (modulo-ring data #f)
             (modulo-ring data #t)))))
