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
  "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
")

(define *part-10-data*
  (call-with-input-file "./10.txt" get-string-all))

(define-peg-pattern dataset body
  (+ init-proc))
(define-peg-pattern init-proc body
  (and init-state ws (+ button-schema) joltage-req (* nl)))
(define-peg-pattern init-state all
  (and (ignore "[")
       (* (or s-on s-off))
       (ignore "]")))
(define-peg-pattern button-schema all
  (and (ignore "(")
       (* (or (ignore ",") num))
       (ignore ")")
       (* ws)))
(define-peg-pattern joltage-req all
  (and (ignore "{")
       (* (or (ignore ",") num))
       (ignore "}")))
(define-peg-pattern s-on all
  (ignore "#"))
(define-peg-pattern s-off all
  (ignore "."))
(define-peg-pattern num all
  (+ (range #\0 #\9)))
(define-peg-pattern ws none
  " ")
(define-peg-pattern nl none
  "\n")


(define (parse-tree->state parse-tree)
  (list-ec (:list p parse-tree)
           (list-ec (:list item p)
                    (match item
                      [('init-state states)
                       (string->number
                        (reverse-list->string
                         (list-ec (:list state states)
                                  (if (eq? state 's-on) #\1 #\0)))
                        2)]
                      [(('button-schema . nlists) ..1)
                       (list-ec (:list nstr nlists)
                                (:let nlist (map string->number (map second nstr)))
                                nlist)]
                      [('joltage-req . nlist)
                       (map string->number (map second nlist))]))))

(define (list->button-wiring nlists)
  (list-ec (:list nlist nlists)
           (:let bstr (make-string (1+ (apply max nlist)) #\0))
           (begin
             (do-ec (:list bw nlist)
                    (string-set! bstr bw #\1))
             (string->number (string-reverse bstr) 2))))

(define (parse-data data)
  (let* ([parse-tree (peg:tree (match-pattern dataset data))]
         [configs (parse-tree->state parse-tree)])
    configs))

(define (min-indicator configs)
  (sum-ec (:list config configs)
          (:let target (first config))
          (:let wirings (list->button-wiring (second config)))
          (:let lw (length wirings))
          (first-ec #f (:range i 1 (1+ lw))
                    (:let perms (list-permutations-with-repeats wirings i))
                    (if (any?-ec (:list bs perms)
                                 (:let state-after (fold logxor 0 bs))
                                 (= state-after target)))
                    i)))

(define (glpsol-lp-string port minimize subject-to bounds general)
  (format port
          "Minimize
 obj: ~a

Subject To
~a

Bounds
~a

General
 ~a

End
" minimize subject-to bounds general))

(define (generate-constraints wirings joltages)
  (string-join (list-ec (:list r (index i) joltages)
                        (:let c (string-join (list-ec (:list button (index j) wirings)
                                                      (if (member i button))
                                                      (format #f "x~a" j))

                                             "+"))
                        (format #f " pos~a: ~a = ~a" i c r))
               "\n"))

(define (send-to-glpsol configs)
  (sum-ec (:list config configs)
          (:let wirings (second config))
          (:let joltages (third config))
          (:let lw (length wirings))
          (:let lj (length joltages))
          (:let var-names (list-ec (:range i lw)
                                   (format #f "x~a" i)))
          (:let minimize (string-join var-names "+"))
          (:let subject-to (generate-constraints wirings joltages))
          (:let bounds (string-join (list-ec (:list v var-names)
                                             (format #f " ~a >= 0" v))
                                    "\n"))
          (:let general (string-join var-names " "))
          (begin
            (call-with-output-file "10.lp"
              (lambda (port)
                (glpsol-lp-string port minimize subject-to bounds general)))
            (system "glpsol --lp 10.lp -w 10.sol > /dev/null 2>&1")
            (let* ([op (call-with-input-file "./10.sol" get-string-all)]
                   [val-str (match:substring (string-match "obj = ([0-9]+)" op) 1)])
              (string->number val-str)))))

(define (solve-10 data)
  (statprof
   (lambda ()
     (let ([configs (parse-data data)])
       (values (min-indicator configs)
               (send-to-glpsol configs))))))


