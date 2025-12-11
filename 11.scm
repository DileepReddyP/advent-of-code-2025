(use-modules (statprof)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-42)
             (srfi srfi-71)
             (ice-9 textual-ports)
             (aoc-2024-common))

(define *part-11-data*
  (call-with-input-file "./11.txt" get-string-all))

(define (parse-data data)
  (let* ([lines (remove string-null? (string-split data #\newline))]
         [tree-table (make-hash-table)])
    (do-ec (:list l lines)
           (:let sl (string-split l #\:))
           (:let device (first sl))
           (:let outputs (remove string-null? (string-split (second sl) #\space)))
           (hash-set! tree-table device outputs))
    tree-table))

(define-cached (device-bfs tree-table device dac? fft?)
  (if (string= device "out")
      (if (and dac? fft?) 1 0)
      (sum-ec (:list next-device (hash-ref tree-table device))
              (device-bfs tree-table next-device
                          (or dac? (string= "dac" next-device))
                          (or fft? (string= "fft" next-device))))))

(define (solve-11 data)
  (statprof
   (lambda ()
     (let ([tree-table (parse-data data)])
       (values (device-bfs tree-table "you" #t #t)
               (device-bfs tree-table "svr" #f #f))))))
