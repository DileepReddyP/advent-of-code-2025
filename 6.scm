(use-modules (statprof)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-42)
             (srfi srfi-71)
             (ice-9 textual-ports)
             (aoc-2024-common))

(define *example-data*
  "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
")

(define *part-6-data*
  (call-with-input-file "./6.txt" get-string-all))

(define (parse-oplist ops)
  (map (cut assoc-ref `(("*" . ,*)
                        ("+" . ,+)) <>)
       (remove string-null? (string-split ops #\space))))

(define (parse-data dataset)
  (let* ([ops vals (car+cdr (reverse (remove string-null?
                                             (string-split dataset #\newline))))]
         [oplist (parse-oplist ops)]
         [valarray (make-array #f (length vals) (length oplist))])
    (do-ec (:list valstring (index i) vals)
           (:let vallist (remove string-null?
                                 (string-split valstring #\space)))
           (do-ec (:list v (index j) vallist)
                  (array-set! valarray v i j)))
    (values oplist (transpose-array valarray 1 0))))

(define (vertical-ops data)
  (let ([oplist valarray (parse-data data)])
    (sum-ec (:list op (index i) oplist)
            (apply op (map string->number (array->list (array-cell-ref valarray i)))))))

(define (parse-cephalopod dataset)
  (let* ([ops vals (car+cdr (reverse (remove string-null?
                                             (string-split dataset #\newline))))]
         [col-list (map match:substring (list-matches "(\\+|\\*)[ ]+" ops))]
         [oplist (parse-oplist ops)]
         [curr-pos 0])
    (sum-ec (:list col (index i) col-list)
            (:let cl (string-length col))
            (:let tns (list-ec (:range k cl)
                               (:let ts (string-ec (:list vs vals)
                                                   (string-ref vs (+ curr-pos k))))
                               (:let tb (string-reverse (string-trim-both ts)))
                               (not (string-null? tb))
                               (string->number tb)))
            (begin
              (set! curr-pos (+ curr-pos cl))
              (apply (list-ref oplist i) tns)))))

(define (solve-6 data)
  (statprof
   (lambda ()
     (values (vertical-ops data)
             (parse-cephalopod data)))))
