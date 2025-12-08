(use-modules (statprof)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-42)
             (srfi srfi-71)
             (ice-9 textual-ports))

(define *example-data*
  "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
")

(define *part-8-data*
  (call-with-input-file "./8.txt" get-string-all))

(define (parse-data data)
  (let* ([lines (remove string-null? (string-split data #\newline))]
         [pos-strs (map (cut string-split <> #\,) lines)]
         [positions (map (cut map string->number <>) pos-strs)])
    positions))

(define (euclidean-distance p1 p2)
  (+ (expt (- (first p2) (first p1)) 2)
     (expt (- (second p2) (second p1)) 2)
     (expt (- (third p2) (third p1)) 2)))

(define (point-distances points)
  (let ([l (length points)])
    (sort (list-ec (:list p1 (index i) points)
                   (:range j (1+ i) l)
                   (:let p2 (list-ref points j))
                   (cons (euclidean-distance p1 p2) (list p1 p2)))
          (lambda (a b) (< (car a) (car b))))))

(define (make-circuits pds n l)
  (let ([circuits (make-hash-table)]
        [circuit-member (make-hash-table)]
        [done #f])
    (do-ec (:list pd (index i) pds)
           (not done)
           (match-let ([(_ . (p1 p2)) pd])
             (match (list (hash-ref circuit-member p1) (hash-ref circuit-member p2))
               [(#f #f)
                (and (hash-set! circuits i (list p1 p2))
                     (hash-set! circuit-member p1 i)
                     (hash-set! circuit-member p2 i))]
               [(c1 #f)
                (and (hash-set! circuits c1 (cons p2 (hash-ref circuits c1)))
                     (hash-set! circuit-member p2 c1))]
               [(#f c2)
                (and (hash-set! circuits c2 (cons p1 (hash-ref circuits c2)))
                     (hash-set! circuit-member p1 c2))]
               [(c1 c2)
                (when (not (= c1 c2))
                  (hash-set! circuits c1 (append (hash-ref circuits c1)
                                                 (hash-ref circuits c2)))
                  (do-ec (:list p (hash-ref circuits c2))
                         (hash-set! circuit-member p c1))
                  (hash-remove! circuits c2))])
             (when (= i n)
               (let* ([cs (hash-map->list (lambda (_ v) (length v)) circuits)]
                      [l3 (take (sort cs >=) 3)])
                 (format #t "Part 1: ~s\n" (apply * l3))))
             (when (and (= (hash-count (const #t) circuit-member) l)
                        (apply = (hash-map->list (lambda (_ v) v) circuit-member)))
               (format #t "Part 2: ~s\n" (* (first p1) (first p2)))
               (set! done #t))))))

(define (solve-8 data n)
  (statprof
   (lambda ()
     (let* ([positions (parse-data data)]
            [pds (point-distances positions)])
       (make-circuits pds n (length positions))))))

