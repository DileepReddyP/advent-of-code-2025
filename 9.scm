(use-modules (statprof)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-42)
             (srfi srfi-71)
             (ice-9 textual-ports)
             (aoc-2024-common))

(define *example-data*
  "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
")

(define *part-9-data*
  (call-with-input-file "./9.txt" get-string-all))

(define (parse-data data)
  (let* ([lines (remove string-null? (string-split data #\newline))]
         [pos-strs (map (cut string-split <> #\,) lines)]
         [positions (map (cut map string->number <>) pos-strs)])
    positions))

(define (rect-area px1 px2 py1 py2)
  (* (1+ (- px2 px1))
     (1+ (- py2 py1))))

(define (points-to-edges points)
  (zip points (append (cdr points) (list (first points)))))

(define (green-boxify edges)
  (list-ec (:list e edges)
           (match e
             [((x1 y1) (x2 y2))
              (list (list (min x1 x2) (min y1 y2))
                    (list (max x1 x2) (max y1 y2)))])))

(define (in-green? px1 px2 py1 py2 green-boxes)
  (not (any?-ec (:list gb green-boxes)
                (match gb
                  [((gx1 gy1) (gx2 gy2))
                   (and (< px1 gx2) (< py1 gy2) (> px2 gx1) (> py2 gy1))]))))

(define* (max-rectangle points part2?)
  (let ([l (length points)]
        [green-boxes (green-boxify (points-to-edges points))])
    (max-ec (:list p1 (index i) points)
            (:range j (1+ i) l)
            (:let p2 (list-ref points j))
            (:let px1 (min (first p1) (first p2)))
            (:let px2 (max (first p1) (first p2)))
            (:let py1 (min (second p1) (second p2)))
            (:let py2 (max (second p1) (second p2)))
            (or (not part2?)
                (in-green? px1 px2 py1 py2 green-boxes))
            (rect-area px1 px2 py1 py2))))

(define (solve-9 data)
  (statprof
   (lambda ()
     (let* ([positions (parse-data data)])
       (values (max-rectangle positions #f)
               (max-rectangle positions #t))))))

