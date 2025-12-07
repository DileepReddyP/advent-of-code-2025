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
  ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
")

(define *part-7-data*
  (call-with-input-file "./7.txt" get-string-all))

(define (parse-data data)
  (let* ([tachyon-map (create-grid-dict data)])
    (values tachyon-map
            (hash-fold (lambda (p c prev)
                         (if (char=? c #\S) p prev))
                       '() tachyon-map))))

(define (split-beams tachyon-map beam-table acc)
  (let* ([pos (hash-map->list (lambda (k _) k) beam-table)])
    (if (any?-ec (:list b pos)
                 (not (hash-ref tachyon-map (1+ b)))
                 b)
        (values acc (hash-fold (lambda (_ v prev)
                                 (+ prev v))
                               0 beam-table))
        (let ([new-beam-table (make-hash-table)]
              [new-acc acc])
          (do-ec (:list p pos)
                 (:let np (1+ p))
                 (:let c (hash-ref tachyon-map np))
                 (begin
                   (when (char=? c #\.)
                     (hash-set! new-beam-table np
                                (+ (hash-ref new-beam-table np 0)
                                   (hash-ref beam-table p))))
                   (when (char=? c #\^)
                     (set! new-acc (1+ new-acc))
                     (do-ec (:list np2 (map (cut + np <>) '(0.0+1.0i 0.0-1.0i)))
                            (hash-set! new-beam-table np2
                                       (+ (hash-ref new-beam-table np2 0)
                                          (hash-ref beam-table p)))))))
          (split-beams tachyon-map new-beam-table new-acc)))))

(define (solve-7 data)
  (statprof
   (lambda ()
     (let ([beam-table (make-hash-table)]
           [tachyon-map start-pos (parse-data data)])
       (hash-set! beam-table start-pos 1)
       (split-beams tachyon-map beam-table 0)))))

