(define-module (union-find)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 hash-table)
  #:export (make-union-find
            uf-new-set!
            uf-find!
            uf-union!
            uf-connected?))

(define-record-type <union-find-node>
  (make-union-find-node parent rank)
  union-find-node?
  (parent node-parent node-parent-set!)
  (rank node-rank node-rank-set!))

(define-record-type <union-find>
  (make-union-find-internal table)
  union-find?
  (table union-find-table))

(define (make-union-find)
  (make-union-find-internal (make-hash-table)))

(define (uf-new-set! uf x)
  (hash-set! (union-find-table uf) x
             (make-union-find-node x 0)))

(define (uf-find! uf x)
  (and=> (hash-ref (union-find-table uf) x)
         (lambda (node)
           (let ([parent (node-parent node)])
             (if (equal? parent x)
                 x
                 (and=> (uf-find! uf parent)
                        (lambda (root)
                          (node-parent-set! node root)
                          root)))))))

(define (uf-union! uf x y)
  (let ([root-x (uf-find! uf x)]
        [root-y (uf-find! uf y)])
    (if (and root-x root-y (not (equal? root-x root-y)))
        (let* ([table (union-find-table uf)]
               [node-x (hash-ref table root-x)]
               [node-y (hash-ref table root-y)]
               [rank-x (node-rank node-x)]
               [rank-y (node-rank node-y)])
          (cond
           [(< rank-x rank-y)
            (node-parent-set! node-x root-y)]
           [(> rank-x rank-y)
            (node-parent-set! node-y root-x)]
           [else
            (node-parent-set! node-y root-x)
            (node-rank-set! node-x (1+ rank-x))])
          #t)
        #f)))

(define (uf-connected? uf x y)
  (let ([root-x (uf-find! uf x)]
        [root-y (uf-find! uf y)])
    (and root-x root-y (equal? root-x root-y))))
