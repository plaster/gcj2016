;; runs on Gauche 0.9.5: http://practical-scheme.net/gauche/
;; module 'codejam' is available at:
;;   https://github.com/plaster/google-code-jam-solutions/blob/master/lib/codejam.scm
;; latest template is available at:
;;   https://github.com/plaster/google-code-jam-solutions/blob/master/_template.scm

(use codejam)
(use util.match)
(use gauche.sequence)

(define (main args)
  (run/parallel parse solve emit 4))

(define (emit . xs)
  (format #t "Case #~a: ~a\n"
          (current-case)
          (string-join (map x->string xs) " ")
          ))

(define (parse)
  (apply values (line-read (pa$ replist 2 read))))

(define (interval-distance i)
  (match i
    [ #(l r) (- r l) ] ) )

(define (interval-comparator i0 i1)
  (match i0
    [ #(l0 r0)
      (match i1
        [ #(l1 r1)
          (let [[ d0 (- r0 l0) ]
                [ d1 (- r1 l1) ]
                ]
            (cond
              [ (< d1 d0) -1 ]
              [ (< d0 d1) 1 ]
              [ (< l0 l1) -1 ]
              [ (< l1 l0) 1 ]
              [else 0 ] ) )
          ] ) ] ) )

(define (init-interval N)
  ;; left inclusive
  ;; right exclusive
  `#(0 ,N) )

(define (split-interval i0)
  (match i0
    [ #( l0 r1 )
      (or (< l0 r1) (errorf "no space left: ~s" i0))
      (let* [[d0 (- r1 l0)]
             [r0 (+ l0 (quotient d0 2)) ]
             [l1 (+ r0 1) ]
             ]
        (values `#(,l0 ,r0)
                `#(,l1 ,r1))
        ) ] ) )

(define (solve N K)
  (let [[q (make-tree-map interval-comparator)]
        [i0 (init-interval N) ]
        ]
    (tree-map-put! q i0 i0)
    (let loop [[K K]
               [dl #f]
               [dr #f]
               ]
      (if (zero? K)
        (values (max dl dr)
                (min dl dr))
        (match (tree-map-pop-min! q)
          [ ( i . _ )
           (receive (il ir) (split-interval i)
             (tree-map-put! q il il)
             (tree-map-put! q ir ir)
             (loop (- K 1)
                   (interval-distance il)
                   (interval-distance ir) ) ) ] ) ) ) ) )
