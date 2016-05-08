;; runs on Gauche 0.9.4: http://practical-scheme.net/gauche/
;; module 'codejam' is available at:
;;   https://github.com/plaster/google-code-jam-solutions/blob/master/lib/codejam.scm
;; latest template is available at:
;;   https://github.com/plaster/google-code-jam-solutions/blob/master/_template.scm

(use codejam)
(use util.match)
(use gauche.sequence)
(use gauche.generator)

(define (main args)
  (run parse solve emit))

(define (emit m)
  (cond
    [ m
      (format #t "Case #~a: ~a\n"
              (current-case)
              'POSSIBLE
              )
      (for-each (.$ print
                    (cut string-join <> "")
                    (map$ x->string)
                    )
                m)
      ]
    [ else
      (format #t "Case #~a: ~a\n"
              (current-case)
              'IMPOSSIBLE
              )
      ]
    ))

(define (mat-transpose mat) 
  (apply map list mat)
  )

(define (mat-mult mat0 tr-mat1)
  (map 
    (^ (row0)
      (map
        (^ (row1)
          (apply + (map * row0 row1))
          )
        tr-mat1))
    mat0
    ))

(define (parse)
  (apply values (line-read (replist$ 2 read)))
  )

(define (g$ B)
  (let1 imax ($ expt 2 $ / ($ * B $ - B 1) 2)
    (generate
      (^ [yeald]
        (let loop [[i 1]]
          (when (< i imax)
            ($ yeald $ mat<-i B i)
            ($ loop $ + i 1)
            ))))))

(define (mat<-i B i)
  (map (^k
         (map (^l
                (if (and
                      (< k l)
                      (< k (- B 1)))
                  (logand (ash (+ i 1)
                               (* -1
                                  (+ (- l k)
                                     (let1 k- (- B 1 k)
                                       (/ (* k- (- k- 1)) 2)
                                       ))
                                  ))
                          1)
                  0
                  ))
              (iota B)))
       (iota B))
  )

(define (count-way B mat)
  (let1 tr-mat (mat-transpose mat)
    (let loop [[s 0]
               [m mat]
               [b 1]
               ]
      (if (< b B)
        (loop (+ s (list-ref (list-ref m 0) (- B 1)))
              (mat-mult m tr-mat)
              (+ b 1))
        s
        ))))

(define (solve B M)
  (let/cc return
    (port-for-each (^m (if (= M (count-way B m)) (return m) ))
                   (g$ B))
    #f
    ))
