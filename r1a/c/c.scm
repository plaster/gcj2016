(use gauche.parameter)
(use util.match)
(use gauche.sequence)
(use util.combinations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; common library for GCJ ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (line-read :optional [reader read])
  (with-input-from-string (read-line) reader))

(define line-read$ (pa$ pa$ line-read))

(define (replist n proc)
  (let loop [[n n]
             [acc '()]
             ]
    (if (zero? n)
      (reverse acc)
      (loop (- n 1) (cons (proc) acc)))))

(define replist$ (pa$ pa$ replist))

(define (read-matrix rows cols :optional [reader read-char])
  (replist rows (line-read$ (replist$ cols reader))))

(define gcj-current-case (make-parameter #f))

(define (standard-formatter . xs) (string-join (map x->string xs) " "))

(define (gcj-interact parser solver :optional [formatter standard-formatter])
  (dotimes (n (line-read))
    (parameterize [[gcj-current-case (+ n 1)]]
      (format #t "Case #~a: ~a\n"
              (gcj-current-case)
              ((compose formatter
                        solver
                        parser))
              ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; problem specific code ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main args)
  (gcj-interact parse solve))

(define (parse)
  (let1 N (line-read)
    (values N
            ($ list->vector
              $ map (pa$ + -1)
              $ line-read $ replist$ N read)
            )))

(define (traverse N BFFs)
  (rlet1 v (make-vector N #f)
    ;; v[i] = #f -- not visited
    ;;      | #t -- just in traversal
    ;;      | #( sz point-list #f 0 )
    ;;           -- in loop of rep-point, size=sz. 
    ;;      | #( sz point-list end-point dt )
    ;;           -- at distance dt from end-point, which is included loop of rep-point
    (dotimes (i N)
      (let dfs
        [[i i]]
        (match (vector-ref v i)
          [ #f
            (set! (vector-ref v i) #t)
            (match (dfs (vector-ref BFFs i))
              [#('inside point-list end-point cont ) ;; in loop
               (cond
                 [(= end-point i)
                  (let* [[sz (length point-list)]
                         [e (vector sz point-list end-point 0)]]
                    (cont point-list)
                    (set! (vector-ref v i) e)
                    (vector 'outside sz point-list end-point 0)
                    )
                  ]
                 [else
                   (vector 'inside (cons i point-list)
                           (^ (e)
                             (set! (vector-ref v i) e)
                             (cont e)))
                   ]
                 )
               ]
              [#('outside sz point-list end-point dt)
               (set! (vector-ref v i)
                 (vector sz point-list end-point (+ dt 1)))
               (vector 'outside sz point-list end-point (+ dt 1))
               ]
              )
            ]
          [ #t
            (vector 'inside (list i)
                    (^ (e)
                      (set! (vector-ref v i) e)))
            ]
          [ #(circuit-size end-point distance)
            ]
          )
        ))))

(define (solve N BFFs)
  ;; single N-loop
  ;; multiple 2-loops
  )
