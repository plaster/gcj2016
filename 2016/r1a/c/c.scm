(use gauche.parameter)
(use util.match)
(use gauche.sequence)
(use util.combinations)
(use gauche.collection)

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
    ;;      | #( sz point-list rep-point 0 )
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
                    (cont e)
                    (set! (vector-ref v i) e)
                    (vector sz point-list end-point 0)
                    )
                  ]
                 [else
                   (vector 'inside (cons i point-list) end-point
                           (^ (e)
                             (set! (vector-ref v i) e)
                             (cont e)))
                   ]
                 )
               ]
              [#(sz point-list end-point dt)
               (rlet1 e (if (= dt 0)
                          (vector sz point-list (vector-ref BFFs i) 1)
                          (vector sz point-list end-point (+ dt 1))
                          )
                 (set! (vector-ref v i) e)
                 ) ]
              )
            ]
          [ #t
            (vector 'inside (list i) i
                    (^ (e)
                      (set! (vector-ref v i) e)))
            ]
          [ (and e #(sz point-list end-point dt))
           e
           ]
          )
        ))))

(define (loop-size-of v)
  (match v
    [ #(sz _ _ _)
      sz ]))

(define (point-list-of v)
  (match v
    [ #(_ point-list _ _)
      point-list
      ]))

(define (max-size-of vs)
  (let loop
    [[ vs vs ]
     [ dt-left 0]
     [ dt-right 0]
     ]
    (match vs
      [()
       (+ 2 dt-left dt-right)
       ]
      [( #( 2 (p0 p1) pe dt) . vs)
       (cond
         [ (= pe p0)
          (loop vs (max dt-left dt) dt-right)
          ]
         [ (= pe p1)
          (loop vs dt-left (max dt-right dt))
          ]
         [else
           (errorf "no match end-point: ~s for loop ~s" pe `(,p0 ,p1))
           ])
       ])))

(define (solve N BFFs)
  ;; single N-loop or
  ;; multiple 2-loop (and its trailer)s
  (let1 ls (vector->list (traverse N BFFs))
    (max (apply max (map loop-size-of ls))
         (apply + (map max-size-of
                       (group-collection (filter (.$ (pa$ = 2) loop-size-of) ls)
                                         :key point-list-of
                                         )
                       )))))
