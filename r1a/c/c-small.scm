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

#|
(define (valid? ring ring-size BFF)
  (let/cc return
    (dotimes (i ring-size)
      (let [[R (vector-ref ring (modulo (+ i 1) ring-size))]
            [L (vector-ref ring (modulo (+ i -1 ring-size) ring-size))]
            ]
        (let1 v (vector-ref ring i)
          (or
            (= R (vector-ref BFF v))
            (= L (vector-ref BFF v))
            (return #f)
            ))))
    #t
    ))
  (let* [[sh1 (append (cdr ring) (list (car ring)))]
         [sh2 (append (cdr sh1) (list (car sh1))) ]
         ]
    (every
      (^ (s0 s1 s2)
        (or
          (= (vector-ref BFF s1) s0)
          (= (vector-ref BFF s1) s2)
          ))
      ring
      sh1
      sh2
      )
    |#

(define (score-of ring BFF)
  (rlet1 score 0
    (let/cc return
      (dotimes (i-1 (- (vector-length ring) 2))
        (let* [[i (+ i-1 1)]
               [i+1 (+ i 1)]
               [L (vector-ref ring i-1)]
               [R (vector-ref ring i+1)]
               [v (vector-ref ring i)]
               ]
          (or
            (= L (vector-ref BFF v))
            (= R (vector-ref BFF v))
            (return)
            )
          (and (closed? ring i+1 BFF)
            (set! score (+ i+1 1))
            ))))))

(define (closed? ring end BFF)
  (and
    (or
      (= (vector-ref BFF (vector-ref ring 0)) (vector-ref ring 1))
      (= (vector-ref BFF (vector-ref ring 0)) (vector-ref ring end))
      )
    (or
      (= (vector-ref BFF (vector-ref ring end)) (vector-ref ring 0))
      (= (vector-ref BFF (vector-ref ring end)) (vector-ref ring (- end 1)))
      )
    ))

(define (solve N BFF)
  (rlet1 result 0

    #|
    (define (score-of seq score BFF)
      (if (<= score result)
        0
        (if (valid? seq score BFF)
          score
          (score-of seq (- score 1) BFF)
          )))
    |#

    (permutations-for-each
      (^ (seq)
        (update! result (pa$ max result
                             (score-of (list->vector seq) BFF))))
      (iota N))))
