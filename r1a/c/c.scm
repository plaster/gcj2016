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
    ;; v[i] = #f (not visited)
    ;;      | #t (in traversal)
    ;;      | #( circuit-size end-point distance )
    (let loop
      [[i 0]]
      (and (< i N)
        (match (vector-ref v i)
          [ #f
            ]
          [ #t
            ]
          [ #(circuit-size end-point distance)
            ]
          )
        (loop (+ i 1))
        ))))

(define (solve N BFFs)
  ;; single N-loop
  ;; multiple 2-loops
  )
