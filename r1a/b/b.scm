(use gauche.parameter)
(use util.match)
(use gauche.sequence)

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
  (gcj-interact parse solve
                (lambda (ls)
                  (string-join (map x->string ls) " ")
                  )
                ))

(define (parse)
  (let1 N (line-read)
    (values N
            (replist (- (* 2 N) 1)
                     (line-read$ (replist$ N read)))))
  )

(define (solve N reports)
  (let1 v (make-vector 2500 0)
    (for-each
      (^ (report)
        (for-each
          (^ (h)
            (update! (vector-ref v (- h 1)) (pa$ + 1))
            )
          report))
      reports
      )
    (let1 r '()
      (vector-for-each-with-index
        (^ (h f)
          (if (odd? f)
            (push! r (+ h 1)))
          )
        v)
      (reverse r)
      )))
