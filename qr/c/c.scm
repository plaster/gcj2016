(use gauche.parameter)
(use util.match)
(use util.combinations)
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

(define (gcj-interact parser solver)
  (dotimes (n (line-read))
    (parameterize [[gcj-current-case (+ n 1)]]
      (format #t "Case #~a:\n" (gcj-current-case))
      (let1 jam-coin-list (call-with-values parser solver)
        (for-each (lambda (jam-coin)
                    (print (string-join (map x->string jam-coin) " "))
                    )
                  jam-coin-list
                  )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; problem specific code ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main args)
  (gcj-interact parse solve))

(define (parse)
  (let* [[N (read)]
         [J (read)]
         ]
    (values N J)
    ))

(define (find-divisor n)
  (let loop [[d 2]
             ]
    (and (< d n)
      (<= (* d d) n)
      (or (and (zero? (modulo n d))
            d)
        (loop (+ d 1))
        ))))

(define (jam-coin? s)
  (let/cc return
    (map (^r
           (if-let1 d (find-divisor (string->number s r))
             d
             (return #f)
             ))
         (iota 9 2)
         )))

(define (solve N J)
  (rlet1 cs '()
    (let/cc return
      (dotimes (c (expt 2 (- N 2)))
        (if (zero? J) (return))
        (let* [[s (number->string
                    (logior 1
                            (ash c 1)
                            (ash 1 (- N 1))
                            )
                    2)]
               [ds (jam-coin? s)]]
          (when ds
            (push! cs (cons s ds))
            (dec! J)
            ))
        )
        )))
