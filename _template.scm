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

(define (gcj-interact parser solver emitter)
  (dotimes (n (line-read))
    (parameterize [[gcj-current-case (+ n 1)]]
      ((.$ emitter solver parser)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; problem specific code ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main args)
  (gcj-interact parse solve emit))

(define (emit . xs)
  (format #t "Case #~a: ~a\n"
          (gcj-current-case)
          (string-join (map x->string xs) " ")
          ))

(define (parse)
  (error "not implemented")
  )

(define (solve . args)
  (error "not implemented")
  )
