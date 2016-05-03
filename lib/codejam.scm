;;; codejam - Common Utility for Google Code Jam

(define-module codejam
  (export-all))

(use gauche.parameter)

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

(define current-case (make-parameter #f))

(define (run parser solver emitter)
  (dotimes (n (line-read))
    (parameterize [[current-case (+ n 1)]]
      ((.$ emitter solver parser)))))

(define (run/parallel parser solver emitter :optional [parallel-level 4])
  (let* [[n (line-read)]
         [parsed (make-vector n #f)]
         [solved (make-vector n #f)]
         ]
    (dotimes (i n)
      (call-with-values parser
                        (cut set! (vector-ref parsed i) <...>))
      )
    ;;; TODO: run solver parallel and store result to vector solved
    (dotimes (i n)
      (parameterize [[current-case (+ i 1)]]
        (apply emitter (vector-ref solved i))
        ))))
