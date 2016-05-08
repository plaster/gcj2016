;;;; codejam - Common Utility for Google Code Jam

(define-module codejam
  (export-all))

(select-module codejam)

(use gauche.parameter)
(use gauche.threads)
(use util.match)

;;; I/O helper

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

;;; Parameter to output "Case #~a:"

(define current-case (make-parameter #f))

;;; Common Data structure

;; Skew Heap
(define (skew-heap seq)
  (fold skew-heap-insert '#() seq)
  )

(define (skew-heap-empty) '#())

(define (skew-heap-singleton e)
  (vector e '#() '#()))

(define (skew-heap-union heap0 heap1)
  (match `#(,(force heap0) ,(force heap1))
    [ #( #() _ )
      heap1
      ]
    [ #( _ #() )
      heap0
      ]
    [ #( #(e0 l0 r0) #(e1 l1 r1) )
      (delay
        (if (<= e0 e1)
          (vector e0 (skew-heap-union heap1 r0) l0)
          (vector e1 (skew-heap-union heap0 r1) l1)
          ))
      ]
    ))

(define (skew-heap-insert e heap)
  (skew-heap-union heap (skew-heap-singleton e))
  )

(define (skew-heap-extract-min heap)
  (match (force heap)
    [ #() (values #f #f) ]
    [ #(e l r)
      (values e (skew-heap-union l r))
      ]))

;;; Runner

(define (run parser solver emitter)
  (dotimes (n (line-read))
    (parameterize [[current-case (+ n 1)]]
      ((.$ emitter solver parser)))))

(define (run/parallel parser solver emitter :optional [parallel-level 4])
  (let* [[n (line-read)]
         [parsed (make-vector n #f)]
         [solved (make-vector n #f)]
         [unsolved ($ atom $ vector $ skew-heap $ iota n)]
         ]
    (dotimes (i n)
      (call-with-values parser
                        (^ pvs (set! (vector-ref parsed i) pvs)))
      )
    (let1 thread-body (^ ()
                        (let loop []
                          (let1 i (atomic unsolved
                                    (^ (v)
                                      (receive (i h)
                                               (skew-heap-extract-min (vector-ref v 0))
                                         (and h
                                           (begin
                                             (set! (vector-ref v 0) h)
                                             i)))))
                            (and i
                              (begin
                                (receive res (apply solver (vector-ref parsed i))
                                  (atomic unsolved
                                    (^ (_)
                                      (set! (vector-ref solved i) res))))
                                (loop)
                                )))))
      (let1 threads (replist parallel-level (cut make-thread thread-body))
        (for-each thread-start! threads)
        (for-each thread-join! threads)
        ))
    (dotimes (i n)
      (parameterize [[current-case (+ i 1)]]
        (apply emitter (vector-ref solved i))
        ))))

(provide "codejam")
