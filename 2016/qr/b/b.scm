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
  (gcj-interact parse solve))

(define parse
  (.$ reverse
      (map$ (.$ string->symbol string))
      string->list
      read-line
      ))

;; + => 0
;; - => 1
;; ++ => 0
;; -+ => 1
;; -- => 1
;; +- => 2
;; +++ => 0
;; -++ => 1
;; --+ => 1
;; --- => 1
;; +-- => 2
;; ++- => 2
;; +-+ => 2
;; -+- => 3

(define (solve ps)
  (let loop
    [[y 0]   ;; number of operation
     [p0 '+] ;; pancake previous
     [ps ps] ;; pancake stack (reverse order)
     ]
    (match ps
      [() y]
      [(p1 . ps)
       (loop (if (eq? p1 p0) y (+ y 1))
             p1
             ps
             )
       ]
      )))
