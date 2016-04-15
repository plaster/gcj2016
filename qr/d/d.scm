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

(define (parse)
  (line-read (replist$ 3 read))
  )

;; L -> LL -> LLLL -> ...
;; G -> GG -> GGGG -> ...

;; LL -> LLLL -> LLLLLLLL -> ...
;; LG -> LGGG -> LGGGGGGG -> ...
;; GL -> GGGL -> GGGGGGGL -> ...
;; GG -> GGGG -> GGGGGGGG -> ...

;; LLL -> LLLLLLLLL -> LLLLLLLLLLLLLLLLLLLLLLLLLLL
;; LLG -> LLGLLGGGG -> LLGLLGGGGLLGLLGGGGGGGGGGGGG
;; LGL -> LGLGGGLGL -> LGLGGGLGLGGGGGGGGGLGLGGGLGL
;; LGG -> LGGGGGGGG -> LGGGGGGGGGGGGGGGGGGGGGGGGGG
;; GLL -> GGGGLLGLL -> GGGGGGGGGGGGGLLGLLGGGGLLGLL
;; GLG -> GGGGLGGGG -> GGGGGGGGGGGGGLGGGGGGGGGGGGG
;; GGL -> GGGGGGGGL -> GGGGGGGGGGGGGGGGGGGGGGGGGGL
;; GGG -> GGGGGGGGG -> GGGGGGGGGGGGGGGGGGGGGGGGGGG

(define (length-of K C)
  (expt K C))

(define (next-level seq)
  (apply append
         (map (lambda (c)
                (if (eq? c 'G)
                  (make-list (length seq) 'G)
                  seq
                  ))
              seq)))

(define (solve K C S)
  (error "not implemented")
  )
