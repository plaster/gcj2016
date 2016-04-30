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

(define (gcj-interact parser solver emitter)
  (dotimes (n (line-read))
    (parameterize [[gcj-current-case (+ n 1)]]
      ((.$ emitter solver parser)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; problem specific code ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main args)
  (gcj-interact parse solve emit))

(define (emit C J dgs-C dgs-J)
  (let [[str-C (x->string C)]
        [str-J (x->string J)]]
    (format #t "Case #~a: ~a~a ~a~a\n"
            (gcj-current-case)
            (make-string (- (length dgs-C) (string-length str-C)) #\0)
            C
            (make-string (- (length dgs-J) (string-length str-J)) #\0)
            J
            )))

(define (parse)
  ($ apply values
    $ map (.$ reverse
              (map$ digit->integer)
              string->list)
    (string-split (read-line) #\space)
    ))

(define (count-placeholders dgs)
  (count not dgs))

(define (eval-dgs dgs arg)
  (let loop [[r 1]
             [dgs dgs]
             [arg arg]
             [acc 0]
             ]
    (match dgs
      [ () acc ]
      [ ( dg . dgs )
       (if dg
         (loop (* r 10)
               dgs
               arg
               (+ acc (* r dg))
               )
         (loop (* r 10)
               dgs
               (quotient arg 10)
               (+ acc (* r (modulo arg 10)))
               )
         )]
      )))

(define (solve dgs-C dgs-J)
  (let* [[phcount-C (count-placeholders dgs-C)]
         [phcount-J (count-placeholders dgs-J)]
         [phcount (+ phcount-C phcount-J)]
         [args-int-sup (expt 10 phcount)]
         [args-int-sup-J (expt 10 phcount-J)]
         ]
    (let loop [[args-int 0]
               [C #f]
               [J #f]
               [dif #f]
               ]
      (if (>= args-int args-int-sup)
        (values C J dgs-C dgs-J)
        (let* [[args-J (modulo args-int args-int-sup-J)]
               [args-C (quotient args-int args-int-sup-J)]
               [C1 (eval-dgs dgs-C args-C)]
               [J1 (eval-dgs dgs-J args-J)]
               [dif1 (abs (- C1 J1))]
               ]
          (if (or (not dif)
                (> dif dif1))
            (loop (+ args-int 1)
                  C1 J1 dif1)
            (loop (+ args-int 1)
                  C J dif)
            ))))))
