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

(define (emit . xs)
  (format #t "Case #~a: ~a\n"
          (gcj-current-case)
          (string-join (map x->string xs) " ")
          ))

(define (parse)
  ($ chlist->hist $ sort $ string->list $ read-line)
  )

(define (chlist->hist chs)
  (map (^ (ls)
         (cons (car ls)
               (length ls)))
       (group-sequence chs)
       ))

(define *t*
  (map (.$ chlist->hist sort string->list)
       '("ZERO" "ONE" "TWO" "THREE" "FOUR" "FIVE" "SIX" "SEVEN" "EIGHT" "NINE"
         )))

(define (subt-hist h0 h1)
  (let loop [[h0 h0]
             [h1 h1]
             [acc '()]
             ]
    (match h1
      [() 
       (append-reverse acc h0)
       ]
      [((h1x . h1n) . h1s)
       (match h0
         [()
          #f
          ]
         [((h0x . h0n) . h0s)
          (cond
            [(char>? h0x h1x)
             #f]
            [(and (char=? h0x h1x)
               (>= h0n h1n))
             (loop h0s h1s
                   (if (= h0n h1n)
                     acc
                     (cons (cons h0x (- h0n h1n))
                           acc)))]
             [else
               (loop h0s h1 (cons (cons h0x h0n) acc))
               ])
          ]
         )
       ])))


(define (solve h)
  (let loop
    [[h h]
     [ts *t*]
     [dg 0]
     [acc '()]
     ]
    (match h
      [ ()
       (string-join (map x->string (reverse acc)) "")
       ]
      [else
        (match ts
          [ ()
           #f
           ]
          [ (t . ts1)
           (if-let1 h1 (subt-hist h t)
             (or (loop h1 ts dg (cons dg acc))
               (loop h ts1 (+ 1 dg) acc)
               )
             (loop h ts1 (+ 1 dg) acc)
             )
           ]
          )
        ]
      )))
