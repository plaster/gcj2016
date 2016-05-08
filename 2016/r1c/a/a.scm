;; runs on Gauche 0.9.4: http://practical-scheme.net/gauche/
;; module 'codejam' is available at:
;;   https://github.com/plaster/google-code-jam-solutions/blob/master/lib/codejam.scm
;; latest template is available at:
;;   https://github.com/plaster/google-code-jam-solutions/blob/master/_template.scm

(use codejam)
(use util.match)
(use gauche.sequence)

(define (main args)
  (run parse solve emit))

(define (emit plan)
  (format #t "Case #~a: ~a\n"
          (current-case)
          (string-join (map list->string plan) " ")
          ))

(define (parse)
  (let* [[N (line-read)]
         [_PS (line-read (replist$ N read))
             ]
         ]
    (values N _PS)))

(define *label* (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define (solve N _PS)
  (let f [[PS (sort (map cons *label* _PS) < cdr)]
          [plan '()]
          ]
    (let g [[PS PS]
            [PS2 '()]
            [plan plan]
            ]
      (match PS
        [ ()
         (match PS2
           [()
            plan
            ]
           [else
            (f (sort (remove (.$ (pa$ = 0) cdr) PS2) < cdr)
               plan)
            ])
         ]
        [ ((L . x))
         (g '()
            `((,L . ,(- x 1)) . ,PS2)
            `( (,L) . ,plan)
            )
         ]
        [ ((L0 . x0) (L1 . x1) . PS )
         (g PS `((,L0 . ,(- x0 1) )
                 (,L1 . ,(- x1 1) )
                 . ,PS2)
            `( (,L0 ,L1) . ,plan)
            )
         ]
        ))))
