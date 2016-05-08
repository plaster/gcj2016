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

(define (emit . xs)
  (format #t "Case #~a: ~a\n"
          (current-case)
          (string-join (map x->string xs) " ")
          ))

(define (parse)
  (let* [[N (line-read)]
         [PS (line-read (replist$ N read))
             ]
         ]
    (values N PS)))

(define (solve . args)
  args
  )
