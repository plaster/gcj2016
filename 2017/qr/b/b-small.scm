;; runs on Gauche 0.9.5: http://practical-scheme.net/gauche/
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
  (line-read)
  )

(define (tidy? N)
  (cond
    [ (< N 10)
     #t ]
    [else
      (let* [[ x0 (modulo N 10) ]
             [ N (quotient N 10) ]
             [ x1 (modulo N 10) ]
             ]
        (and (<= x1 x0)
             (tidy? N) ) ) ] ) )

(define (solve N)
  (let loop [[ N N ]]
    (if (tidy? N)
      N
      (loop (- N 1)))))
