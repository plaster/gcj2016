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

(define (emit y)
  (format #t "Case #~a: ~a\n"
          (current-case)
          (or y 'IMPOSSIBLE)
          ))

(define (parse)
  (rxmatch-case (read-line)
    [ #/^([+-]+) ([0-9]+)$/ (#f S-str K-str)
      (values (map (.$ string->symbol string) (string->list S-str))
              (string->number K-str))
      ]
    [else
      (errorf "parse error at case ~s" (current-case))
      ]
    ) )

(define (happy S)
  (map (^_ '+) S))

(define (flip S K)
  (match `#(,S ,K)
    [ #( _ 0 ) S ]
    [ #( () _) #f ]
    [ #( (S0 . SS) K)
      (and-let* [[ XX (flip SS (- K 1)) ]]
        (cons (case S0
                [ (-) '+ ]
                [ (+) '- ]
                [else (errorf "unknown: ~s" S0)] )
              XX )) ]
    ))

(define (solve S K)
  (let loop [[ S S ]
             [ X (happy S) ]
             [ y 0 ]
             ]
    (match `#( ,S ,X )
      [ #( () () ) y ]
      [ #( (S0 . SS) (H0 . HH) )
        (cond
          [ (eq? S0 H0)
           (loop SS HH y) ]
          [else
            (and-let* [[ HH (flip HH (- K 1)) ]]
              (loop SS HH (+ y 1))
              ) ]
          ) ] ) ) )
