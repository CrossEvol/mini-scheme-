(define make-counter
  (lambda (initial-count)
    (lambda ()
      (set! initial-count (+ initial-count 1))
      initial-count)))

(define counter-a (make-counter 10))

(+ (counter-a) (counter-a) )
;=> 11