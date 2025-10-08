(define (parse-json str)
  (call-with-values
      (lambda () (parse-value (string-trim-left str)))
    (lambda (value rest)
      (+ (string-length value) 2))))
	  
(define (parse-value str) (values str 0))

(define (string-trim-left str)
  (let loop ((i 0))
    (if (or (>= i (string-length str)) (not (char-whitespace? (string-ref str i))))
        (substring str i (string-length str))
        (loop (+ i 1)))))

(parse-json "hello, world!")