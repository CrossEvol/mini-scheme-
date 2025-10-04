(define my-adder
  (lambda (a b)
    "这是一个接收两个数字并返回它们的和的 lambda 函数。"
    (+ a b)))

(let ((f (lambda (x)
           "计算一个数的平方。"
           (* x x))))
  (f 5)) 
  
;=>25