(let loop ((i 0) (acc 0)) (if (< i 5) (loop (+ i 1) (+ acc i)) acc))
;=> 10