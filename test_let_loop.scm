(let loop ((x 0))
  (if (< x 5)
      (loop (+ x 1))
      x))