(define ht (make-hashtable string-hash string=?))
(hashtable-set! ht "key" "value")
ht
;=> #<hashtable>