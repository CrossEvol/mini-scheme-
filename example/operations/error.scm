; Example of error usage without actually calling it in the test
(lambda () (error 'my-procedure "Something went wrong"))
;=> #<procedure>