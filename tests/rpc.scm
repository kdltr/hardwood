; Synchronous messaging test
(define rpc-server
  (spawn
    (lambda ()
      (let loop ()
        (recv
          ((from tag ('add a b))
           (! from (list tag (+ a b)))))
        (loop)))))

(! (self) (list (make-tag) 0))
(assert (= (!? rpc-server '(add 21 21)) 42))
(?)
