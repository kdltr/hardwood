; Synchronous messaging test

(test-group "RPC"
  (define rpc-server
    (spawn
      (lambda ()
        (let loop ()
          (recv
            ((from tag ('add a b))
             (! from (list tag (+ a b)))))
          (loop)))))

  (define wrong-msg (list (make-tag) 0))
  (! (self) wrong-msg)

  (test 42 (!? rpc-server '(add 21 21) 1 #f))
  (test wrong-msg (? 1 #f)))
