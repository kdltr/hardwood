(include "hardwood-impl")

; Make the primordial thread a hardwood thread, practical for testing
(setup-thread (current-thread))

(define rpc-server
  (spawn
    (lambda ()
      (let loop ()
        (recv
          ((from tag (’add a b))
           (! from (list tag (+ a b)))))
        (loop)))))

(assert (= (!? rpc-server '(add 21 21)) 42))
