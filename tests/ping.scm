(include "hardwood-impl")

; Make the primordial thread a hardwood thread, practical for testing
(setup-thread (current-thread))

; Ping-pong test
(define (pong-server)
  (recv
    (((? pid? pid) 'ping)  (! pid 'pong)
                           (pong-server))
    (else  (pong-server))))

(define pong (spawn pong-server))

(! pong `(,(self) ping))
(assert (eqv? (? 1 'fail) 'pong))

