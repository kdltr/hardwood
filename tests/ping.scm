(use matchable)

(include "hardwood")

; Ping-pong test
(define (pong-server)
  (let ((m (?)))
    (match m
      ((pid 'ping)  (! pid 'pong)
                    (pong-server))
      (else  (pong-server)))))

(define pong (spawn pong-server))

(! pong `(,(self) ping))
(assert (eqv? (?) 'pong))

