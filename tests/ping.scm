; Ping-pong test
(define (pong-server)
  (recv
    (((? pid? pid) 'ping)  (! pid 'pong)
                           (pong-server))
    (else  (pong-server))))

(define pong (spawn pong-server))

(! pong `(,(self) ping))
(assert (eqv? (?? any? 1 'fail) 'pong))

