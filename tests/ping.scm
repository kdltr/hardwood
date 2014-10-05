; Ping-pong test

(test-group "Ping"

  (define (pong-server)
    (recv
      (((? pid? pid) 'ping)  (! pid 'pong)
                           (pong-server))
      (else  (pong-server))))

  (define pong (spawn pong-server))

  (! pong `(,(self) ping))
  (test 'pong (? 1 #f)))

