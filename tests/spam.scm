; Lots-of-threads-and-messages test

(test-group "Spam"
  (define message-number 100)
  (define thread-number 100)

  (define (spam who)
    (let loop ((i message-number))
      (unless (zero? i)
        (! who i)
        (loop (sub1 i)))))

  (let loop ((i thread-number))
    (unless (zero? i)
      (spawn spam (self))
      (loop (sub1 i))))

  (let loop ((i (* thread-number message-number)))
    (unless (zero? i)
      (?)
      (loop (sub1 i))))

  (test 'ok (? 1 'ok)))

