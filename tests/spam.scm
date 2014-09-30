; Lots-of-threads-and-messages test
(define primordial (self))
(define message-number 100)
(define thread-number 100)

(define (spam)
  (let loop ((i message-number))
    (unless (zero? i)
      (! primordial i)
      (loop (sub1 i)))))

(let loop ((i thread-number))
  (unless (zero? i)
    (spawn spam)
    (loop (sub1 i))))

(let loop ((i (* thread-number message-number)))
  (unless (zero? i)
    (?)
    (loop (sub1 i))))

(assert (not (? 1 #f)))

