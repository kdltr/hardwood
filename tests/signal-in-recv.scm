(define pid
  (monitor
    (spawn
      (lambda ()
        (recv (x  (signal 'foo)))))))

(! pid 'm)
(assert
  (recv
    (('DOWN (? (cut eqv? pid <>)) ('condition 'foo))  #t)
    (else  #f)
    (after 1 #f)))
