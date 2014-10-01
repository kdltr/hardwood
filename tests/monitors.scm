(define (monitors-test)
  (recv
    ('crash  (/ 0))
    ('exit  (void))))

(define pid1 (spawn monitors-test))
(define pid2 (spawn monitors-test))

(monitor pid1)
(monitor pid2)

(! pid1 'exit)
(assert (recv (('DOWN pid1 'exited)  #t) (else  #f) (after 1 #f)))

(! pid2 'crash)
(assert (recv (('DOWN pid1 ('condition exn))  #t) (else  #f) (after 1 #f)))

(monitor pid1)
(assert (recv (('DOWN pid1 'no-process)  #t) (else  #f) (after 1 #f)))
