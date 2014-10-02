(define (monitors-test)
  (recv
    ('crash  (abort 'some-condition))
    ('exit  (void))))

(define pid1 (spawn monitors-test))
(define pid2 (spawn monitors-test))
(define pid3 (spawn monitors-test))

(define ref1 (monitor pid1))
(define ref2 (monitor pid2))
(define ref3 (monitor pid3))

; Normal exit
(! pid1 'exit)
(assert
  (recv
    (('DOWN ref pid 'exited)  (and (equal? ref ref1)
                                   (equal? pid pid1)))
    (else  #f)
    (after 1 #f)))

; Excptional exit
(! pid2 'crash)
(assert
  (recv
    (('DOWN ref pid ('condition 'some-condition))  (and (equal? ref ref2)
                                                        (equal? pid pid2)))
    (else  #f)
    (after 1 #f)))

; Demonitoring
(demonitor ref3)
(! pid3 'exit)
(assert (? 0.2 #t))

; Dead process
(define ref4 (monitor pid1))
(assert
  (recv
    (('DOWN ref pid 'no-process)  (and (equal? ref ref4)
                                       (equal? pid pid1)))
    (else  #f)
    (after 1 #f)))
