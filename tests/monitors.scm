(test-group "Monitors"

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
  (test "normal exit"
    (list 'DOWN ref1 pid1 'exited)
    (? 1 #f))

  ; Excptional exit
  (! pid2 'crash)
  (test "exit with condition"
    (list 'DOWN ref2 pid2 (list 'condition 'some-condition))
    (? 1 #f))

  ; Demonitoring
  (demonitor ref3)
  (! pid3 'exit)
  (test "demonitoring"
    #t
    (? 0.2 #t))

  ; Dead process
  (define ref4 (monitor pid1))
  (test "dead process"
    (list 'DOWN ref4 pid1 'no-process)
    (? 1 #f)))
