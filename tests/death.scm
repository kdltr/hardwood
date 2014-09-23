(include "hardwood")

(define pid
  (spawn (lambda ()
           (print (append (mailbox-head)
                          (reverse (mailbox-tail)))))))

(! pid 'foo)
(thread-sleep! 1)
(! pid 'bar)
