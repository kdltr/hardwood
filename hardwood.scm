(use srfi-1 srfi-18)

(define mailbox-head
  (make-parameter '()))

(define-record hardwood tail lock signal)

(define mailbox-tail
  (make-parameter '()))

(define (setup-thread pid)
  (let ((signal (make-mutex)))
    (mutex-lock! signal)
    (thread-specific-set! pid
                          (make-hardwood '()
                                         (make-mutex)
                                         signal))))

(define self current-thread)

(define (wait-for-messages)
  (let* ((specific (thread-specific (current-thread)))
         (lock (hardwood-lock specific))
         (signal (hardwood-signal specific)))
    (mutex-lock! lock)
    (when (null? (hardwood-tail specific))
      (mutex-unlock! lock)
      (mutex-lock! signal)
      (mutex-lock! lock))
    (mailbox-head (append (mailbox-head)
                          (reverse (hardwood-tail specific))))
    (hardwood-tail-set! specific '())
    (mutex-unlock! lock)))

(define (?)
  (let ((head (mailbox-head)))
    (cond
      ((null? head)  (wait-for-messages)
                     (?))
      (else  (mailbox-head (cdr head))
             (car head)))))

(define (?? pred?)
  (receive (head tail) (break pred? (mailbox-head))
    (if (null? tail)
      (begin
        (wait-for-messages)
        (?? pred?))
      (begin
        (mailbox-head (append head (cdr tail)))
        (car tail)))))

(define (! pid msg)
  (let* ((specific (thread-specific pid))
         (lock (hardwood-lock specific))
         (signal (hardwood-signal specific)))
    (mutex-lock! lock)
    (hardwood-tail-set! specific
                        (cons msg
                              (hardwood-tail specific)))
    (mutex-unlock! lock)
    (mutex-unlock! signal))
  msg)

(define (spawn thunk)
  (let ((pid (make-thread thunk)))
    (setup-thread pid)
    (thread-start! pid)
    pid))

; Primordial thread setup
(setup-thread (self))

