(use srfi-1 srfi-18)

(define no-default (list 'no-default))
(define timeout-condition (make-property-condition 'hardwood-timeout))

(define mailbox-head
  (make-parameter '()))

(define-record hardwood tail lock signal)

(define (setup-thread pid)
  (thread-specific-set! pid
                        (make-hardwood '()
                                       (make-mutex)
                                       (make-condition-variable))))

(define self current-thread)

(define (wait-for-messages timeout)
  (let* ((specific (thread-specific (current-thread)))
         (lock (hardwood-lock specific))
         (signal (hardwood-signal specific)))
    (mutex-lock! lock)
    (if (null? (hardwood-tail specific))
      (if (mutex-unlock! lock signal timeout)
        (wait-for-messages timeout)
        #f)
      (begin
        (mailbox-head (append (mailbox-head)
                              (reverse (hardwood-tail specific))))
        (hardwood-tail-set! specific '())
        (mutex-unlock! lock)
        #t))))

(define (?? pred? #!optional (timeout #f) (default no-default))
  (let ((timeout (if (or (time? timeout) (not timeout))
                   timeout
                   (seconds->time
                     (+ (time->seconds (current-time))
                        timeout)))))
    (receive (head tail) (break pred? (mailbox-head))
      (if (null? tail)
        (if (wait-for-messages timeout)
          (?? pred? timeout default)
          (if (eqv? default no-default)
            (signal timeout-condition)
            default))
        (begin
          (mailbox-head (append head (cdr tail)))
          (car tail))))))

(define (? #!optional (timeout #f) (default no-default))
  (?? (lambda x #t) timeout default))

(define (! pid msg)
  (let* ((specific (thread-specific pid))
         (lock (hardwood-lock specific))
         (signal (hardwood-signal specific)))
    (mutex-lock! lock)
    (hardwood-tail-set! specific
                        (cons msg
                              (hardwood-tail specific)))
    (mutex-unlock! lock)
    (condition-variable-signal! signal))
  msg)

(define (spawn thunk)
  (let ((pid (make-thread thunk)))
    (setup-thread pid)
    (thread-start! pid)
    pid))

