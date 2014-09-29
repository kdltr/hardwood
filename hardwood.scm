(use srfi-1 srfi-18 matchable data-structures)

(define timeout-condition
  (make-composite-condition (make-property-condition 'exn
                                                     'message "Timeout while receiving")
                            (make-property-condition 'hardwood)
                            (make-property-condition 'timeout)))
(define (hardwood-timeout-condition? exn)
  (and ((condition-predicate 'hardwood) exn)
       ((condition-predicate 'timeout) exn)))
(define (no-default)
  (signal timeout-condition))

; TODO
; Send a patch for matchable so that it signals an exception instead of using
; (error)
(define (no-match-condition? exn)
  (string=? "no matching pattern"
            (get-condition-property exn 'exn 'message "")))

(define mailbox-head
  (make-parameter '()))

(define-record pid thread)

(define-record hardwood tail lock signal pid)

(define (setup-thread thread)
  (thread-specific-set! thread
                        (make-hardwood '()
                                       (make-mutex)
                                       (make-condition-variable)
                                       (make-pid thread))))

(define (self)
  (hardwood-pid (thread-specific (current-thread))))

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

(define (rcv-msg proc timeout timeout-proc)
  (let ((timeout (if (or (not timeout) (time? timeout))
                   timeout
                   (seconds->time
                     (+ (time->seconds (current-time))
                        timeout)))))
    (let loop ((prev '()))
      (let* ((head (mailbox-head))
             (empty? (null? head))
             (msg (or empty? (car head))))
        (if empty?
          (if (wait-for-messages timeout)
            (loop prev)
            (begin
              (mailbox-head (append (reverse prev)
                                    head))
              (timeout-proc)))
          (begin
            (mailbox-head (cdr head))
            (receive (ok? . vals)
                     (handle-exceptions exn
                                        (begin
                                          (mailbox-head (append (reverse prev)
                                                                (cons msg
                                                                      (mailbox-head))))
                                          (abort exn))
                       (proc msg))
                     (if ok?
                       (begin
                         (mailbox-head (append (reverse prev)
                                               (mailbox-head)))
                         (apply values vals))
                       (loop (cons msg prev))))))))))

(define (?? pred? #!optional (timeout #f) (default no-default))
  (rcv-msg (lambda (m) (values (pred? m) m))
           timeout
           (if (eqv? default no-default)
             no-default
             (lambda () default))))

(define (? #!optional (timeout #f) (default no-default))
  (?? any? timeout default))

(define-syntax recv
  (ir-macro-transformer
    (lambda (expr inject compare)
      (let* ((clauses (inject (cdr expr)))
             (timeout-clause (assoc 'after clauses))
             (timeout (and timeout-clause (cadr timeout-clause)))
             (timeout-proc (and timeout-clause (caddr timeout-clause)))
             (clauses (remove (lambda (e) (eqv? (car e) 'after)) clauses)))
        `(rcv-msg (lambda (m)
                    (handle-exceptions exn
                                       (if (no-match-condition? exn)
                                         #f
                                         (abort exn))
                      (apply values (cons #t
                                          (receive (match m
                                                          ,@clauses))))))
                  ,timeout
                  (lambda () ,timeout-proc))))))

(define (! pid msg)
  (let* ((specific (thread-specific (pid-thread pid)))
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
  (let ((thread (make-thread thunk)))
    (setup-thread thread)
    (thread-start! thread)
    (hardwood-pid (thread-specific thread))))

