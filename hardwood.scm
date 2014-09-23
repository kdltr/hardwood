(use srfi-1 srfi-18)

(define mailbox-head
  (make-parameter '()))

(define mailbox-tail
  (make-parameter '()))

(define ((handle-signal hdl) s)
  (cond
    ((and (pair? s) (eqv? (car s) 'message-send))  (mailbox-tail
                                                     (cons (cdr s)
                                                           (mailbox-tail))))
    ((and (symbol? s) (eqv? s 'message-receive))
     (mailbox-head (append (mailbox-head)
                           (reverse (mailbox-tail))))
     (mailbox-tail '()))
    (else  (hdl s))))

(define (install-signal-handler)
  (current-exception-handler (handle-signal (current-exception-handler))))

(define (setup-thread)
  (install-signal-handler))

(define self current-thread)

(define (wait-for-messages)
  (signal 'message-receive)
  (when (null? (mailbox-head))
    (thread-suspend! (current-thread))))

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
  (thread-signal! pid (cons 'message-send msg))
  msg)

(define (spawn thunk)
  (let* ((m (make-mutex))
         (pid (make-thread
                (lambda ()
                  (with-exception-handler
                    (handle-signal (current-exception-handler))
                    (lambda ()
                      (mutex-unlock! m)
                      (thunk)))))))
    (mutex-lock! m)
    (thread-start! pid)
    ; wait for the thread to be ready before returning its pid
    (mutex-lock! m)
    pid))

; Primordial thread setup
(setup-thread)

