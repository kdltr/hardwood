(use srfi-18 data-structures matchable)

(define mailbox
  (make-parameter #f))

(define lastmail
  (make-parameter '(#t . #f)))

(define ((handle-signal hdl) s)
  (cond
    ((and (pair? s) (eqv? (car s) 'message-send))  (queue-add! (mailbox)
                                                               (cdr s)))
    ((and (symbol? s) (eqv? s 'message-receive))
     (let ((m (mailbox)))
       (if (queue-empty? m)
         (lastmail '(#t . #f))
         (lastmail `(#f . ,(queue-remove! m))))))
    (else  (hdl s))))

(define (install-signal-handler)
  (current-exception-handler (handle-signal (current-exception-handler))))

(define (setup-thread)
  (mailbox (make-queue))
  (install-signal-handler))

(define self current-thread)

(define (?)
  (signal 'message-receive)
  (let ((last (lastmail)))
    (if (car last)
      (begin
        (thread-suspend! (current-thread))
        (?))
      (cdr last))))

(define (! pid msg)
  (thread-signal! pid (cons 'message-send msg))
  msg)

(define (spawn thunk)
  ; FIXME
  ; We need a way to ensure that the custom exception handler is set up
  ; before anybody has the PID of the thread.
  ; One way would be to make a tag in the spawner. The child would send that
  ; tag to its parent, which would receive it with `??` (which is not yet
  ; implemented)
  (let* ((pid (make-thread
                (lambda ()
                  (setup-thread)
                  ; (! parent tag)
                  (thunk)))))
    (thread-start! pid)
    ; (?? (cut eqv? tag <>))
    pid))

; Primordial thread setup
(setup-thread)


; Tests

; Lots-of-threads-and-messages test
(define primordial (self))
(define message-number 100)
(define thread-number 1000)

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

(assert (zero? (queue-length (mailbox))))


; Ping-pong test
(define (pong-server)
  (let ((m (?)))
    (match m
      ((pid 'ping) (! pid 'pong) (pong-server))
      (else (pong-server)))))

(define pong (spawn pong-server))
(thread-sleep! 1) ; make sure the pong thread is set up (see note in (spawn))

(! pong `(,(self) ping))
(assert (eqv? (?) 'pong))

