;; Hardwood - An actor-model library for Scheme, inspired by Erlang

;; Copyright (c) 2014, Adrien Ramos <kooda@upyum.com> All rights
;; reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

(use
  data-structures
  matchable
  srfi-1
  srfi-18
  uuid)

(define timeout-condition
  (make-composite-condition (make-property-condition 'exn
                                                     'message "Timeout while receiving")
                            (make-property-condition 'hardwood)
                            (make-property-condition 'timeout)))
(define timeout-condition?
  (conjoin (condition-predicate 'hardwood)
           (condition-predicate 'timeout)))
(define (no-default)
  (signal timeout-condition))

; TODO
; Send a patch for matchable so that it signals an exception instead of using
; (error)
(define (no-match-condition? exn)
  (and (condition? exn)
       (string=? "no matching pattern"
                 (get-condition-property exn 'exn 'message ""))))

(define mailbox-head
  (make-parameter '()))

(define-record pid thread)
(define-record hardwood tail lock signal pid monitors monitors-lock)
(define make-tag uuid-v4)

(define (setup-thread thread)
  (thread-specific-set! thread
                        (make-hardwood '()
                                       (make-mutex)
                                       (make-condition-variable)
                                       (make-pid thread)
                                       '()
                                       (make-mutex))))

(define (process-exist? pid)
  (let ((state (thread-state (pid-thread pid))))
    (not (or (eqv? state 'terminated)
             (eqv? state 'dead)))))

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

; Define `?` as a macro to avoid name-conflict in (match) and (recv) due
; to hygiene
(define-syntax ?
  (syntax-rules ()
    ((?)  (?? any?))
    ((? timeout)  (?? any? timeout))
    ((? timeout default)  (?? any? timeout default))))

(define-syntax recv
  (ir-macro-transformer
    (lambda (expr inject compare)
      (let* ((clauses (cdr expr))
             (timeout-clause (alist-ref 'after clauses compare))
             (timeout (and timeout-clause (car timeout-clause)))
             (timeout-proc (and timeout-clause (cadr timeout-clause)))
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

(define (!? pid msg #!optional (timeout #f) (default no-default))
  (let ((tag (make-tag)))
    (! pid (list (self) tag msg))
    (recv
      ((tag reply)  reply)
      (after timeout (if (eqv? default no-default) (no-default) default)))))

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

(define (!! pids msg)
  (for-each (cut ! <> msg) pids))

(define (alert-monitors reason)
  (let* ((msg (list 'DOWN (self) reason))
         (specific (thread-specific (current-thread)))
         (lock (hardwood-monitors-lock specific))
         (monitors (and (mutex-lock! lock)
                        (hardwood-monitors specific))))
    (!! monitors msg)
    (mutex-unlock! lock)))

(define (monitor-thunk thunk)
  (lambda ()
    (handle-exceptions exn
                       (begin
                         (alert-monitors (list 'condition exn))
                         (abort exn))
      (thunk))
    (alert-monitors 'exited)))

(define (spawn thunk)
  (let ((thread (make-thread (monitor-thunk thunk))))
    (setup-thread thread)
    (thread-start! thread)
    (hardwood-pid (thread-specific thread))))

(define (monitor pid)
  (if (process-exist? pid)
    (let* ((specific (thread-specific (pid-thread pid)))
           (lock (hardwood-monitors-lock specific))
           (monitors (and (mutex-lock! lock)
                          (hardwood-monitors specific))))
      (hardwood-monitors-set! specific (cons (self) monitors))
      (mutex-unlock! lock))
    (! (self) (list 'DOWN pid 'no-process)))
  pid)

