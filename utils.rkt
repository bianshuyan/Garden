#lang racket
(provide (all-defined-out))
(define valid-char*
  (string->list
   " -_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
(define (valid-char? char)
  (if (memq char valid-char*) #t #f))
(define (purify-string str)
  (define l (string-length str))
  (define s (make-string l))
  (let iter ((i 0))
    (cond ((= i l) s)
          ((valid-char? (string-ref str i))
           (string-set! s i (string-ref str i))
           (iter (+ i 1)))
          (else
           (string-set! s i #\space)
           (iter (+ i 1))))))
(define (read-command)
  (let* ((str (read-line))
         (purified-str (purify-string str))
         (port (open-input-string purified-str))
         (seq (in-port read port)))
    (sequence->list seq)))
;<command> ::= <verbA>
;           |  <verbB> <obj>
;           |  <verbC> <obj> <prep> <obj>
(define (get-method obj msg) (obj msg))
(define (method? x) (procedure? x))
(define (tell obj msg . arg*)
  (let ((method (get-method obj msg)))
    (if (method? method)
        (apply method obj arg*)
        (error 'tell "unknown method ~s" msg))))
