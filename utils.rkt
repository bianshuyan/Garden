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
        (error 'tell "unknown method [~s] for object [~s]"
               msg (get-id obj)))))
(define make-obj
  (let ()
    (define id* '())
    (define (fresh? id)
      (not (memq id id*)))
    (define (add-id! id)
      (if (symbol? id)
          (if (fresh? id)
              (set! id* (cons id id*))
              (error 'make-obj "id [~s] has been used" id))
          (error 'make-obj "id [~s] should be a symbol" id)))
    (lambda (id)
      (add-id! id)
      (lambda (msg)
        (case msg
          ((id) (lambda (self) id))
          ((type) (lambda (self) '()))
          (else #f))))))
(define (get-id obj) (tell obj 'id))
