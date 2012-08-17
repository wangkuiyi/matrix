#lang racket

(require xml)
(require net/url)
(require "./matrix-renderer.rkt")

(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (loop)))

(define (accept-and-handle listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread (lambda ()
              (handle in out)
              (close-input-port in)
              (close-output-port out))))
  ; Watcher thread:
  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))

(define (handle in out)
  (let* ((head-fields (regexp-match #rx"([A-Z]+) ([^ ]+)" in))
         (method (second head-fields))
         (url (third head-fields)))
    (regexp-match #rx"(\r\n|^)\r\n" in)
    (cond ((bytes=? method #"POST") (handle-post-request in out))
          (else (return-usage in out)))))

(define (respond/ok out)
  (display "HTTP/1.1 200 Okay\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out))

(define (respond/bad-request out)
  (display "HTTP/1.1 400 Bad Request\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out))

(define usage "How to use this server.")

(define (return-usage in out)
  (respond/ok out)
  (display (xexpr->string `(html (body ,usage))) out))

(define (allowed? expr);; Filter out illegal requests here
  #t)

(define (handle-post-request in out)
  (let ((expr (read in)))
    (cond ((allowed? expr)
           (respond/ok out)
           (output-matrix-as-html (first expr) (second expr) out))
          (else
           (respond/bad-request out)))))

(serve 9988)
