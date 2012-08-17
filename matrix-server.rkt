#lang racket

(require xml)
(require net/url)
(require "./matrix-renderer.rkt")

(define usage
  '(html
    (body
     (h1 "matrix-server")
     (p "This is matrix-server, an HTTP server that accepts either a POST\
request with an S-expression defining a 2D table, or a GET request.\
In the former case, matrix-server renders the S-expression into an\
HTML table and returns the HTML code.  In the latter case,\
matrix-server displays this information.")
     (p "the S-expression should have the following format:"
        (pre "
((\"column head 1\"\n\
  \"column head 2\"\n\
  ...\n\
  )\n\
 (\n\
  (\"row head 1\" \"hyperlink 1\" \"column head\" \"column head\" ...)\n\
  (\"row head 2\" \"hyperlink 2\" \"column head\" \"column head\" ...)))\n")))))


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

(define (return-usage in out)
  (respond/ok out)
  (display (xexpr->string usage) out))

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
