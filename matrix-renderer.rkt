#lang racket

(require xml)

(define (normalize-module-name module)
  (list->string
   (map (lambda (ch)
          (cond ((char-whitespace? ch) #\_)
                ((char-numeric? ch) ch)
                ((char-alphabetic? ch) ch)
                (else #\_)))
        (string->list (string-downcase module)))))

(define (module-name<? m1 m2)
  (string<? (normalize-module-name m1)
            (normalize-module-name m2)))

(define (make-module-names-thead modules)
  `(thead
    (tr (th)
        ,@(map
           (lambda (module)
             `(th
               (span (span (span
                            (a ((href
                                 ,(format "modules/~a.html"
                                          (normalize-module-name module))))
                               ,(format "~a" module)))))))
           (sort modules module-name<?)))))

;; Given a sorted list of modules (belonging to a feature) and the
;; sorted list of all modules (used to generate the table header),
;; this function outputs an xexpr which corresponds to a sequence of
;; <td/> elements, where each element is an empty table cell or
;; contains either a yes sign.
;;
(define (make-row-tds ml gml xexpr)
  (cond ((empty? gml) xexpr)
        ((or (empty? ml)
             (string>? (car ml) (car gml)))
         (make-row-tds ml (cdr gml) (append xexpr '((td)))))
        ((string=? (car ml) (car gml))
         (make-row-tds (cdr ml) (cdr gml) (append xexpr '((td "\u2713")))))
        ((string<? (car ml) (car gml))
         (make-row-tds (cdr ml) (cdr gml)
                       (append
                        xexpr
                        `((td ,(format "Unknown module:~a" (car ml)))))))))

(define (make-feature-tr feature sorted-module-list)
  (let ((readable (car feature))
        (url (cadr feature))
        (deps (cddr feature)))
    `(tr
      (th ((scope "row"))
          (a ((href ,(format "features/~a.html" url)))
             ,(format "~a" readable)))
      ,@(make-row-tds (sort (map normalize-module-name deps) module-name<?)
                      sorted-module-list '()))))

(define (output-matrix-as-html modules features out)
  (let ((sorted-module-list
         (sort (map normalize-module-name modules) module-name<?)))
    (display
     (xexpr->string
      `(table
        ,(make-module-names-thead modules)
        (tbody
         ,@(map (lambda (fl)
                  (make-feature-tr fl sorted-module-list))
                features))))
     out)))

(provide output-matrix-as-html)
