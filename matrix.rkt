#lang racket

(require xml)

(define modules
  '("UDP server"
    "frontend"
    "mixer"
    "index"
    "pCTR server"
    "pCTR training"
    "pCTR expr platform"
    "user server"
    "realtime user server"
    "user pipeline"
    "page server"
    "updater"
    "site_updater"
    "bigtable"
    "user agent"
    "interest server"
    "introspector server"
    "log server"
    "click server"
    "js log server"
    "accounter"
    "LDA"
    "classifiers"
    "keyword extractor"
    "text miner"
    "touchstone"
    "auto import blob"
    "auction log"
    "query log"
    "click log"))

(define features
  '(("广告投放" "targeting" "index" "mixer" "frontend")
    ("  广告bidding" "bidding" "mixer")
    ("  广告位和虚拟广告位" "virtual_place" "frontend" "mixer")
    ("广告请求" "request")
    ("检索条件构造" "query_build")
    ("广告组触发和过滤" "retrieval_and_filtering" "index" "mixer" "user server" "page server")
    ("  bigtable" "retrieval_bigtable")
    ("  index" "retrieval_index")
    ("  随机触发" "retrieval_random")
    ("  过滤" "filtering")
    ("广告组的排序和计费" "ranking_and_pricing" "pCTR server" "pCTR training")
    ("  创意选择" "creative_selection")
    ("  广告的ranking" "ranking")
    ("  广告去重" "deduplication")
    ("  广告的pricing" "pricing")
    ("广告补余" "complement")
    ("广告展现" "impression")
    ("广告点击" "click")
    ("消耗速度控制" "control_cost_rate")
    ("特定广告的promotion" "promotion")
    ("广告预览" "preview")
    ("大表相关逻辑" "bigtable")
    ("电商优化" "ebusiness_optimization")
    ("用用户信息触发和排序特定垂直领域广告" "vertical_ads_domain")
    ("exploration and exploitation" "exploration_and_exploitation")
    ("基于budget的展示控制逻辑" "impression_probability_by_budget")
    ("landing page的处理" "landing_page_processing")
    ("ip到地域的映射" "ip_to_geo_mapping")
    ("一定比例的流量上只出拍拍广告" "paipai_only_traffic")
    ("联调环境" "integrated_test_system")
    ("index白板" "index_whiteboard")
    ("预发布环境" "prelaunch_test_system")))

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

(define css "\
    body {\n\
      font: normal normal normal Calibri, sans-serif;\n\
      margin: auto;\n\
      width: 90%;\n\
    }\n\
    table {\n\
      border-collapse: collapse;\n\
      /* Illustrate the table space with color */\n\
      background-color: silver;\n\
      /* Screen real estate for our headers */\n\
      margin-top: 150px;\n\
    }\n\
    th {\n\
      /* Must text-align left in order to rotate in the right corner */\n\
      text-align: left;\n\
      padding: 0;\n\
    }\n\
    td {\n\
      border: 1px solid;\n\
      padding: 0;\n\
    }\n\
    tbody th {\n\
      border: 1px solid;\n\
      padding: 0 4px;\n\
      width: 320px;\n\
    }\n\
    /* Outer span is just a reference for the inner span */\n\
    th > span {\n\
      position: relative;\n\
    }\n\
    th > span > span {\n\
      /* Must remove span from normal flow in order to */\n\
      /* keep columns from widening */\n\
      position: absolute;\n\
      left: -0.5px;\n\
        /*\n\
          So far only Firefox does subpixel positioning =\n\
            ignored by Opera, Chrome and Safari .\n\
          But they got this right (visually) in the first place.\n\
          This rule puts the rotated span exactly in place for Firefox\n\
          (tested on Linux and Windows July 2010)\n\
        */\n\
      white-space: nowrap;\n\
      -moz-transform: rotate(-65deg) skewX(25deg);\n\
      -o-transform: rotate(-65deg) skewX(25deg);\n\
      -webkit-transform: rotate(-65deg) skewX(25deg);\n\
      -moz-transform-origin: 0% 0%;\n\
      -o-transform-origin: 0% 0%;\n\
      -webkit-transform-origin: 0% 0%;\n\
      border: 1px solid;\n\
      padding: 0;\n\
      height: 23px; /* 1.3 em = 23px in Opera and */\n\
                    /* 23.4 in FFox (1em = 18px) */\n\
      width: 160px;\n\
      /* Illustrate where it's at with color */\n\
      background-color: yellow;\n\
      /* If one wants centered text, this is the place to reset it */\n\
      /* From a design point of view it might not be desirable */\n\
      text-align: center;\n\
    }\n\
    th > span > span > span {\n\
      /* Rotate the text back, so it will be easier to read */\n\
      -moz-transform: skewX(-30deg);\n\
      -o-transform: skewX(-25deg);\n\
      /* Safari and Chrome won't skew back, so the next line is actually*/\n\
      /* redundant right now (checked July 2010 on Linux) */\n\
      -webkit-transform: skewX(-30deg);\n\
    }\n\
    td {\n\
      padding: 1px;\n\
      text-align: right;\n\
      width: 23px;\n\
    }\n\
    td.yes {\n\
      background-color:cyan\n\
    }")

(define css-placeholder "===css===")

(define description
  "情境广告系统是一个后台广告服务系统。多个模块（RPC servers）构成。因为\n\
  情境广告系统支持多种流量和多种广告，而不同类型的流量和广告需要做不同\n\
  类型的处理，所以系统中功能繁多。每个系统功能可能涉及多个模块。\n\
  为了便于跟踪各功能和模块之间的关系，保证系统的可维护性，下表列出了所\n\
  有模块和功能，以及他们之间的关系。")

(define (output-matrix-as-html)
  (let ((sorted-module-list
         (sort (map normalize-module-name modules) module-name<?)))
    (display
     ;; To avoid xexpr->string from escaping CSS code, we insert a
     ;; placeholder (without escape characters) in the xexpr, and
     ;; regexp-replace the placeholder into CSS code after invoking
     ;; xexpr->string.
     (regexp-replace css-placeholder
                     (xexpr->string
                      `(html
                        (head
                         (meta ((charset "utf-8")))
                         (title "情境广告系统功能和模块")
                         (style ,css-placeholder))
                        (body
                         (h1 "情境广告系统功能和模块")
                         (p ,description)
                         (table
                          ,(make-module-names-thead modules)
                          (tbody
                           ,@(map (lambda (fl)
                                    (make-feature-tr fl sorted-module-list))
                                  features))))))
                     css))))

(output-matrix-as-html)
