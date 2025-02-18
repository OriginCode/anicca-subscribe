#lang racket/base

(require racket/contract)
(require net/http-easy)
(require json)

(provide (all-defined-out))

(define *pkgsupdate-json-url*
  "https://raw.githubusercontent.com/AOSC-Dev/anicca/main/pkgsupdate.json")   

(define/contract (online-data)
  (-> jsexpr?)
  (response-json
   (let ([res
          (get
           *pkgsupdate-json-url*)])
     (unless (= (response-status-code res) 200)
       (raise-user-error 'anicca-subscribe "failed to fetch anicca package update list"))
     res)))

(define/contract (search ps anicca-data)
  (-> (listof string?) (listof (listof string?)) (listof (listof string?)))
  (foldl (lambda (p acc)
           (define entry (assoc p anicca-data))
           (if entry
               (cons entry acc)
               acc))
         '()
         ps))
