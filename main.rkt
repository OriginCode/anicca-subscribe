#lang racket

(require racket/cmdline)
(require net/http-easy)
(require text-table)
(require json)

(require "convert.rkt")

(define/contract (subscription-file path)
  (-> path-string? input-port?)
  (let ([exn->user-error (λ (e) (raise-user-error (exn-message e)))])
    (with-handlers ([exn:fail:filesystem:errno? exn->user-error]
                    [exn:fail:filesystem? exn->user-error])
      (open-input-file path #:mode 'text))))

(define/contract (online-data)
  (-> jsexpr?)
  (response-json
   (let ([res
          (get
           "https://raw.githubusercontent.com/AOSC-Dev/anicca/main/pkgsupdate.json")])
     (if (= (response-status-code res) 200)
         res
         (raise-user-error 'anicca-subscribe
                           "failed to fetch anicca package update list")))))

(define/contract (format-table res)
  (-> (listof (listof string?)) void?)
  (print-table #:row-sep? '(#t #f ...)
               #:col-sep? '(#t #f ...)
               (cons '(Name Category Before After Warnings) res)))

(define/contract (search ps anicca-data)
  (-> (listof string?) (listof (listof string?)) (listof (listof string?)))
  (foldl (lambda (p acc)
           (let ([entry (assoc p anicca-data)])
             (if entry (cons entry acc) acc)))
         '()
         ps))

(define/contract package-names
  (parameter/c (listof string?))
  (make-parameter null))
(define/contract local-data
  (parameter/c jsexpr?)
  (make-parameter #f))

(define (cli)
  (command-line #:program "Anicca Subscribe"
                #:once-each
                [("-l" "--local")
                 path
                 "Use a local aosc-findupdate json file"
                 (local-data (read-json (open-input-file path)))]
                #:once-any
                [("-f" "--file")
                 path
                 "Use a subscription file"
                 (package-names (port->lines (subscription-file path)))]
                [("-p" "--packages")
                 packages
                 "Use package names"
                 (package-names (string-split packages ","))]))

(cli)

(define/contract anicca-data
  (listof (listof string?))
  (convert (if (local-data) (local-data) (online-data))))

(let ([search-result (search (package-names) anicca-data)])
  (if (null? search-result)
      (exit)
      (format-table (sort search-result string<? #:key car))))
