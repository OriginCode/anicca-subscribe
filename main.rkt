#lang racket/base

(require racket/cmdline
         racket/contract
         racket/list
         racket/port
         racket/string)
(require net/http-easy)
(require text-table)
(require json)

(require "convert.rkt")
(require "lib.rkt")

(define/contract (subscription-file path)
  (-> path-string? input-port?)
  (let ([exn->user-error (λ (e) (raise-user-error (exn-message e)))])
    (with-handlers ([exn:fail:filesystem:errno? exn->user-error]
                    [exn:fail:filesystem? exn->user-error])
      (open-input-file path #:mode 'text))))

(define/contract (format-table res)
  (-> (listof (listof string?)) void?)
  (print-table #:row-sep? '(#t #f ...)
               #:col-sep? '(#t #f ...)
               (cons '(Name Category Before After Warnings) res)))

(define/contract package-names
  (parameter/c (listof string?))
  (make-parameter null))
(define/contract local-data
  (parameter/c jsexpr?)
  (make-parameter #f))
(define/contract roll?
  (parameter/c boolean?)
  (make-parameter #f))

(define (cli)
  (command-line #:program "anicca-subscribe"
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
                 (package-names (string-split packages ","))]
                [("-r" "--roll")
                 "Roll 10 packages"
                 (roll? #t)]))

(cli)

(define/contract anicca-data
  (listof (listof string?))
  (convert (if (local-data) (local-data) (online-data))))

(cond
  [(roll?)
   (define roll-result (take (shuffle anicca-data) (min 10 (length anicca-data))))
   (format-table (sort roll-result string<? #:key car))]
  [else
   (define search-result (search (package-names) anicca-data))
   (if (null? search-result)
       (exit)
       (format-table (sort search-result string<? #:key car)))])
