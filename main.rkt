#lang racket

(require racket/cmdline)
(require net/http-easy)
(require text-table)

(define (subscription-file path)
  (let ([exn->user-error (λ (e) (raise-user-error (exn-message e)))])
    (with-handlers ([exn:fail:filesystem:errno? exn->user-error]
                    [exn:fail:filesystem? exn->user-error])
      (open-input-file path #:mode 'text))))

(define anicca-data
  (response-json
   (let ([res
          (get
           "https://raw.githubusercontent.com/AOSC-Dev/anicca/main/anicca-data.json")])
     (if (= (response-status-code res) 200)
         res
         (raise-user-error 'anicca-subscribe
                           "failed to fetch anicca package update list")))))

;; Original entry list '(Name Before After Category Timestamp Warnings)
;; Reorder to '(Name Category Before After Warnings)
(define (format-entry e)
  (list (first e)
        (fourth e)
        (second e)
        (third e)
        (let ([warnings (sixth e)])
          (if (null? warnings)
              "N/A"
              (if (> (length warnings) 1) (cadr warnings) "N/A")))))

(define (format-table res)
  (print-table #:row-sep? '(#t #f ...)
               #:col-sep? '(#t #f ...)
               (cons '(Name Category Before After Warnings) res)))

(define (search ps)
  (foldl (lambda (p acc)
           (let ([entry (assoc p anicca-data)])
             (if entry (cons (format-entry entry) acc) acc)))
         '()
         ps))

(define package-names (make-parameter null))

(define (cli)
  (command-line #:program "Anicca Subscribe"
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

(let ([search-result (search (package-names))])
  (if (null? search-result)
      (exit)
      (format-table (sort (search (package-names)) string<? #:key car))))
