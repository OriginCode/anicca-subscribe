#lang racket

(require net/http-easy)
(require text-table)

(define (exn->user-error e)
  (raise-user-error (exn-message e)))

(define subscription-file
  (let ([args (current-command-line-arguments)])
    (if (> (vector-length args) 0)
        (with-handlers ([exn:fail:filesystem:errno? exn->user-error]
                        [exn:fail:filesystem? exn->user-error])
          (open-input-file (vector-ref args 0) #:mode 'text))
        (raise-user-error 'anicca-subscribe "no subscription file provided"))))

(define subscribe-packages
  (letrec ([load (λ (acc)
                   (let ([line (read-line subscription-file)])
                     (if (eof-object? line) acc (cons line (load acc)))))])
    (load '())))

(define anicca-list
  (response-json
   (let ([res
          (get
           "https://raw.githubusercontent.com/AOSC-Dev/anicca/main/pkgsupdate.json")])
     (if (= (response-status-code res) 200)
         res
         (raise-user-error 'anicca-subscribe
                           "failed to fetch anicca package update list")))))

(define (hash-assoc v key hs)
  (if (null? hs)
      #f
      (if (equal? v (hash-ref (car hs) key))
          (car hs)
          (hash-assoc v key (cdr hs)))))

(define (entry->list e)
  (list (hash-ref e 'name)
        (hash-ref e 'path)
        (hash-ref e 'before)
        (hash-ref e 'after)
        (let ([warnings (hash-ref e 'warnings)])
          (if (null? warnings) "N/A" warnings))))

(define (fmttable res)
  (print-table #:row-sep? '(#t #f ...)
               #:col-sep? '(#t #f ...)
               (cons '(Name Path Before After Warnings) res)))

(define (search acc ps)
  (if (null? ps)
      acc
      (let ([assoc-res (hash-assoc (car ps) 'name anicca-list)])
        (if assoc-res
            (search (cons (entry->list assoc-res) acc) (cdr ps))
            (search acc (cdr ps))))))

(fmttable (sort (search '() subscribe-packages) string<? #:key car))
