#lang racket

(require net/http-easy)
(require text-table)

(define subscribe-file
  (open-input-file (vector-ref (current-command-line-arguments) 0)))

(define subscribe-packages
  (letrec ([load (λ (acc)
                   (let ([line (read-line subscribe-file)])
                     (if (eof-object? line) acc (cons line (load acc)))))])
    (load '())))

(define anicca-list
  (response-json
   (get
    "https://raw.githubusercontent.com/AOSC-Dev/anicca/main/pkgsupdate.json")))

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

(define result
  (letrec ([search
            (λ (acc ps)
              (if (null? ps)
                  acc
                  (let ([assoc-res (hash-assoc (car ps) 'name anicca-list)])
                    (if assoc-res
                        (search (cons (entry->list assoc-res) acc) (cdr ps))
                        (search acc (cdr ps))))))])
    (fmttable (search '() subscribe-packages))))
