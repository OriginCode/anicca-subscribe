#lang racket

(require json)

(define (convert pkgsupdate)
  (map (λ (pkg)
         (list (hash-ref pkg 'name)
               (car (string-split (hash-ref pkg 'path) "/"))
               (hash-ref pkg 'before)
               (hash-ref pkg 'after)
               (let ([warnings (hash-ref pkg 'warnings)])
                 (if (null? warnings)
                   "N/A"
                   (if (> (length warnings) 1) (cadr warnings) "N/A")))))
       pkgsupdate))

(provide convert)
