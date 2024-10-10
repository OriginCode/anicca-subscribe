#lang typed/racket/base

(require racket/match
         racket/string)

(: convert
   :
   (Listof (HashTable Symbol (U String (Listof String))))
   ->
   (Listof (Listof String)))
(define (convert pkgsupdate)
  (map
   (λ ([pkg : (HashTable Symbol (U String (Listof String)))])
     (match pkg
       [(hash 'name name
              'path path
              'before before
              'after after
              'warnings warnings)
        (cast (list name
                    (if (string? path) (car (string-split path "/")) "N/A")
                    before
                    after
                    (if (list? warnings)
                        (if (null? warnings)
                            "N/A"
                            (if (> (length warnings) 1) (cadr warnings) "N/A"))
                        "N/A"))
              (Listof String))]))
   pkgsupdate))

(provide convert)
