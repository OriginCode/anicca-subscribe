#lang racket/base

(require racket/contract
         racket/match
         racket/string)

(define/contract (convert pkgsupdate)
  (-> (listof (hash/c symbol? (or/c string? (listof string?))))
      (listof (listof string?)))
  (map
   (λ (pkg)
     (match pkg
       [(hash 'name name
              'path path
              'before before
              'after after
              'warnings warnings)
        (list name
              (if (string? path) (car (string-split path "/")) "N/A")
              before
              after
              (if (list? warnings)
                  (if (null? warnings)
                      "N/A"
                      (if (> (length warnings) 1) (cadr warnings) "N/A"))
                  "N/A"))]))
   pkgsupdate))

(provide (contract-out [convert
                        (-> (listof (hash/c symbol? (or/c string? (listof string?))))
                            (listof (listof string?)))
                        ]))
