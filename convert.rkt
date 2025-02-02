#lang racket/base

(require racket/contract
         racket/match
         racket/string)

(define/contract (convert pkgsupdate)
  (-> (listof (hash/c symbol? (or/c string? (listof string?))))
      (listof (listof string?)))
  (for/list ([pkg (in-list pkgsupdate)])
    (match-define (hash 'name name 'path path 'before before 'after after 'warnings warnings) pkg)
    (list name
          (if (string? path)
              (car (string-split path "/"))
              "N/A")
          before
          after
          (if (list? warnings)
              (cond
                [(null? warnings) "N/A"]
                [(> (length warnings) 1) (cadr warnings)]
                [else "N/A"])
              "N/A"))))

(provide (contract-out [convert
                        (-> (listof (hash/c symbol? (or/c string? (listof string?))))
                            (listof (listof string?)))
                        ]))
