#lang racket/base

(require racket/contract
         racket/list
         racket/string)
(require db)
(require gregor)
(require json)
(require net/http-easy)

(require "convert.rkt")
(require "lib.rkt")

(define *telegram-api-url-base* "https://api.telegram.org/bot")
(define token (getenv "TELEGRAM_TOKEN"))
(define telegram-api-url (string-append *telegram-api-url-base* token "/"))
(define get-updates-url (string-append telegram-api-url "getUpdates"))
(define send-message-url (string-append telegram-api-url "sendMessage"))

(define subscription-db (sqlite3-connect #:database "./subscription.db"))

(define/contract (info datetime text)
  (-> datetime? string? void?)
  (displayln (format "[~a]~a" (datetime->iso8601 datetime) text)))

(define/contract (get-updates offset)
  (-> integer? response?)
  (post get-updates-url
        #:json (hash 'offset offset 'timeout 0 'allowed_updates '("message"))))

(define/contract (send-reply chat-id message-id text)
  (-> integer? integer? string? response?)
  (post send-message-url
        #:json (hash 'chat_id
                     chat-id
                     'reply_parameters
                     (hash 'message_id message-id)
                     'text
                     text)))

(define/contract (subscribe chat-id message-id user-id packages)
  (-> integer? integer? integer? (listof string?) response?)
  (for ([package packages])
    (query-exec subscription-db
                "insert into subscription values ($1, $2)"
                user-id
                package))
  (send-reply chat-id message-id "Subscribed."))

(define/contract (unsubscribe chat-id message-id user-id packages)
  (-> integer? integer? integer? (listof string?) response?)
  (for ([package packages])
    (query-exec subscription-db
                "delete from subscription where user_id = $1 and package = $2"
                user-id
                package))
  (send-reply chat-id message-id "Unsubscribed."))

(define/contract (get-packages user-id)
  (-> integer? (listof string?))
  (query-list subscription-db
              "select package from subscription where user_id = $1"
              user-id))

(define/contract (get-anicca chat-id message-id user-id)
  (-> integer? integer? integer? response?)
  (define anicca-res
    (sort (search (get-packages user-id) (convert (online-data)))
          string<?
          #:key car))
  (send-reply chat-id
              message-id
              (if (null? anicca-res)
                  "No package update found."
                  (string-join (for/list ([package anicca-res])
                                 (format "~a: ~a -> ~a ~a"
                                         (first package)
                                         (third package)
                                         (fourth package)
                                         (if (equal? "N/A" (fifth package))
                                             ""
                                             (format "(~a)" (fifth package)))))
                               "\n"))))

(define/contract (get-subscribed-packages chat-id message-id user-id)
  (-> integer? integer? integer? response?)
  (define packages (sort (get-packages user-id) string<?))
  (send-reply chat-id
              message-id
              (if (null? packages)
                  "No package subscribed."
                  (format "Subscribed ~a package~a: ~a"
                          (length packages)
                          (if (> (length packages) 1) "s" "")
                          (string-join packages ", ")))))

(define/contract (handle-command update)
  (-> jsexpr? any/c)
  (define message (hash-ref update 'message))
  (define args (string-split (hash-ref message 'text)))
  (define command (car args))
  (define chat-id (hash-ref (hash-ref message 'chat) 'id))
  (define message-id (hash-ref message 'message_id))
  (define user-id (hash-ref (hash-ref message 'from) 'id))
  (info (posix->datetime (hash-ref message 'date))
        (format "[CMD][CHAT ~a][USER ~a]: ~a"
                chat-id
                user-id
                (hash-ref message 'text)))
  (cond
    [(string-prefix? command "/ping") (send-reply chat-id message-id "pong")]
    [(string-prefix? command "/subscribe")
     (subscribe chat-id message-id user-id (cdr args))]
    [(string-prefix? command "/unsubscribe")
     (unsubscribe chat-id message-id user-id (cdr args))]
    [(string-prefix? command "/updates")
     (get-anicca chat-id message-id user-id)]
    [(string-prefix? command "/list")
     (get-subscribed-packages chat-id message-id user-id)]))

(define (handle-update update)
  (when (hash-has-key? update 'message)
    (define message (hash-ref update 'message))
    (define text
      (if (hash-has-key? message 'text)
          (hash-ref message 'text)
          ""))
    (if (string-prefix? text "/")
        (handle-command update)
        (info (posix->datetime (hash-ref message 'date))
              (format "[TXT]: ~a" text)))))

(define (mainloop [offset 0])
  (sleep 1)
  (define updates-res (get-updates (add1 offset)))
  (cond
    [(not (= 200 (response-status-code updates-res)))
     (displayln (string-append "Cannot fetch updates, status code: "
                               (response-status-code updates-res)))
     (mainloop)]
    [else
     (define updates (hash-ref (response-json updates-res) 'result))
     (cond
       [(null? updates) (mainloop)]
       [else
        (for-each handle-update updates)
        (mainloop (hash-ref (last updates) 'update_id))])]))

(info (now) " Started")
(mainloop 0)
