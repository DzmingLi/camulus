;;; config.el --- Example Camulus configuration -*- lexical-binding: t; -*-

(require 'camulus)

;; Replace account names, paths and URLs with your own.
;; Browsing groups do not automatically subscribe to their content.
(setopt camulus-groups
        '(((name . "NixOS forum")
           (source . "discourse:https://discourse.nixos.org") (group . "latest"))
          ((name . "OCaml subreddit")
           (source . "reddit:YOUR_USERNAME") (group . "ocaml"))
          ((name . "Reddit replies and mentions")
           (source . "reddit:YOUR_USERNAME") (group . "notifications"))
          ((name . "Example blog")
           (source . "rss:https://example.org/feed.xml") (group . "https://example.org/feed.xml"))
          ((name . "Mail inbox")
           (source . "maildir:/absolute/path/to/Maildir") (group . "/Inbox"))
          ((name . "Jane Street")
           (source . "janestreet") (group . "posts"))
          ((name . "Atlantic · Ian Bogost")
           (source . "atlantic") (group . "author.ian-bogost"))
          ((name . "Zhihu articles")
           (source . "zhihu") (group . "articles.people.USER"))
          ((name . "Douban timeline")
           (source . "douban") (group . "timeline.12345"))
          ((name . "NetEase events")
           (source . "netease") (group . "events.12345"))
          ((name . "Public-inbox thread")
           (source . "public-inbox") (group . "https://list.example.org/list/message-id@example.org/"))))

;; Add the sources you want in Home, for example:
;; (setopt camulus-janestreet-subscriptions '("posts")
;;         camulus-rss-subscriptions '("https://example.org/feed.xml"))

;; Configure credentials and backend options with M-x customize-group RET camulus.
