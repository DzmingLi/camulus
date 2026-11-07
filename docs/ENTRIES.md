# 浏览入口与订阅配置

浏览入口与订阅分开配置。`camulus-groups` 为组声明名称和来源，供首页后端按钮与组目录浏览；它不会自动把组加入主页。

```elisp
(setopt camulus-groups
        '(((name . "OCaml") (source . "reddit:alice") (group . "ocaml"))
          ((name . "Blog") (source . "rss:https://example.org/feed.xml")
           (group . "https://example.org/feed.xml"))
          ((name . "Forum") (source . "discourse:https://forum.example.org")
           (group . "latest"))))
```

本地订阅由各后端的 `defcustom` 选项声明。订阅列表只保存组标识；后端负责把它转换成来源和浏览请求。修改选项后刷新主页即可，不需要重启后台 worker，也不再读取或写入 JSON 订阅文件。

```elisp
(setopt camulus-rss-subscriptions '("https://example.org/feed.xml")
        camulus-jsonfeed-subscriptions '("https://example.org/feed.json")
        camulus-reddit-subscriptions '("ocaml" "emacs")
        camulus-mu-subscriptions '("/work/Inbox")
        camulus-theatlantic-subscriptions '("author.ian-bogost")
        camulus-janestreet-subscriptions '("posts")
        camulus-blogger-subscriptions '("posts.example.blogspot.com")
        camulus-zhihu-subscriptions '("answers.example-user")
        camulus-douban-subscriptions '("timeline.12345")
        camulus-netease-subscriptions '("events.12345")
        camulus-public-inbox-subscriptions
        '("https://list.example.org/list/message-id/"))
```

列表默认为 `nil`。已注册但没有列入订阅的组仍可浏览；订阅来源也可以不单独注册，届时使用后端提供的默认名称。Mu 的来源由 `camulus-mail-root` 决定，Reddit 的来源由当前后端身份决定，其他后端处理自己的 URL 和组标识。

首页或组目录的 `s` 对本地订阅打开当前后端的 Customize 选项。通过 Customize 保存，或在自己的 Emacs / Nix 配置中维护 `setopt`；阅读器不会另外写一份订阅状态。`U` 列出各后端当前提供的订阅来源，`g` 重新读取选项。

Discourse 使用服务器的会话通知和追踪机制。已注册且已认证的站点提供通知时间线，`T` 打开远端会话追踪面板；阅读讨论时的 `s` 修改远端通知级别。全站、分类等浏览组没有本地订阅开关。取消一个话题的追踪通过服务器接口完成。

优先级由 `camulus-subscription-priorities`、`camulus-default-subscription-priority` 和 `camulus-home-priorities` 配置。它只决定主页展示哪些订阅来源，不修改订阅列表或远端追踪。

阅读与写入状态仍独立持久保存：已读、移除、收藏文件夹、归档正文和附件属于内容状态；Mu 草稿属于邮件后端。更改订阅或移除浏览入口不会删除这些内容。

普通来源的主页 `d` 移除当前已显示的进展，新更新仍可出现；组内 `d` 只影响该入口。邮件的 `d` 由 Mu 移动到相应账户的 Trash；收藏夹中的 `d` 取消当前收藏归属。

后端扩展通过 `Reader_backend.register_local_groups` 显式提供选项名、组到来源的转换及可接受组规则。这个 helper 只在 Emacs owner thread 读取 Customize 选项、返回即时发现结果；不引入本地订阅 worker、JSON 存储或公共订阅数据库。远端会话追踪使用独立的可选能力。
