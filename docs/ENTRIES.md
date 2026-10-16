# 入口配置与后端订阅

来源、组入口与关注关系分别管理。组和个人 feed 使用同一个后端列表接口；首页汇集明确选入的个人 feed，而不扫描所有可浏览的组。配置一个入口不会调用远端订阅操作。

## 配置与交互

配置文件为 `$XDG_CONFIG_HOME/camulus/entries.json`，默认 `~/.config/camulus/entries.json`。`ef-reader-config-file` 可以覆盖路径。文件是 JSON 数据，不执行 Lisp 或 OCaml 代码；后端仍然是 OCaml 模块。

完整示例见 [entries.json](../examples/entries.json)，请替换示例用户名、URL 和本地路径。最小例子：

```json
{
  "version": 1,
  "home": null,
  "entries": [
    {"name": "我的讨论", "source": "discourse:https://discourse.nixos.org", "group": "watching", "timeline": true},
    {"name": "浏览论坛", "source": "discourse:https://discourse.nixos.org", "group": "latest"}
  ]
}
```

`M-x camulus` 固定打开统一首页。非 RSS 入口设置 `"timeline": true` 后加入首页；省略或设为 false 时仅作为浏览入口。RSS 来源从后端的 `rss-subscriptions.json` 自动发现，声明一个 RSS 浏览入口不会使其加入首页。`home` 字段仅兼容旧配置，不再改变首页目标，可设为 null。

首页先显示选入 feed 的本地缓存，并异步刷新；最多同时刷新 4 个入口，每个来源返回后逐步更新列表。按活动时间倒序排列，同一来源内的讨论跨入口合并，邮件继续按回复与 References 关系合并。每个来源维护自己的分页游标，`n` 加载尚有下一页的来源。失败时保留该来源缓存，页首显示失败数，`e` 查看详情。`b` 浏览所选条目的来源入口，`h` 返回统一首页。

- 首页顶部可点击 `Groups`、`Discourse`、`Reddit`、`RSS / Atom` 浏览入口。`B` 打开组目录，按来源列出所有配置组，也可以浏览未配置的 subreddit、论坛和 RSS URL；这些操作不会订阅或加入首页。论坛内 `c` 选择分类，`/` 搜索。
- `C` 编辑配置，保存后 `B` 重新打开组目录，或 `G` 选择入口。
- `G` 同时显示 RSS 后端已关注的 feed；取消关注移除这个发现入口，但不会移除配置文件里显式声明的入口。
- 首页不提供全局搜索；在 `G` / `b` 打开的入口中，`/` 搜索当前入口，空查询恢复列表。搜索结果不写入入口活动缓存，不改变订阅关系。
- `s` 查询后端当前关注设置，再选择新设置。没有该能力的后端明确报错。
- 首页 `d` 只移除统一时间线中已显示的进展，不影响组内浏览；组内 `d` 仍只影响该入口。两者都保留已读、收藏和远端追踪设置；新的进展可以再次出现。搜索结果不执行此操作。
- `m` 收藏；`g` 另外更新已收藏内容及附件，归档不依赖关注关系。

认证沿用各后端现有配置。Discourse 入口中的站点在启动时加入认证加载列表；运行中新增身份，需要停止并重新启动运行时。Reddit 用户名、Maildir 根目录等必须与已有后端配置的来源身份相符。配置文件不放凭据。

## OCaml 能力接口

`Backend.Subscription` 是可选模块：

```ocaml
module type Subscription = sig
  type t
  type target
  type settings
  val get : t -> target -> settings
  val set : t -> target -> settings -> unit
end
```

`target` 与 `settings` 均由后端定义。`set` 指定绝对设置，`get` 每次读取后端当前状态，接受站外修改。它不要求创建一个组，也不依赖内容数据库。接口不强制所有后端枚举全部关注对象；入口发现可以使用后端自己的能力。

- Discourse 的 `target` 是 topic/文章标识，`settings` 是 `Regular | Tracking | Watching | Muted`。调用现有远端 topic 通知级别接口。`watching`、`tracking` 是个人入口，使用服务器筛选的普通 topic 列表，并支持限定在此范围内搜索；需要有效身份。此实现依照 [Discourse TopicQuery 的 state/search 筛选](https://github.com/discourse/discourse/blob/main/lib/topic_query.rb)，没有在本地收集所有讨论再过滤。
- RSS 的 `target` 是 feed URL，`settings` 是布尔值。关注列表保存在 RSS 后端自己的 `rss-subscriptions.json` 中，格式为 URL 数组；路径由 `ef-reader-rss-subscriptions-file` 指定。读写在系统线程执行，使用锁文件和临时文件原子替换，无需 SQLite，也不会因关注而立即抓取正文。`ef-rss-followed` 返回该后端发现的关注入口。
- Reddit 已有的 `notifications` 入口设置 `timeline: true` 后加入首页，展示回复及提及并按讨论合并，读取列表不加载整个讨论，也不标记远端已读。本轮未为 Reddit 伪造不存在的通用 thread 订阅实现。
- 邮箱入口继续使用 `/Inbox` 等后端组标识，本轮没有给 Maildir 添加订阅能力。

Emacs 边界的 `camulus-subscription-submit` 使用 `source`、`target` 与可选 `setting`。省略设置时查询；设置后再次查询后端状态，返回 `subscription` 结果及后端提供的选项。此接口使用 alist；后端内部保持有类型的 OCaml 接口。

## 本地状态及迁移

SQLite 继续负责内容身份、已读、列表缓存、移除边界和收藏归档，已不负责有效订阅关系。缓存按来源及组隔离；统一首页有独立的移除边界，同一事件后来从另一入口发现不会复活已处理条目。内容身份和已读仍跨入口一致。刷新第一页成功后替换该入口的可见成员，后续分页补充；失败保留旧缓存，移除历史不会因缓存替换丢失。远端取消关注后，相关讨论会在下一次个人入口刷新时消失。

schema v1/v2 自动迁移到 v3，保留 MIME、附件、收藏和已读。旧订阅表、旧活动和旧移除记录留在 `legacy_*` 表供迁移；它们不再决定首页内容。旧活动与移除边界映射至该来源原来配置的各组，以保留重复拉取去重；由于旧结构没有保存活动所属组，升级时不会据此构造可见成员。

## 验证与边界

测试覆盖：入口声明无订阅副作用、配置失败保留旧配置、RSS 订阅在禁用内容数据库时读写及重启、接受文件外部修改、迟到列表结果隔离、同内容的跨入口移除隔离、迁移后保留移除边界、远端取消关注后的列表变化，以及原有邮件/归档/阅读回归。写入只针对本地临时文件和 loopback 模拟服务。另验证同一入口重叠刷新时，旧响应不会覆盖新缓存或让已移除的进展重新出现。

`make test` 覆盖 OCaml 测试及 45 项 Emacs 测试，记录见 entries-test-results.txt（本地验证输出）；重叠刷新专项记录见 entries-concurrency-test-results.txt（本地验证输出）。

首页打开会刷新一次，也可用 `g` 手动刷新；还没有周期性后台刷新。持久离线写入队列、自动后台同步仍未实现；本轮的订阅接口不会假装已具备离线重试。Discourse 的回复/发帖能力已在后端提供，编辑界面及自动保存草稿尚未接入。

## 来源与组标识


| source | group | 内容与能力 |
| --- | --- | --- |
| `janestreet` | `posts` | 官方 Atom 索引，打开时抓取完整文章，提取正文及图片 |
| `atlantic` | `author.ian-bogost` 等 | 官方作者 Atom，保留头图、正文和图片，去除注入的推荐链接 |
| `zhihu` | `articles.people.USER`、`articles.org.USER`、`answers.USER` | 作者文章与回答，原生 ZSE 签名、分页、正文和图片 |
| `douban` | `timeline.USER`、`replies.ACCOUNT`、`topic.ID`、`group-topic.ID` | 用户时间线、回复通知与讨论；广播/话题评论按回复关系渐进展开，含嵌套回复分页 |
| `netease` | `events.USER-ID` | 公开云村动态、图片、歌曲信息及可点击播放链接；不下载歌曲音源 |
| `public-inbox` | `https://HOST/LIST/MESSAGE-ID/` | 原生 gzip/mboxrd/MIME 解析，用 References / In-Reply-To 重建邮件树，保留 MIME 附件 |

这些后端分别位于 `src/backends/` 各自目录，不调用旧 Gnus 后端。Public Inbox 复用邮件后端的纯 MIME 解析器，不调用 mu、mbsync 或解压命令。它使用消息 URL 作为来源内标识，邮件关系参与公共层的合并，不人为指定永久 thread ID。读取 thread mbox 时先下载有大小限制的完整响应，再逐封交付界面。

新增后端的正文、图片或 MIME 附件都可收藏并离线读取。Jane Street / Atlantic 的搜索限定于该 feed 当前文章；Public Inbox 搜索限定于当前完整讨论。知乎、豆瓣、网易云目前不提供组内搜索接口，`/` 明确报告不支持，不退化为全站搜索。这六个后端本轮提供阅读与归档；站内点赞、回复、发帖与关注修改尚未接入。没有远端关注修改能力的入口通过配置文件管理。

### 非秘密运行设置

`backend_settings` 可以声明以下字段，完整校验后才应用；运行中改身份或路径需重启运行时：

```json
{
  "browser": {"firefox_profile": "/absolute/path/to/firefox/profile"},
  "discourse": {"auth_sources": ["/absolute/path/to/deployed-authinfo"]},
  "reddit": {"username": "YOUR_USERNAME", "token_file": "/absolute/path/to/refresh-token"},
  "maildir": {
    "root": "/absolute/path/to/Maildir",
    "muhome": "/absolute/path/to/mu",
    "sender": "you@example.org",
    "account": "main",
    "accounts": [["/main", "you@example.org", "main"]],
    "smtp_profiles": [["main", "smtp.example.org", 465, "you@example.org", "/path/to/password-file", ""]]
  }
}
```

知乎和豆瓣由 OCaml 读取指定 Firefox profile 的 Cookie，仅选择默认容器、匹配域名/路径且有效的记录。浏览器锁定数据库时，使用权限受限的临时数据库/WAL 快照，读取后删除；配置文件不保存 Cookie。请求签名、分页和正文解析也在 OCaml 中。图片下载不携带登录 Cookie。服务端登录挑战或访问限制会报告失败并保留旧列表，不伪装成空 feed。

Discourse 增加 `replies`（直接回复）和 `notifications`（回复、提及、引用）入口，读取服务器通知并按原生 topic 合并；GET 不提交标为已读操作。这些通知入口暂不提供组内搜索。

### 验证

`make test` 使用隔离的 XDG 配置/状态目录，包含原有回归、原生新后端测试、Firefox WAL 快照测试和 Emacs 界面测试。真实网络检查是单独显式运行的 `test/publication_live.ml` 和 `test/publication-native-live.el`，不会随普通测试运行；不会发帖、点赞或修改远端订阅。相关后端的源项目署名位于各目录 `NOTICE`。

启动新进程以加载新版本原生模块：在项目目录执行 `nix develop -c make reader-demo`，首页 `G` 选择入口，`g` 抓取当前入口，`RET` 打开，`m` 收藏。已加载旧 `.so` 的 Emacs 进程需要重启才能使用新后端。

## CS.RIN.RU

`source: "csrin"`，`group: "forum.ID"` 或 `"topic.ID"`。默认浏览入口不进入首页；需要时设置 `timeline: true`。支持论坛发现、逐页完整讨论、登录、发帖/回复与草稿，详细配置和边界见 [CSRIN.md](CSRIN.md)。
