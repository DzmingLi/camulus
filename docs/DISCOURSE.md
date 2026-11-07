# Discourse 后端

后端位于 `src/backends/discourse/`。原生接口实现 `Backend.S`、`Threading`、`Liking`、`Writing`、`Subscription`、`Topic_tracking`；Discourse topic 身份、楼层回复关系、User API 身份及 HTTP 操作由本后端管理。`interaction_emacs.ml` 使用 Ecaml 实现认证、浏览、分类与点赞命令，不依赖 Gnus，也不调用旧后端抓取内容。

## 远端追踪管理

在首页的 Discourse 组目录中，每个已注册站点下都有“远端追踪管理…”按钮。也可以在组目录、订阅页或阅读界面按 `T`，或执行 `M-x camulus-manage-subscriptions` 选择站点。

- 默认显示 `watching`（所有回复通知）的会话；`/` 切换 `tracking`（追踪未读数量）或 `muted`（免打扰）。这些是同一个远端会话追踪功能的不同级别，不是独立浏览组。
- `watching` 对每条新回复发通知；`tracking` 记录未读数量，对回复自己和提及发通知。`regular` 不追踪未读数量，仍可收到回复和提及。
- `n` 分页加载，`g` 重新读取远端；每行显示话题标题与追踪级别。
- `RET` 在右侧打开完整讨论；`s` 先查询最新远端级别，再选择新级别并立即提交。
- `regular` 表示回到普通级别；`muted` 表示静音，两者明确区分。
- 修改后重新读取服务器确认，随后刷新面板；网页端的修改在 `g` 后反映出来。失败显示错误并保留已有列表，不伪装成成功，也不自动重试写入。

此面板只保存当前页的内存状态，不创建或维护本地话题订阅清单，通知由服务器根据追踪设置生成，个人时间线读取这些通知。面板与阅读界面的 `s` 使用同一个后端追踪操作。

站点 `latest`、`category:ID` 和 `topic:ID` 仅用于浏览，不提供本地组订阅。已注册且加载了 User API 凭据的每个站点自动提供一个通知时间线来源；同站点注册多个组不会重复添加。没有认证的站点仍可匿名浏览，但不进入通知时间线。

首页通过 `/notifications.json` 分页读取远端通知，不再选择本地“回复我的／关注／追踪”入口。`replies`、`watching`、`tracking` 浏览组已移除，旧本地 Discourse 组订阅记录被忽略。时间线不按通知类型过滤：回复、提及、引用、点赞、追踪会话的新回复等由服务器决定；同一会话的通知合并展示。通知刷新保留稳定事件 ID，`d` 移除当前已知活动，新通知到来才重新出现。

读取通知和本地 `d` 不提交远端标为已读操作，也不改变追踪级别。非会话通知（如徽章）作为独立通知摘要显示，附站点通知页链接；不把它伪装成可回复或点赞的帖子。重启后重新打开这类未收藏通知会从远端通知历史查找，最多 100 页；不可用时明确报错，已收藏副本仍可离线阅读。

列表使用本站点 `/latest.json?state=...` 的远端过滤，不抓取全部话题后本地筛选。游标绑定站点和过滤级别；服务器给出的下一页 URL 只作为是否还有下一页的提示，不直接携带认证信息跟随它。

## 使用

```sh
nix develop -c make reader-demo
```

在 [入口配置](ENTRIES.md) 中声明论坛来源（例如 `discourse:https://discourse.nixos.org`）及组，首页 `G` 选择：

| 组 | 范围 |
| --- | --- |
| `notifications` | 后端自动提供的远端通知时间线，不需要手写注册或订阅 |
| `latest` | 这个论坛的最新讨论 |
| `category:3` | 分类 ID 为 3 的讨论，不含子分类 |
| `topic:123` | 单个讨论的后续进展 |

按首页 `g` 拉取。`d` 移除当前活动，不改变订阅、已读或论坛上的追踪设置；新的远端通知使该讨论重新出现。每个讨论合并为一个条目。

`M-x camulus-reader-discourse` 可以先浏览，不自动订阅。列表支持：

- `c` 选择分类，`/` 在当前组远端搜索，空查询恢复普通列表。
- `n` 取下一页，`g` 从第一页刷新。
- `RET` 在右侧打开默认展开的回复树；兄弟楼层按发布时间排列。
- `m` 收藏整个讨论；阅读窗口内 `m` 只收藏当前楼层。
- `s` 查询并修改所选 thread 的远端追踪设置，`h` 回首页。
- 实时阅读窗口内 `l` 点赞、`C-u l` 取消点赞。更新立即显示，同一楼层的重叠操作被阻止；迟到的旧读取不会覆盖待确认操作。明确失败恢复之前的显示，结果不确定则标记 unconfirmed，重新打开文章向服务器核实。

这里的刷新仍然由 `g` 手动触发，尚无后台轮询或 MessageBus 推送。

## 认证

公开内容支持匿名读取。认证沿用本机 `nndiscourse` / `discourse-auth` 的 User API 约定：

- auth-source `host` 为规范化后的论坛基础 URL，保留 `/forum` 等部署路径。
- `port` 为 `discourse-user-api`。
- `user` 为 User API Client ID，`secret` 为 User API Key。
- 如果已加载 `discourse-auth-headers`，优先读取它管理的现有授权会话。

在 `camulus-groups` 中注册站点浏览组，启动 reader 时自动加载这些站点的身份。

授权继续使用已有的 `M-x discourse-auth-login`，本次没有另建授权协议或替换凭据存储。新站点可以直接匿名浏览；在运行时已经启动之后新增或变更授权，需要先 `M-x camulus-probe-session-stop`，再启动 reader。`emacs -Q` 不会自动加载个人 auth-source 配置，私有论坛需要在该会话中提供原有认证配置。

API 凭据只发往指定站点及其部署路径。API 请求不跟随重定向，避免凭据泄漏；分页只使用后端生成且绑定当前组/查询的游标，不直接访问响应中给出的任意 URL。附件支持 CDN 重定向，跨域时不发送 User API 凭据。远端使用 HTTPS，仅测试允许 loopback HTTP。

## 身份、回复树与归档

来源标识为 `discourse:<规范化论坛URL>`。讨论 ID 为 `topic:123`，文章 ID 为 `topic:123/post:4`。文章 ID 中的数字是讨论内楼层号；点赞接口使用另一套全站 post ID，由后端解析维护。列表、搜索、讨论阅读及收藏中的同一楼层保持一致身份。

讨论先显示 `/t/ID.json` 已提供的帖子，再按 `post_stream.stream` 中尚未加载的 post ID，每批最多 20 个请求 `/t/ID/posts.json`。根据 `reply_to_post_number` 建树；未明确回复某楼的帖子连接到首楼。缺失父楼用占位条目表达，后续真实内容到达时替换。服务器缺页、跨 topic 返回、重复 ID 和无效回复关系都会报错；不会把半个讨论标记为完整归档。

收藏保存 cooked HTML 和附件二进制数据：识别 attachment 链接、lightbox 原图、上传图片、音视频 source。普通外链网页和任意远端装饰图片不进行递归抓取。附件出现在各楼正文下面，保存后的文件可以离线导出；离线阅读当前通过 SHR 呈现正文，图片文件归档不等于离线内嵌图片渲染。

收藏的新回复在首页 `g` 时补齐，旧副本在失败时保留。单篇收藏与整个讨论收藏分别显示，支持嵌套、多文件夹归属；最后一个归属被移除时释放相应本地内容。

单个 API 响应上限 16 MiB，单附件与整次归档上限 128 MiB，讨论最多 20000 个可见 post ID；超限报错。连接超时 10 秒，单 HTTP 请求总超时 30 秒。网络等待通过 Eio/libcurl multi 进行，可以取消，无外部 curl 进程。

## 写入与追踪接口

原生 `camulus-discourse-submit` 接受 alist，所有请求都有 `site` 字段。当前已实现：

| `op` | 额外字段 / 作用 |
| --- | --- |
| `categories` | 分类列表 |
| `list` | `group`、可选 `after` |
| `search` | `group`、`query`、可选 `after` |
| `thread` / `read` | `id`，完整讨论 / 单篇 |
| `like` | `id`、布尔 `liked`，绝对目标状态 |
| `reply` | 父楼 `id`、`text` |
| `submit` | 数字 `category`、`title`、`text` |
| `track` | `id`、`level`：regular / tracking / watching / muted |
| `topic-state` | `id`，读取服务器当前 notification level |
| `attachment` | 文章 `id`、数字 `part`、本地 `destination` |

回复、发帖与 tracking 已实现后端接口及模拟服务测试。回复/发帖编辑界面仍待接入；当前界面已接入点赞和通用订阅设置。**持久离线写入队列尚未实现**。HTTP 超时、5xx 或无法确认的写入回执会报告结果不确定，不自动重发；确认论坛实际状态后再决定后续操作。

`Subscription.get/set` 直接读写远端 notification level；`Topic_tracking.list` 用于会话追踪管理。个人时间线只读取服务器通知，不维护第二份本地追踪或通知选择状态。

## 验证

入口与订阅重构后的完整结果见 entries-test-results.txt（本地验证输出）。

新增 OCaml 测试覆盖身份、分批树、缺页失败、分类与搜索隔离、游标隔离、凭据头、点赞幂等、回复回执、写入不确定且不重试、tracking 读写，以及 CDN 下载不携带认证信息。

Emacs 测试覆盖组浏览、分页、搜索、完整展开的树、入口/移除/新回复、通知自动发现、移除后新通知恢复、远端追踪变化和并发刷新缓存、归档更新、重启后的离线正文与二进制附件、User API 写入、乐观点赞与旧结果协调、取消及其他任务继续运行。写入测试全部针对 loopback 模拟论坛，不修改真实论坛账户或发布真实帖子。

原有 35 项 Emacs 回归与最初 4 项 Discourse 测试通过；补充点赞竞争测试与父楼归档占位处理后，5 项 Discourse 测试及 OCaml 测试通过。记录见 discourse-test-results.txt（本地验证输出） 和 discourse-followup-test-results.txt（本地验证输出）。另对 `discourse.nixos.org` 匿名 GET 最新列表及一个讨论，核对了当前 API 的分页、stream 与 post 字段；未进行真实认证写入测试。

参考：[官方 API 文档](https://docs.discourse.org/)、[TopicsController](https://github.com/discourse/discourse/blob/main/app/controllers/topics_controller.rb)、[PostActionsController](https://github.com/discourse/discourse/blob/main/app/controllers/post_actions_controller.rb)。本地旧实现只用于核对认证、ID 和分页约定；归档与统一 feed 使用当前 OCaml 阅读器的实现。
