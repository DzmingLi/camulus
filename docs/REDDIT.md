# 第一个真实后端：Reddit

后端完全用 OCaml 实现，运行在已有 Eio worker domain 内。沿用先前 nnreddit 后端的公开 installed-app Client ID `5oagOpX2_NKVDej_iuZjFA` 和回调地址 `http://127.0.0.1:17973`。已有 nnreddit refresh-token 文件可以直接读取，token 不写进源码，也不传到前端结果中。

参考版本：[DzmingLi/nnreddit](https://github.com/DzmingLi/nnreddit/tree/d2f3ffc9e714560e2c6aa740cb4a72ab58f808b1)。沿用应用及协议约定；没有沿用 Gnus article-number、缓存或组状态结构。

## 现在可以做什么

| 功能 | 实现 |
| --- | --- |
| subreddit | 最新帖子摘要，保留分页游标；不提前取评论 |
| 单个帖子 | `t3_…` 原生 thread 身份，独立读取 |
| 回复树 | 首批立即返回，后续逐批 `morechildren`，处理继续展开分支 |
| 顺序 | 同级按发布时间、ID 排序，不采用热度排序 |
| 搜索 | 指定 subreddit 内的远端搜索及分页 |
| 通知 | 返回 reply/mention 活动及原生 thread、comment ID；不自动获取正文或标已读 |
| 身份 | 当前 OAuth 账户查询 |
| 写入 | 回复帖子/评论、发表文本帖子、上票/撤票/下票 |
| 授权 | 刷新 token、完整授权码流程、随机 state 校验、0600 原子保存 |

帖子和评论在列表、搜索、通知及回复树中保留相同原生 ID。通知按活动返回，后续个人 feed 可按 `thread` 合并；不会用通知读取隐式修改已读状态。

## 直接试用

在本目录运行：

```sh
nix develop -c make reader-demo
```

启动全新 `emacs -Q` 后输入 subreddit 和 Reddit 用户名。默认读取 `~/News/nnreddit/refresh-token`。身份和凭据路径由 Camulus 配置声明，不读取旧后端变量。

也可以在 Emacs 中设置下列变量，或使用入口 JSON 的 `backend_settings.reddit`：

```elisp
(setq ef-reddit-username "YOUR_USERNAME"
      ef-reddit-token-file "/path/to/private/refresh-token")
(ef-reddit-browse "ocaml")
```

列表中 `RET` 在右侧打开整个回复树，默认展开；`s` 在当前 subreddit 搜索，空查询回到普通列表；`n` 加载下一页，`g` 刷新。关闭阅读 buffer 会取消其请求；同一阅读 buffer 切换帖子时，旧请求结果不会覆盖新帖子。

浏览、回复树、投票与收藏都使用同一个原生界面，见 [README](../README.md#阅读与内容管理)。

这是方便联调的薄 Elisp 界面，调用的后端、认证、解析、传输和调度都在 OCaml 中。它还不是最终阅读器：暂未实现已读状态、折叠、收藏、附件归档、draft group、写入编辑界面或 WebKit。Olivetti 已加载时，阅读 buffer 会启用它。现在只展示文本及原始链接，保留的 HTML 尚未接入正式渲染器。

需要新授权时，运行 `M-x ef-reddit-authorize`。浏览器的 localhost 页面可以连接失败，复制完整地址回 Emacs 即可，与 nnreddit 相同。写入路径不能是 agenix 等提供的只读 symlink；已有 symlink token 可以继续用于读取。授权回调不会记录进 minibuffer history。

nnreddit 原来的授权 scope 没有 `vote`。旧 grant 可继续用于已授权能力；投票若缺权限，会返回 `Missing_scope "vote"`，不能靠刷新 token 自动增加权限。新授权请求包含 `identity read privatemessages submit vote`。

## OCaml 模块边界

```text
Reader.Backend.S            通用阅读契约，group/cursor 由后端定义
Reader.Backend.Threading    可选讨论树加载契约，不要求所有后端提供
Reader.Backend.Writing      可选写入契约，draft/receipt 均为抽象类型
Reader.Backend.Voting       可选投票契约
Reader.Backend.Liking       独立的单向点赞契约，Reddit 不实现
          ▲
Reddit_backend.Make(Http)           身份、授权、分页、解析、操作语义
          │
Reddit_backend.Curl_transport       libcurl multi，Eio 等待/取消
          │
src/application.ml            扩展 Protocol.work / Protocol.payload
          │
Reader.Runtime                 与具体后端无关的调度和生命周期
```

`Reddit_backend.Make` 的 session 类型 `t` 通过 `.mli` 隐藏；access token、刷新状态和 OAuth challenge 属于后端。注入 `Http.S` 可以完整测试协议行为。核心 runtime 只接收扩展处理器；之后增加 Discourse 等后端，不需要给核心调度器添加一个新的硬编码分支。

线程/消息关系使用普通不可变 OCaml 记录。`Content.message.thread_id` 是 option，未把 Reddit 的原生稳定 thread ID 假设强加给邮件后端。JSON 只用于 Emacs 和 CLI 边界，跨 domain 通信仍然是类型化 OCaml 值。

`Backend.S` 只要求来源标识、组内列表和搜索；讨论树加载由独立的 `Backend.Threading` 提供。Reddit 和 Maildir 均实现该能力，未来的 RSS 后端可以只实现基础接口。投票参数、操作结果和 `Content.message.my_vote` 使用 `Content.vote = Upvote | Downvote | No_vote`。Reddit 请求和现有 Emacs/CLI JSON 边界保留 `1/-1/0` 编码，非法输入在边界拒绝。

## 交互式投票

summary 列表和回复树都支持 `+` 赞成、`-` 反对、`0` 取消投票。这些命令设置绝对状态，不会因为重复按同一键而反转。列表显示远端 score 和自己的 vote；score 不做本地推算。

投票立即显示目标状态及 `pending`，同一内容的所有已打开视图共享状态；投票不会替换正在加载的 thread 请求，关闭阅读视图也不会取消它。一个内容在投票未结束时拒绝新的投票操作。明确拒绝会回到此前状态并显示 `failed`；结果不确定时保留目标状态并显示 `unknown`，不会宣称服务器已回滚。用 `g` 刷新列表或讨论，重新获取远端状态。此前已发出的读取结果不能覆盖本次操作结果。

状态协调在 `src/voting.ml` 中实现，由 Ecaml 主线程管理；演示 Lisp 只负责命令入口与渲染。当前是会话内的乐观更新，尚无持久离线队列、自动重试或跨重启恢复；退出不会将未完成投票转为可靠的待同步任务。点赞的 `Backend.Liking.set_like` 接口独立于投票，未来由有单向点赞语义的后端实现。

此切片通过 OCaml 测试及完整的 29 个 Emacs 集成测试（运行时 9、Reddit 7、邮件阅读/发送 8、草稿/归档 5），记录见 voting-test-results.txt（本地验证输出）。投票测试使用注入传输和缺失凭据场景，未向真实账户投票。状态测试覆盖明确拒绝、结果不确定、重复结果、在途读取和跨视图协调。

## 调度与失败处理

- 同一后端 session 串行进行 API 请求，合并 access-token 刷新；锁可取消，失败后不会被永久锁住。
- 默认请求间隔 1 秒，处理 rate-limit headers、429 和后续请求的冷却时间。
- GET 收到 401 时刷新一次；传输失败、502/503/504 可以重试一次。没有无限重试。
- POST 不自动重试。传输中断、取消、5xx、成功响应缺少确认等情况返回 `Uncertain_write`，供后续同步层核对结果，避免重复发帖。
- `morechildren` 每批最多 100 个 ID，每次 thread 加载最多 200 次页面请求。未解决的分支保留 gap，不能假装完整加载。
- libcurl 验证 TLS 证书和主机名，禁用重定向及 signal handling，要求 asynchronous DNS；使用 multi 接口，每 10ms 通过 Eio 挂起后推进。此轮尚未改为 socket-readiness 驱动。
- HTTP 响应体限制 16 MiB，header 限制 64 KiB；读取过程取消或失败都会清理 easy/multi handles。

当前写入接口是真实 API 实现，但没有持久 outbox，也没有完整的乐观 UI 状态管理。断网自动同步仍需下一阶段的持久操作模型；不能把当前内存中的 service task 当成已实现的离线队列。

## 原生调用入口

`ef-reddit-configure` 在 runtime 启动前接收用户名、token 文件路径、Client ID 和 redirect URI。`camulus-reddit-submit` 接收操作 alist 并立即返回请求 ID；原生 UI 通过事件回调接收结果。

```elisp
(ef-reddit-request "{\"op\":\"subreddit\",\"name\":\"ocaml\"}")
(ef-reddit-request "{\"op\":\"thread\",\"id\":\"t3_POST_ID\"}")
(ef-reddit-request "{\"op\":\"search\",\"name\":\"ocaml\",\"query\":\"effects\"}")
(ef-reddit-request "{\"op\":\"notifications\"}")
```

其他操作：`identity`、`post`、`vote`、`reply`、`submit`、`authorize`、`authorize-complete`。精确字段见 `operation_of_json`。写入和授权任务归属 `Service`，不会因切换阅读视图被取消。原生结果格式为 `(reddit REQUEST-ID VIEW-OR-NIL JSON)`，随后收到既有 `finished` 事件；错误也保留结构化的 `uncertain_write`、`retry_after`。

提供只读 CLI，凭证通过文件路径指定：

```sh
export REDDIT_USERNAME=YOUR_USERNAME
export REDDIT_TOKEN_FILE=/path/to/private/refresh-token
nix-shell --run 'dune exec bin/reddit_cli.exe -- identity'
```

上述命令使用本地 `_build`；也可用 `--build-dir` 指定现有构建目录的绝对路径。CLI 还支持 `subreddit NAME`、`thread t3_ID`、`search NAME QUERY`、`notifications`。`REDDIT_SUMMARY=1` 只输出数量/状态，不输出正文。

## 本轮验证

在本机 Emacs 31.1、OCaml 5.3.0、Ecaml 0.17.0、Eio 1.6、ocurl 0.9.2 上测试。

- `make test`：10 组 Reddit 协议测试、4 项新增原生模块/界面 ERT，加上原有核心、9 项 Emacs 集成测试和退出测试。
- `make reddit-live`：显式指定环境变量后进行只读联调，默认不包含在 `make test` 中。
- 真实既有 token：身份验证成功；CLI 读取 25 个帖子和分页游标，读取 3 条内容的完整 thread。
- 真实原生模块：读取 25 个帖子、3 条内容的回复树、25 条组内搜索结果；期间 Emacs 心跳运行 515 次，并完成并发 GC 和 SIGPIPE/SIGCHLD 不变检查。
- 没有进行真实发帖、回复、投票、token 文件替换或远端已读变更。写入功能通过注入传输的协议测试验证，尚未实际发布测试内容。

原始测试输出见 `reddit-test-results.txt` 和 `reddit-live-results.txt`。图形显示的视觉效果、长期运行及实际编辑/发布流程还未验证。

协议参考：[Reddit API](https://www.reddit.com/dev/api/)、[官方 OAuth2 说明](https://github.com/reddit-archive/reddit/wiki/OAuth2)。本实现保留 nnreddit 的 GPL-3.0-or-later 来源说明；应用 ID 是公开标识，不是 token。
