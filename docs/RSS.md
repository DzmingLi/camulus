# RSS / Atom 后端

RSS 2.0、RSS 1.0/RDF 和 Atom 1.0 共用 `src/backends/rss/` 中的 OCaml 后端。使用 Syndic 1.8.0 解析标准字段和日期，格式自动识别，无需账户配置；实现 `Backend.S` 的列表，以及单篇读取和附件下载，不实现 `Backend.Threading`。

## 使用

```sh
nix develop -c make reader-demo
```

通过 `setopt camulus-rss-subscriptions` 声明主页订阅的 URL 列表，`camulus-groups` 单独配置浏览入口。也可以 `M-x camulus-reader-rss` 输入地址浏览，按 `s` 打开后端的 Customize 订阅选项。没有独立订阅文件。首页 `g` 刷新来源。

- 每篇文章独立一行；`RET` 在右侧打开正文，支持文本及 SHR HTML 阅读。
- `m` 选择收藏文件夹，保存文章和所有声明的 enclosure 附件；支持多个文件夹及嵌套目录。
- 附件按钮位于文章下面，可以下载；收藏中的附件直接从本地数据库保存，不访问网络。
- `d` 只从首页移除当前修订。重复拉取同一内容不会复活条目；新文章或已有文章的新修订会进入 feed。
- 从 `camulus-rss-subscriptions` 移除 URL 即取消订阅；浏览入口和既有收藏保留。
- `f` 打开收藏，读取持久副本。

也可以 `M-x camulus-reader-rss` 输入地址，先浏览而不订阅。`g` 刷新、`RET` 阅读、`m` 收藏、`h` 回首页。

## 内容与身份

来源标识为 `rss:<订阅时输入的URL>`，HTTP 重定向不会更换来源身份。RSS `guid`、Atom `id`、RSS 1.0 `rdf:about` 优先作为文章标识，其次使用文章链接。不同订阅 URL 的内容隔离；不猜测不同 feed 中同名 ID 是否表示同一篇文章。

缺少 ID 和链接时，使用标题和正文摘要生成稳定的备用 ID。这能去除重复拉取，但无法证明修改后的匿名条目仍是原文章。RSS 缺少可选日期时使用固定的 epoch 值，避免每次刷新获得新的时间。Syndic 负责 RFC 3339 和 RFC 822 日期与时区；文档格式或必需字段不合法时报告解析错误并保留旧缓存。

RSS 优先使用 `content:encoded`，其次 `description`。Atom 支持 text、html、xhtml 正文和 summary 回退；XHTML 保留结构，并解析 `xml:base` 下的相对链接。支持文章链接、RSS enclosure 和 Atom enclosure。外置 Atom `content src` 不另行抓取，使用 summary；摘要型源不会自动抓取网页全文。

正文中普通远端图片不自动下载，附件归档限于源明确列出的 enclosure。HTML 通过 Emacs SHR 呈现，不执行网页脚本；邮件 WebKit 方案不用于这个后端。

## 网络与存储

网络请求复用公共 `Reader.Http`，由工作 domain 中的 libcurl multi 驱动，通过 Eio 挂起等待，可取消，不调用外部 curl 程序。支持 HTTP/HTTPS、证书验证、压缩响应和最多五次重定向；重定向目标同样只允许 HTTP(S)，不接受带用户名密码的 URL。

会话内保存 ETag / Last-Modified 和已解析结果。304 复用已有内容；无缓存时收到 304、解析错误或 HTTP 错误不会覆盖上次成功结果。重启后重新拉取订阅源，SQLite 中的文章 ID、修订和处理状态仍用于去重。

普通文章的正文只存在于当前会话缓存，持久保存的只是列表元数据和操作状态。收藏先下载正文与所有 enclosure，成功后一次事务提交；任何附件失败都不会留下声称完整的收藏。schema v1/v2 自动升级到 v3，保留既有邮件 MIME、附件、收藏和阅读状态；迁移详情见 [ENTRIES.md](ENTRIES.md)。从最后一个文件夹移除时，相应正文和附件一起清理。

`g` 同时更新已收藏文章，即使它们不再订阅。文章已从源中移除或服务器暂时不可用时保留旧归档，并报告刷新失败；不会删除已有副本。未收藏且已从源中消失的文章，重启后可能无法再读取正文。

XML 直接交给 Syndic / Xmlm 解析，不额外预扫描或限制大小、深度与节点数量。HTTP 响应、附件下载及归档不设大小配额。连接超时 10 秒、单请求总超时 30 秒。暂不支持 OPML 导入、网页自动发现订阅源、历史分页、认证订阅源、WebSub 或自动轮询；仍由 `g` 手动同步。

## 验证

`make test` 包含 RSS/Atom 的 OCaml 测试和三个新的原生 Emacs 集成测试：

- RSS/RDF/Atom、命名空间、CDATA、XHTML、相对链接、日期与时区、稳定身份和修订。
- 重定向、条件请求、304 缓存、失败保留缓存、非法协议与无效 feed 拒绝。
- 首页订阅、处理后去重、新修订重现、文件夹选择、正文和二进制附件归档、重启后的离线阅读。
- Atom 无订阅浏览及下载；慢 HTTP 请求期间其他任务继续运行，取消正常结束。
- 数据库 v1 → v2 保留既有 MIME，最后一个收藏归属移除时清理附件。

测试使用合成内容与本地 HTTP 服务，无需真实订阅或远端账户。首次全量回归中，原有 32 项 Emacs 测试通过，新测试发现终端 SHR 将 HTML 正文换行过窄。修正后，3 项 RSS/Atom 测试与 3 项共享持久阅读界面测试全部通过；OCaml 测试也通过，包括 schema 迁移。原始回归与修复后的针对性验证分别记录在 rss-test-results.txt（本地验证输出） 和 rss-followup-test-results.txt（本地验证输出）。

格式依据：[RSS 2.0 规范](https://www.rssboard.org/rss-specification)、[Atom RFC 4287](https://www.rfc-editor.org/rfc/rfc4287)。
