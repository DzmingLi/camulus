# OCaml 邮件后端：本地 Maildir / mu 切片

这一版是本地 Maildir 邮件后端，保留 mu 索引和现有 mbsync 同步。`Maildir_backend` 使用 Mrmime 解析及生成 MIME，通过 Colombe + tls-eio 直接发送 SMTP；不再绑定 GMime 或调用 msmtp。`process_emacs.ml` 通过 Ecaml 提供 mu 子进程宿主；`compose_emacs.ml` 负责撰写和草稿交互。没有以 Gnus 或 mu4e 作为后端，也没有 Elisp 后端扩展接口。

## 运行

在项目目录执行：

```sh
nix develop -c make reader-demo
```

它启动新的 `emacs -Q` 并打开统一首页；在入口配置中声明 Maildir，再用 `G` 选择，例如 `/main/Inbox`。默认读取 `~/Maildir` 和 `~/.cache/mu`，不修改正在运行的 Emacs。

- `RET`：右侧打开整棵回复树，默认展开。
- `g`：重新查询本地索引。
- `/`：当前 Maildir 内搜索，支持 mu 查询语法；空字符串恢复普通列表。
- `n`：下一页。
- `c` / `r`：新建 / 回复，summary 所在窗口切换为 Message 编辑 buffer，右侧阅读窗口保留。
- `D`：打开当前账户的草稿组；`RET` 恢复编辑。
- `a`：summary 中归档当前组内的整条讨论；阅读窗口中归档当前这一封。
- 每封邮件下的附件按钮：选择保存位置，不覆盖已有文件。

启动前可设置 `ef-mail-root`、`ef-mail-muhome`、`ef-mail-sender` 和 `ef-mail-account`。发信地址由后端配置，`ef-mail-account` 选择 SMTP profile。`ef-mail-smtp-profiles` 的每一项为 `(名称 主机 端口 用户名 密码文件 CA文件)`；CA 文件为空时使用系统信任库。未配置 SMTP 时仍可阅读、编辑草稿和归档，发送会失败。

账户映射通过 `ef-mail-accounts` 配置 Maildir 前缀、发件地址和 SMTP profile。`ef-mail-smtp-profiles` 指定 SMTP 主机、端口、用户名、密码文件及 CA 文件；凭据只在发送时读取。账户配置示例见 [ENTRIES.md](ENTRIES.md)。

原生阅读界面采用文本 / SHR，Olivetti 已加载时会启用它。**约定的默认 WebKit 整树阅读尚未接入**。界面、窗口、撰写和草稿回调均使用 Ecaml。

## 阅读与身份

`Maildir_backend.Readable` 实现统一阅读接口，`Maildir_backend.Writing` 提供邮件专属 draft/receipt。由 `src/application.ml` 直接调用，可与 Reddit 共用同一个工作 domain。

列表和搜索只读索引元数据。组内搜索同时采用 mu 查询约束和结果 Maildir 校验。按 Message-ID / 引用关系合并 summary 条目，不以相同主题合并讨论；列表以当前结果中最新的一封作为打开入口。邮件的 `thread_id` 保持 `None`，`Thread` 参数是入口邮件 ID，不是永久 thread ID。

打开 thread 时，mu 查找相关邮件，OCaml 先发布元数据，再逐封解析正文，最后发布完成状态。父子关系优先取单一 `In-Reply-To`，否则取 `References` 最后一项。缺失父邮件保留为孤立节点；渲染有循环保护。每封邮件同时发布附件元数据，保存附件时重新解码并写入所选文件。Mrmime 处理 HTML、文本、字符集、quoted-printable、base64、编码标题和附件名。附带的 `message/rfc822` 作为附件，不混入外层回复树。

ID 为 `mail:` 加 Message-ID，跨文件夹副本共享身份。没有 Message-ID 时退化为本地路径摘要，重命名后不保证身份延续。普通邮件会重新查询索引路径，以适应 Maildir 标志引起的重命名；索引过时或文件内容身份不符会报错。

分页使用会话内结果快照，保留最近 8 个；过期 cursor 要求刷新，cursor 不能跨组或查询复用。每次最多查询约 5000 条元数据，超过时提示截断，需收窄查询。尚未实现任意大邮箱的完整流式遍历。相关邮件范围是**当前本地索引**，不代表服务器上的全部邮件。

## 后端边界与异步宿主

mu 命令、查询语法、Maildir 移动及索引维护都属于这个后端，核心只负责通用任务调度。`src/backends/maildir/process.ml` 是 Maildir 实现使用的 Emacs 宿主能力，不是所有邮件后端都必须实现的接口。未来 JMAP 可以成为独立后端，直接使用网络 API，不依赖 mu、mbsync、子进程接口或这里的 SMTP 模块；目前尚未实现 JMAP。

`src/backends/maildir/process.ml` 随 Maildir 后端组织，提供该后端需要的异步子进程支持。工作 domain 提交普通 OCaml 请求，通过 Eio 挂起等待；主线程用 Emacs `make-process` 管理子进程、输出和退出状态。不另设 SIGCHLD handler，不调用 Eio process manager。取消或超时会请求 Emacs 终止相应子进程。

mu 的 argv 按参数传递，不拼接 shell 命令。子进程默认超时 60 秒，stdout/stderr 合计上限 16 MiB；stderr 只计量、不转发，避免写入敏感日志。SMTP 不再经过这个宿主，也不再将发送正文写入临时 stdin 文件。

SMTP 在 Eio 工作 domain 内直接使用 TCP、tls-eio 和 Colombe 的调度器无关客户端。当前支持隐式 TLS（通常为 465）和 AUTH PLAIN，验证系统或指定 CA 及服务器主机名，没有明文降级；暂不支持 STARTTLS、OAuth2 或读取 msmtp 配置。连接、TLS 握手及提交共享 60 秒超时，等待网络时可取消且不阻塞 Emacs。Colombe 的 SMTP DATA 流按行提供，确保点转义正确。

原始邮件和解码数据限制为 32 MiB；流式收集限制 4096 个叶子，遍历和重建限制深度 64。Mrmime 解析本身仍在工作 domain 同步执行，深度检查不覆盖解析器内部；取消需等待当前解析返回。支持 LF 编辑草稿、折叠编码标题、RFC2231 附件名、二进制附件和附带邮件。CRLF 输入保留原始二进制字节；LF-only 且显式声明 binary 的输入拒绝处理，避免转换换行损坏附件。附带邮件下载会由 Mrmime 重建 MIME，不承诺与原附件逐字节相同。未知字符集目前按 UTF-8 尽力显示，不能保证覆盖 GMime 支持的所有编码及畸形邮件。

## 草稿与发送

新建和回复立即生成账户 `Drafts/cur` 中的本地草稿。编辑停止 0.75 秒后自动保存，`C-x C-s` / `C-c C-s` 手动保存，`C-c C-k` 保存并关闭，`C-c C-c` 明确发送。关闭编辑 buffer 或正常退出 Emacs 时也尝试保存；保存失败会保留编辑内容并报告错误。

草稿使用 `0600` 文件和原子替换，保存前检查版本，避免覆盖已经变化的文件。索引被锁住时，草稿仍可成功落盘；草稿组直接扫描文件，因此下次仍能恢复。当前扫描 `Drafts/cur`，尚未分页，单次保存上限 1 MiB。草稿是否同步到服务器由用户的 mbsync 配置决定。

编辑器发送使用后端的 `draft-send`：按账户选择 SMTP profile 并通过 Colombe 提交，成功后保存包含完整已提交 MIME 的 Sent 副本，再清理原草稿。发送前持久记录提交状态；异常、超时、取消或重启后的不确定结果不会自动重发。发送成功但本地清理失败时，会保留草稿并明确提示已经发送，避免再次提交。目前还没有不确定发送状态的恢复界面，也没有离线 outbox 或联网自动发送。

当前编辑与发送只支持纯文本。包含附件、HTML 或 MML 标记的草稿会拒绝发送，避免静默丢失内容；Bcc、加密和签名也尚未支持。

## 邮件归档

归档是移动到同一账户的 `Archive` Maildir。summary 的操作仅移动当前源组中属于该讨论的邮件，阅读窗口的操作只移动选中的一封；其他账户副本及 Sent 中的相关邮件保留。Drafts 组不执行此操作。

使用 `mu move --change-name` 更新文件和索引，保留原始 MIME（包括附件）及已读等标志，重命名时移除旧的 mbsync UID 信息。不存在的 Archive 会先创建并登记到索引。多封移动可能部分失败，界面会报告成功与失败数量。后续由已有 mbsync 同步这次本地移动；本后端不会自动启动 mbsync。

这实现的是邮件文件夹归档。跨后端多文件夹收藏已在统一 feed 界面实现，独立保存正文和原始 MIME，并在手动刷新时更新新增回复；见 [README](../README.md#阅读与内容管理)。后台自动持续同步仍待实现。

## 底层发送接口与边界

原生 `send` 接受收件地址列表、抄送、主题、正文和可选回复目标，生成 Message-ID、日期及 UTF-8 MIME；回复补充 `In-Reply-To` / `References`，通过配置的 SMTP profile 提交。目前只支持文本撰写，不支持发信附件、Bcc、加密或签名。

一次请求只提交一次。成功表示 SMTP 会话成功结束；连接中断、超时或取消不能简单视为“服务器肯定没收到”，不可自动重试。发送和附件保存是服务任务，关闭阅读视图不会取消它们。后端不执行远端删除，也不改变 Maildir 已读标志。

底层 `send` 接口仍是单次发送，不提供 `draft-send` 的持久提交标记和 Sent 保存流程；编辑器使用后者。既有 mbsync 继续负责收信与远端同步；本后端会为草稿、Sent 和归档操作更新 mu 索引。

## 验证

`make test` 创建独立合成 Maildir / mu 索引，不读取个人邮件。覆盖 MIME 编码、二进制附件、组内范围、引用合并、同主题不合并、分页、header 注入拒绝及 SMTP 不自动重试；真实原生模块集成测试覆盖渐进阅读、附件按钮、Emacs 心跳、取消和宿主信号。

发信集成测试使用监听 loopback 的 TLS SMTP 测试服务，只保存测试邮件，不向外投递。验证证书与主机名检查、认证、回复头、账户路由、等待期间 Emacs 心跳和取消，以及关闭阅读视图后发送继续完成。没有连接真实 Migadu SMTP 或读取真实密码。

真实本地只读验证需显式配置：

```sh
MAIL_ROOT="$HOME/Maildir" MAIL_MUHOME="$HOME/.cache/mu" \
MAIL_SENDER='your-address@example.org' MAIL_GROUP='/account/Inbox' \
nix develop -c make mail-live
```

只输出条目数量和状态，不输出主题、地址、正文，不发信、不标记已读、不重建个人索引。本次 Mrmime 替换后的本机只读验证返回 8 个 summary 条目，打开并解码了 1 封真实邮件，组内搜索通过，记录见 `pure-ocaml-mail-live-results.txt`。另有合成回归测试保证长 Message-ID 不会因格式化折行而改变。

最新完整验证通过 OCaml 测试及 26 个 Emacs 集成测试：9 个运行时、4 个 Reddit、8 个邮件阅读/发送、5 个草稿/归档流程。新增测试覆盖自动保存和窗口布局、跨重启恢复、版本冲突、账户选择、归档字节/标志/范围、Sent 副本以及不确定发送后禁止重放。归档只操作合成邮箱，发信只连接 loopback TLS 测试服务，没有移动个人邮件或实际发信。记录见 `pure-ocaml-mail-test-results.txt`。本次未做图形 WebKit 或长时间运行验证。

参考：[Emacs 异步进程](https://www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html)、[mu 源码](https://github.com/djcb/mu)、本机 mu 1.14.3 帮助、[Mrmime](https://github.com/mirage/mrmime)、[Colombe](https://github.com/mirage/colombe) 和 [tls-eio](https://github.com/mirleft/ocaml-tls)。当前 Nix 依赖为 Mrmime 0.7.0、Colombe/sendmail 0.12.1、tls-eio 2.1.3；这里的 `sendmail` 是 OCaml 库，不是外部命令。
