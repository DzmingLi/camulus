# Mu 后端：本地索引、Maildir 与邮件同步

这一版是本地 Maildir 邮件后端，保留 mu 索引和现有 mbsync 同步。`Mu_backend` 使用 Mrmime 解析及生成 MIME，通过 Colombe + tls-eio 直接发送 SMTP；不再绑定 GMime 或调用 msmtp。`process_emacs.ml` 通过 Ecaml 提供 mu 子进程宿主；`compose_emacs.ml` 负责邮件编辑格式、草稿组以及 Mu 操作接入，公共 `src/writing.ml` 管理自动保存、编辑版本、关闭和发送状态。没有以 Gnus 或 mu4e 作为后端，也没有 Elisp 后端扩展接口。

## 运行

在项目目录执行：

```sh
nix develop -c make reader-demo
```

它启动新的 `emacs -Q` 并打开统一首页；在入口配置中声明 Maildir，再用 `G` 选择，例如 `/main/Inbox`。默认读取 `~/Maildir` 和 `~/.cache/mu`，不修改正在运行的 Emacs。

- `RET`：右侧打开整棵回复树，默认展开。
- `g`：重新查询本地索引。
- `n`：下一页。
- `c` / `r`：新建 / 回复，summary 所在窗口切换为 Message 编辑 buffer，右侧阅读窗口保留。
- `D`：打开当前账户的草稿组；`RET` 恢复编辑。
- `a`：summary 中归档当前组内的整条讨论；阅读窗口中归档当前这一封。
- 邮件有附件时，在正文窗口下方显示附件面板；随当前邮件切换。`A` 手动打开，面板中 `RET` / `o` 打开附件，`s` 保存当前附件，`S` 保存全部，`q` 关闭。正文不再显示附件按钮。

启动前可设置 `camulus-mail-root`、`camulus-mail-muhome`、`camulus-mail-sender` 和 `camulus-mail-account`。发信地址由后端配置，`camulus-mail-account` 选择 SMTP profile。`camulus-mail-smtp-profiles` 的每一项为 `(名称 主机 端口 用户名 密码文件 CA文件)`；CA 文件为空时使用系统信任库。未配置 SMTP 时仍可阅读、编辑草稿和归档，发送会失败。

账户映射通过 `camulus-mail-accounts` 配置 Maildir 前缀、发件地址和 SMTP profile。`camulus-mail-smtp-profiles` 指定 SMTP 主机、端口、用户名、密码文件及 CA 文件；凭据只在发送时读取。账户配置示例见 [ENTRIES.md](ENTRIES.md)。

设置 `(setopt camulus-mail-sync-command '("mbsync" "-a"))` 后，进入首页或邮件组时自动同步，`M-x camulus-mail-sync` 可手动同步。每次成功后运行 `mu index`，更新首页邮件来源和打开的邮件列表。同步尚未完成时跳过重复触发；失败时保留缓存，下一次进入或手动刷新可重试。默认 `camulus-mail-sync-interval` 为 `0`，不定时轮询；显式设置正数才启用周期同步。未配置同步命令时使用纯本地 Maildir。

原生阅读界面采用文本 / SHR，Olivetti 已加载时会启用它。**约定的默认 WebKit 整树阅读尚未接入**。界面、窗口、撰写和草稿回调均使用 Ecaml。

## 阅读与身份

`Mu_backend` 直接提供阅读、发送和邮件专属 draft/receipt。由 `mu_emacs.ml` 注册会话和执行能力，公共运行时通过 worker 接口调用，可与 Reddit 共用同一个工作 domain。整体关系见 [架构文档](ARCHITECTURE.md)。

列表只读索引元数据，按当前 Maildir 查询并校验结果范围。按 Message-ID / 引用关系合并 summary 条目，不以相同主题合并讨论；列表以当前结果中最新的一封作为打开入口。邮件的 `thread_id` 保持 `None`，`Thread` 参数是入口邮件 ID，不是永久 thread ID。

打开 thread 时，mu 查找相关邮件，OCaml 先发布元数据，再逐封解析正文，最后发布完成状态。父子关系优先取单一 `In-Reply-To`，否则取 `References` 最后一项。缺失父邮件保留为孤立节点；渲染有循环保护。每封邮件同时发布附件元数据，保存附件时重新解码并写入所选文件。Mrmime 处理 HTML、文本、字符集、quoted-printable、base64、编码标题和附件名。附带的 `message/rfc822` 作为附件，不混入外层回复树。

ID 为 `mail:` 加 Message-ID，跨文件夹副本共享身份。没有 Message-ID 时退化为本地路径摘要，重命名后不保证身份延续。普通邮件会重新查询索引路径，以适应 Maildir 标志引起的重命名；索引过时或文件内容身份不符会报错。

分页使用会话内结果快照，保留最近 8 个；过期 cursor 要求刷新，cursor 不能跨组或查询复用。查询一次收集全部匹配的元数据，再按页显示，尚未实现完整流式遍历。相关邮件范围是**当前本地索引**，不代表服务器上的全部邮件。

## 后端边界与异步宿主

mu 命令、查询语法、Maildir 移动及索引维护都属于这个后端，核心只负责通用任务调度。`src/backends/mu/process.ml` 是 Maildir 实现使用的 Emacs 宿主能力，不是所有邮件后端都必须实现的接口。未来 JMAP 可以成为独立后端，直接使用网络 API，不依赖 mu、mbsync、子进程接口或这里的 SMTP 模块；目前尚未实现 JMAP。

`src/backends/mu/process.ml` 随 Maildir 后端组织，提供该后端需要的异步子进程支持。工作 domain 提交普通 OCaml 请求，通过 Eio 挂起等待；主线程用 Emacs `make-process` 管理子进程、输出和退出状态。不另设 SIGCHLD handler，不调用 Eio process manager。取消或超时会请求 Emacs 终止相应子进程。

mu 的 argv 按参数传递，不拼接 shell 命令。子进程默认超时 60 秒，启动后关闭 stdin，stdout 完整收集，stderr 直接丢弃，不转发到日志。宿主只执行命令，不再维护未使用的 stdin 临时文件和 shell 转发；SMTP 直接通过原生连接发送。查询不设邮件总数上限，列表仍按页显示；正文、附件、草稿和收件人数不设业务配额，大内容按实际大小占用内存。

SMTP 在 Eio 工作 domain 内直接使用 TCP、tls-eio 和 Colombe 的调度器无关客户端。当前支持隐式 TLS（通常为 465）和 AUTH PLAIN，验证系统或指定 CA 及服务器主机名，没有明文降级；暂不支持 STARTTLS、OAuth2 或读取 msmtp 配置。连接、TLS 握手及提交共享 60 秒超时，等待网络时可取消且不阻塞 Emacs。Colombe 的 SMTP DATA 流按行提供，确保点转义正确。

原始邮件、解码数据、MIME 部件数量和嵌套深度不设业务配额。Mrmime 解析本身在工作 domain 同步执行，取消需等待当前解析返回；极深的内容仍受解析器及 OCaml 栈能力约束。支持 LF 编辑草稿、折叠编码标题、RFC2231 附件名、二进制附件和附带邮件。CRLF 输入保留原始二进制字节；LF-only 且显式声明 binary 的输入拒绝处理，避免转换换行损坏附件。附带邮件下载会由 Mrmime 重建 MIME，不承诺与原附件逐字节相同。未知字符集目前按 UTF-8 尽力显示，不能保证覆盖 GMime 支持的所有编码及畸形邮件。

## 草稿与发送

新建和回复立即生成账户 `Drafts/cur` 中的本地草稿。编辑停止 0.75 秒后自动保存（`camulus-writing-autosave-delay`），`C-x C-s` / `C-c C-s` 手动保存，`C-c C-k` 保存并关闭，`C-c C-c` 明确发送。关闭编辑 buffer 或正常退出 Emacs 时也尝试保存；保存失败会保留编辑内容并报告错误。

草稿使用 `0600` 文件和原子替换，保存前检查版本，避免覆盖已经变化的文件。索引被锁住时，草稿仍可成功落盘；草稿组直接扫描文件，因此下次仍能恢复。当前扫描 `Drafts/cur`，尚未分页。草稿是否同步到服务器由用户的 mbsync 配置决定。

编辑器发送使用后端的 `draft-send`：按账户选择 SMTP profile 并通过 Colombe 提交，成功后保存包含完整已提交 MIME 的 Sent 副本，再清理原草稿。发送前持久记录提交状态；异常、超时、取消或重启后的不确定结果不会自动重发。发送成功但本地清理失败时，会保留草稿并明确提示已经发送，避免再次提交。目前还没有不确定发送状态的恢复界面，也没有离线 outbox 或联网自动发送。

当前编辑与发送只支持纯文本。包含附件、HTML 或 MML 标记的草稿会拒绝发送，避免静默丢失内容；Bcc、加密和签名也尚未支持。

## 邮件归档

在统一首页或邮件组的 summary 列表按 `d`，邮件后端会将当前组中属于该讨论的邮件移入同一账户的 `Trash`，成功后更新本地索引和列表缓存。失败时不写入 feed 移除标记；部分成功会报告数量并刷新实际剩余内容。已在 Trash 的邮件不会再次删除或永久清除。收藏文件夹中的 `d` 仍只取消收藏归属。

移入 Trash 保留完整 MIME、附件及已有标志，不额外添加会使 mbsync expunge 的 `T` 标志。其他账户和 Sent 中的副本保留；本地移动由下一次邮件同步推送到 IMAP。

归档是移动到同一账户的 `Archive` Maildir。summary 的操作仅移动当前源组中属于该讨论的邮件，阅读窗口的操作只移动选中的一封；其他账户副本及 Sent 中的相关邮件保留。Drafts 组不执行此操作。

使用 `mu move --change-name` 更新文件和索引，保留原始 MIME（包括附件）及已读等标志，重命名时移除旧的 mbsync UID 信息。不存在的 Archive 会先创建并登记到索引。多封移动可能部分失败，界面会报告成功与失败数量。后续由 mbsync 同步这次本地移动。

这实现的是邮件文件夹归档。跨后端多文件夹收藏已在统一 feed 界面实现，独立保存正文和原始 MIME，并在手动刷新时更新新增回复；见 [README](../README.md#阅读与内容管理)。

## 底层发送接口与边界

原生 `send` 接受收件地址列表、抄送、主题、正文和可选回复目标，生成 Message-ID、日期及 UTF-8 MIME；回复补充 `In-Reply-To` / `References`，通过配置的 SMTP profile 提交。目前只支持文本撰写，不支持发信附件、Bcc、加密或签名。

一次请求只提交一次。成功表示 SMTP 会话成功结束；连接中断、超时或取消不能简单视为“服务器肯定没收到”，不可自动重试。发送和附件保存是服务任务，关闭阅读视图不会取消它们。后端不执行远端删除，也不改变 Maildir 已读标志。

底层 `send` 接口仍是单次发送，不提供 `draft-send` 的持久提交标记和 Sent 保存流程；编辑器使用后者。既有 mbsync 继续负责收信与远端同步；本后端会为草稿、Sent 和归档操作更新 mu 索引。
