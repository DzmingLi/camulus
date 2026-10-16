# Camulus

使用 OCaml 5、Ecaml 和 Eio 构建的 Emacs 统一阅读器。将邮件、论坛讨论和个人 feed 放进同一个阅读界面，后端通过 OCaml 模块扩展。

**当前是可试用的开发原型。** 已有浏览、阅读、收藏归档，以及部分后端写入能力；完整后台同步、持久离线写入队列和邮件 WebKit 阅读仍待实现。目前构建目标为 **x86_64 Linux / Emacs 31**。

## 阅读与内容管理

- `M-x camulus` 打开统一时间线；组目录提供配置好的浏览入口。声明入口不等于订阅，订阅由后端可选能力管理。
- `RET` 在右侧打开文章或完整回复树，并转移焦点。树默认展开，同级回复按时间正序排列，定位第一条已加载的未读内容；渐进加载保留阅读位置。
- 已读只表示正文进入过可见区域，不会从列表移除。`d` 移除时间线里当前已处理的进展，保留订阅；后续新进展可以再次出现。
- `m` 收藏到文件夹并持久保存正文与附件。支持嵌套文件夹、同一内容加入多个文件夹，以及分别收藏整条讨论和单独一楼。`f` 打开收藏夹，可离线阅读。
- 搜索限定在当前组，由后端决定远端搜索或本地搜索。没有全局搜索。
- 阅读界面使用文本 / SHR；已安装并加载 Olivetti 时启用对应排版。

| 按键 | 行为 |
| --- | --- |
| `B` / `G` | 组目录 / 选择入口 |
| `h` | 回统一时间线 |
| `RET` | 打开当前内容 |
| `g` / `n` | 刷新 / 下一页 |
| `/` | 当前组内搜索，取决于后端能力 |
| `d` | 移除当前进展；收藏夹中则移除收藏归属 |
| `m` / `f` | 收藏 / 打开收藏夹 |
| `s` | 后端提供的订阅或追踪设置 |
| `C` / `e` | 编辑入口配置 / 查看来源错误 |

## 后端

| 后端 | 当前范围 |
| --- | --- |
| Maildir | mu 本地索引与搜索、回复树、MIME 附件、自动保存草稿、纯 OCaml SMTP 发送、移动到 Archive；同步仍使用外部 mbsync |
| Reddit | OAuth、subreddit、回复及提及、组内搜索、渐进回复树、投票；回复/发帖编辑界面未完成 |
| CS.RIN.RU | Atom 更新、phpBB 逐页回复树、附件归档、登录、BBCode 发帖/回复、自动保存草稿 |
| Discourse | 最新讨论、分类、回复及提及、追踪列表、远端搜索、渐进回复树、点赞、追踪设置、回复/发帖与草稿 |
| RSS / Atom | 分页、后端订阅列表、正文与 enclosure 附件归档 |
| JSON Feed 1 / 1.1 | `next_url` 分页、后端订阅列表、正文与附件归档 |
| Blogger | Atom 列表、打开时提取公开网页全文、图片归档 |
| 机核 | 用户动态、文章正文、图库；尚无评论树、点赞或发帖 |
| The Atlantic / Jane Street | 作者 feed 或文章列表、网页正文与图片 |
| 知乎 / 豆瓣 | 作者或个人入口、正文与图片；豆瓣支持渐进评论树；部分入口需要 Firefox 登录 Cookie |
| 网易云 | 公开动态、图片与歌曲链接；不下载歌曲音源 |
| Public Inbox | 原生 gzip / mboxrd / MIME 解析，以邮件回复关系重建讨论树 |

这些后端不依赖旧 Gnus 后端。Mastodon、GitHub / Forgejo 后端尚未实现。

## 使用 Nix 安装

`flake.nix` 通过 **opam-nix** 构建 OCaml 依赖及原生模块，包含宿主所需的 Ecaml / Eio 补丁。运行时不需要 OCaml 编译器。

```sh
git clone https://github.com/DzmingLi/camulus.git
cd camulus
nix build .#emacs-camulus
```

在 Emacs 配置中加入构建产物：

```elisp
(add-to-list 'load-path "/absolute/path/to/camulus/result/share/emacs/site-lisp")
(require 'camulus)
;; M-x camulus
```

也可直接加入自己的 Nix flake：

```nix
inputs.camulus.url = "github:DzmingLi/camulus";
inputs.camulus.inputs.nixpkgs.follows = "nixpkgs";

# 在已有的 emacsWithPackages 调用中：
emacsWithPackages (epkgs: [
  (inputs.camulus.lib.mkEmacsPackage epkgs)
])
```

建议使用支持动态模块的 Emacs 31。打包后加载的是原生 OCaml runtime；更新原生模块需要启动新的 Emacs 进程。

## 配置

入口文件默认为 `~/.config/camulus/entries.json`，或 `$XDG_CONFIG_HOME/camulus/entries.json`。从 [配置示例](examples/entries.json) 开始，替换其中的用户名、URL 和本地路径：

```json
{
  "version": 1,
  "entries": [
    {
      "name": "NixOS forum",
      "source": "discourse:https://discourse.nixos.org",
      "group": "latest"
    },
    {
      "name": "Jane Street",
      "source": "janestreet",
      "group": "posts",
      "timeline": true
    }
  ]
}
```

`timeline: true` 明确将入口加入首页；RSS 和 JSON Feed 另有各自后端管理的订阅文件。只声明可浏览的论坛、subreddit 或 feed URL 不会自动订阅。

非秘密的账户、Maildir 路径、Firefox profile 等放在 `backend_settings`，密钥使用 auth-source 或独立凭据文件。完整说明见 [入口与订阅](docs/ENTRIES.md)、[邮件](docs/MAIL.md)、[Reddit](docs/REDDIT.md)、[Discourse](docs/DISCOURSE.md) 和 [RSS](docs/RSS.md) 和 [CS.RIN.RU](docs/CSRIN.md)。

本地内容状态默认为 `~/.local/state/camulus/state.sqlite3`，遵循 `XDG_STATE_HOME`。已读、移除、收藏归属和归档相互独立。普通条目保存列表元数据及操作状态；收藏才持久保存正文和附件。最后一个收藏归属移除后可清理归档，不会阻止以后重新获取内容。

## 开发

```sh
nix develop -c make reader-demo
nix develop -c make test
# 在 Nix 构建沙箱中运行完整测试：
nix build .#checks.x86_64-linux.reader
```

开发构建默认使用 `_build/`，可通过 `BUILD_DIR=/absolute/path` 覆盖。测试包括 OCaml 单元测试、Emacs 原生模块与界面集成、Python 导入器，以及本机 HTTP/SMTP 模拟服务；普通测试不写入真实远端账户。

```text
src/
  backend.ml          后端基本接口及可选能力
  content.ml          公共内容与分页类型
  runtime.ml          Eio worker、任务生命周期与取消
  reader_ui.ml        Ecaml 列表、阅读、窗口与收藏交互
  content_state.ml    本地状态与归档
  backends/
    reddit/           每个后端拥有自己的协议、解析与身份管理
    discourse/
    maildir/          mu / mbsync 的进程调用也属于此后端
    ...
lisp/                 配置、宿主启动及尚未迁移的撰写流程
patches/              Ecaml / Eio 的宿主适配补丁
```

Emacs 主线程通过 Ecaml 操作窗口和 buffer，独立 OCaml domain 运行 Eio。后端基本接口不强制 thread、点赞、投票、写入或订阅；这些作为可选模块提供。`*_emacs.ml` 是各后端在 Emacs 中的命令和能力注册，同一目录中的其他模块处理协议与内容。详情参见源码接口及 [运行时验证说明](docs/VALIDATION.md)。

当前刷新发生在打开首页和手动操作时。后台增量同步、完整回复/提及规则、持久离线写入重试、Reddit 完整撰写、邮件附件撰写和邮件 WebKit 整树阅读仍待完成。不要把已实现的会话任务调度视为持久同步队列。

## 许可

项目默认采用 [AGPL-3.0-or-later](LICENSE)。Reddit 后端保留 GPL-3.0-or-later，其他第三方来源及补丁声明见 [NOTICE](NOTICE) 和对应后端目录。旧 Gnus 后端的历史来源引用保留在 NOTICE 中；原仓库删除后可能无法访问。
