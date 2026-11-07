# Camulus

使用 OCaml 5 和 Eio 构建的统一阅读器，提供 Ecaml / Emacs 与 Bonsai / Web 前端。将邮件、论坛讨论和个人 feed 放进同一个阅读界面，后端通过 OCaml 模块扩展。

**当前是可试用的开发原型。** 已有浏览、阅读、收藏归档，以及部分后端写入能力；完整后台同步、持久离线写入队列和邮件 WebKit 阅读仍待实现。目前构建目标为 **x86_64 Linux / Emacs 31**。

## 阅读与内容管理

- `M-x camulus` 打开统一时间线；组目录提供配置好的浏览入口。声明入口不等于订阅，订阅由后端可选能力管理。
- `RET` 在右侧打开文章或完整回复树，并转移焦点。树默认展开，同级回复按时间正序排列，定位第一条已加载的未读内容；渐进加载保留阅读位置。
- 已读只表示正文进入过可见区域，不会从列表移除。`d` 移除时间线里当前已处理的进展，保留订阅；后续新进展可以再次出现。
- `m` 选择或创建收藏夹。收藏会持久保存正文、图片和邮件附件。支持嵌套文件夹、同一内容加入多个文件夹，以及分别收藏整条讨论和单独一楼。首页、组目录及阅读界面都可按 `f` 或点击顶部 `[收藏夹 · f]` 打开收藏夹目录，再点击文件夹浏览已保存内容。收藏可离线阅读。
- 普通列表中的收藏条目显示为红色；收藏夹内部使用普通颜色。Nix 包自动引入 `emacs-webkit`，由 Camulus 的[公共 WebKit 接口](docs/WEBKIT.md)统一加载。图形界面提供 GStreamer 解码插件后，正文视频可原位播放；重绘正文保留播放器和播放进度。浏览器模块不可用时保留可点击的视频链接。
- 搜索限定在当前组，由后端决定远端搜索或本地搜索。没有全局搜索。
- 阅读界面使用文本 / SHR，默认内嵌显示题图和正文图片；已安装并加载 Olivetti 时启用对应排版。

| 按键 | 行为 |
| --- | --- |
| `B` / `G` | 组目录 / 选择入口 |
| `h` | 回统一时间线 |
| `RET` | 打开当前内容 |
| `g` / `n` | 刷新 / 下一页 |
| `/` | 当前组内搜索，取决于后端能力 |
| `d` | 移除当前进展；收藏夹中则移除收藏归属 |
| `m` / `f` | 选择收藏夹 / 打开收藏夹 |
| `s` | 后端提供的订阅或追踪设置 |
| `U` / `p` | 订阅面板 / 在面板中设置来源优先级 |
| `P` | 主页选择显示哪些优先级 |
| `C` / `e` | 编辑入口配置 / 查看来源错误 |

## 后端

| 后端 | 当前范围 |
| --- | --- |
| Maildir | mu 本地索引与搜索、回复树、MIME 附件、自动保存草稿、纯 OCaml SMTP 发送、移动到 Archive；同步仍使用外部 mbsync |
| Reddit | OAuth、subreddit、回复及提及、组内搜索、渐进回复树、投票；回复/发帖编辑界面未完成 |
| Discourse | 最新讨论、分类、远端通知、会话追踪管理、远端搜索、渐进回复树、点赞与追踪设置；回复/发帖已有后端接口，编辑界面待接入 |
| RSS / Atom | 分页、后端订阅列表、正文与 enclosure 附件归档 |
| JSON Feed 1.1 | `next_url` 分页、后端订阅列表、正文与附件归档 |
| Blogger | Atom 列表、打开时提取公开网页全文、图片归档 |
| 机核 | 用户动态、文章正文、图库；尚无评论树、点赞或发帖 |
| The Atlantic / Jane Street | 作者 feed 或文章列表、网页正文与图片 |
| 知乎 / 豆瓣 | 作者或个人入口、正文与图片；豆瓣支持渐进评论树及[广播、帖子和评论点赞](docs/DOUBAN.md)；两者均使用 Camulus 的[公共 Firefox Cookie 能力](docs/FIREFOX_COOKIES.md) |
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

Nix 包包含标准 ELPA 元数据和 autoload。加入 Emacs 包集合并 rebuild 后，在新 Emacs 进程直接 `M-x camulus` 打开首页，不需要手动 `require` 或加载 `.so`。

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

## 网页前端

```sh
nix run .#web
# 浏览器打开 http://127.0.0.1:8765
```

首版网页接通 RSS / Atom、JSON Feed 1.1 和 Discourse 浏览，支持组内搜索、分页、渐进回复树、可见内容已读、收藏夹、归档正文、来源优先级和共享设置。点击“组浏览”查看已登记入口，点击“订阅”管理 RSS / JSON Feed 的关注选择。普通列表的收藏显示红色，收藏夹内不标红。

原生服务端使用 OCaml 5.3 + Eio + Cohttp-eio，浏览器使用 Jane Street Bonsai v0.17 + js_of_ocaml 5.6、独立 OCaml 5.1 工具链。网页能力、认证配置和开发方式见 [WEB.md](docs/WEB.md)。其他后端目前仍通过 Emacs 使用；网页不自动获得其他网站的登录 Cookie。

## 配置

来源和显示设置统一保存在 `$XDG_CONFIG_HOME/camulus/settings.json`，默认 `~/.config/camulus/settings.json`。网页通过设置页面提交给服务端，服务端校验并原子保存；Emacs 的 `C` / `M-x camulus-reader-edit-config` 编辑同一个文件，保存前校验。连接参数变动后需重启对应会话。

```json
{
  "version": 1,
  "groups": [
    {"name": "NixOS", "source": "discourse:https://discourse.nixos.org", "group": "latest"},
    {"name": "Anil", "source": "jsonfeed:https://anil.recoil.org/feeds/feed.json", "group": "https://anil.recoil.org/feeds/feed.json"}
  ],
  "priorities": [],
  "default_priority": 2,
  "home_priorities": [],
  "backend_settings": {}
}
```

注册只建立浏览入口。订阅仍是后端管理的运行状态：Discourse 会话追踪保存在远端，RSS / JSON Feed 关注列表等本地状态保存在 `$XDG_STATE_HOME/camulus/`。设置文件不包含订阅列表。

Emacs `U` 打开订阅面板，`p` 设置来源优先级，主页 `P` 选择显示级别。网页订阅卡片也能设置级别，主页筛选为空表示全部。优先级按 `(source, group)` 设置，筛选不改变订阅、已读或收藏。

不再使用 Customize / `setopt` 维护另一份 Camulus 配置，也不保留旧 JSON 导入命令。Emacs 中同名变量只作为后端适配器的内部镜像。密钥存放在独立凭据文件或 auth-source，设置中只引用路径。

详情见 [入口与订阅](docs/ENTRIES.md)、[邮件](docs/MAIL.md)、[Reddit](docs/REDDIT.md)、[Discourse](docs/DISCOURSE.md) 和 [网页前端](docs/WEB.md)。

## 开发

完整的模块职责、请求流转、effects 与队列、订阅和状态模型，以及多前端演进方向，见 [代码结构与总体设计](docs/ARCHITECTURE.md)。

```sh
nix develop -c make reader-demo
nix develop -c make test
# 在 Nix 构建沙箱中运行完整测试：
nix build .#checks.x86_64-linux.reader .#checks.x86_64-linux.startup
```

开发构建默认使用 `_build/`，可通过 `BUILD_DIR=/absolute/path` 覆盖。测试包括 OCaml 单元测试、Emacs 原生模块与界面集成，以及本机 HTTP/SMTP 模拟服务；普通测试不写入真实远端账户。

```text
src/
  backend.ml          后端基本接口及可选能力
  content.ml          公共内容与分页类型
  runtime.ml          Eio worker、任务生命周期与取消
  reader_ui.ml        Ecaml 列表、阅读、窗口与收藏交互
  reader_config.ml    声明式配置与后端启动
  content_state.ml    本地状态与归档
  backends/
    reddit/           每个后端拥有自己的协议、解析与身份管理
    discourse/
    maildir/          mu / mbsync 的进程调用也属于此后端
    ...
lisp/camulus.el       仅加载原生模块；无后端或交互逻辑
patches/              Ecaml / Eio 的宿主适配补丁
```

邮件撰写、草稿、Discourse 认证与点赞、Reddit 授权与投票、进程宿主和配置加载都在 OCaml 模块中实现。Emacs 主线程通过 Ecaml 操作窗口和 buffer，独立 OCaml domain 运行 Eio。跨 domain 消息传递使用 Saturn 的有界 SPSC 队列，命令／事件容量分别为 256／512，邮件进程请求队列容量为 64。后端基本接口不强制 thread、点赞、投票、写入或订阅；这些作为可选模块提供。`*_emacs.ml` 是各后端在 Emacs 中的命令和能力注册，同一目录中的其他模块处理协议与内容。详情参见源码接口及 [运行时验证说明](docs/VALIDATION.md)。

普通来源在打开首页和手动操作时刷新；邮件还提供启动及定时同步。统一后台增量同步、完整回复/提及规则、持久离线写入重试、Reddit 完整撰写、邮件附件撰写和邮件 WebKit 整树阅读仍待完成。不要把已实现的会话任务调度视为持久同步队列。

## 许可

项目默认采用 [AGPL-3.0-or-later](LICENSE)。Reddit 后端保留 GPL-3.0-or-later，其他第三方来源及补丁声明见 [NOTICE](NOTICE) 和对应后端目录。旧 Gnus 后端的历史来源引用保留在 NOTICE 中；原仓库删除后可能无法访问。
