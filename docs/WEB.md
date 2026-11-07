# Bonsai 网页前端

这是直接复用 OCaml 后端和内容状态服务的第一版网页客户端。浏览器代码全部用 OCaml / Bonsai 编写，再由 js_of_ocaml 编译。原生服务通过 Cohttp-eio 提供 HTTP 接口，默认只监听 `127.0.0.1:8765`。

## 运行与配置

```sh
nix run .#web
# 可覆盖端口、配置和状态目录：
nix run .#web -- --port 8766 --config /absolute/settings.json --state-dir /absolute/state
```

设置位置是 `$XDG_CONFIG_HOME/camulus/settings.json`，默认 `~/.config/camulus/settings.json`。网页“设置”可注册来源和编辑共享设置；浏览组与订阅分离。主页先显示缓存，按“刷新”重新拉取选入优先级的订阅来源。

网页已接通 RSS / Atom、JSON Feed 1.1 和 Discourse 的组浏览、搜索及线程阅读。其他后端的注册入口保留并显示“尚未接入网页”；它们仍可在 Emacs 使用。网页只汇入已经连接的后端，不冒充完整的 Emacs 时间线。

RSS / JSON Feed 的订阅按钮读写后端原有状态文件。Discourse 不提供订阅全站按钮；认证站点提供会话通知流。网页服务可从独立文件读取 User API 凭据：

```json
{
  "discourse": {
    "sites": [
      {"site": "https://emacs-china.org", "credentials_file": "/private/emacs-china.json"}
    ]
  }
}
```

这段放在 `backend_settings` 内。凭据文件包含 `api_key` 和 `client_id` 字符串，应设为 0600；内容不会返回给网页。没有凭据仍可浏览公开话题。已有 Emacs `auth_sources` 设置继续由 Emacs 适配器读取，网页尚不解析 auth-source 文件。

## 状态与交互

- 时间线沿用 `Content_state` 的身份合并、来源时间排序和移除进展语义。取消订阅不会删除浏览入口；注册组不会订阅。
- 已读在正文进入可见区域时提交。普通列表已读变灰、收藏变红，收藏夹内不使用收藏红色。
- 点击文章打开右侧阅读区；讨论默认展开、同级按时间排列。分批结果逐步更新，跳转其他内容后丢弃旧结果。
- 收藏可选择或创建嵌套文件夹，正文通过现有归档操作持久保存。网页能离线打开已归档正文。已下载媒体在存储中保留，但网页的本地媒体 URL 重写及下载接口尚未接入；图片和视频目前仍使用原始 URL。
- `m` 打开收藏夹选择，时间线中的 `d` 移除所选进展。组内搜索使用后端接口。
- 来源优先级是共享设置；主页显示哪些级别与订阅状态独立。

## 代码与边界

| 文件 | 责任 |
| --- | --- |
| [settings.ml](../src/settings.ml) | 共享配置类型、校验、0600 原子保存与跨进程文件锁 |
| [service.ml](../src/service.ml) | worker 契约，读取、刷新、已读、移除、归档和收藏操作 |
| [feed_service.ml](../src/backends/rss/feed_service.ml) | RSS / JSON Feed 共用的 worker functor，Emacs 与网页各自实例化 |
| [discourse_service.ml](../src/backends/discourse/discourse_service.ml) | Discourse worker，脱离 Ecaml |
| [web_backend.ml](../src/frontends/web/server/web_backend.ml) | 网页服务的后端组装和配置，不实现协议 |
| [camulus_web.ml](../src/frontends/web/server/camulus_web.ml) | HTTP、受限异步任务、配置冲突检查、正文 HTML 清理与静态文件 |
| [main.ml](../src/frontends/web/client/main.ml) | Bonsai 页面、交互与任务进度展示 |

网页请求创建 Eio 任务后轮询结果，服务端可在任务完成前发布回复批次。任务有并发数、总结果大小、超时和保留数量限制；刷新晚返回的结果通过 `Service` 的入口版本检查避免覆盖较新缓存。

HTTP 服务校验 Host 和 Origin，写请求要求自定义请求头，不开放 CORS；未提供远程部署认证，不应通过反向代理公开。HTML 使用标签和属性白名单，移除脚本、事件、SVG、表单与危险链接；媒体由浏览器原生渲染，CSP 禁止任意脚本和嵌套页面。网页不能直接获得其他网站的 Cookie。

目前两端共用配置文件、持久内容状态和服务实现，Emacs 仍运行自己的宿主会话，尚未改为连接唯一守护进程。网页暂不包含邮件、其他出版物后端、撰写界面、投票和完整的会话追踪管理 UI；服务端已有 Discourse 追踪查询/修改接口。这些是明确的后续工作，不是独立重写业务逻辑的理由。

## 开发与验证

Bonsai v0.17 的 js_of_ocaml 5.6 编译器要求 OCaml < 5.2，因此客户端用独立 opam-nix scope（OCaml 5.1.1）。原生端继续 OCaml 5.3 / Eio 1.6。客户端目录有自己的 Dune 项目，父项目排除该目录：

```sh
nix develop .#web-client -c sh -c 'cd src/frontends/web/client && dune build --root . main.bc.js index.html style.css'
nix develop -c make web-server
_build/default/src/frontends/web/server/camulus_web.exe --assets src/frontends/web/client/_build/default
nix develop -c make test-web
```

`test/web-integration.py` 使用隔离状态和本地 RSS / JSON Feed 服务，覆盖注册不订阅、后端订阅持久化、优先级过滤、已读、收藏归档、移除去重、重启恢复、Host/Origin 校验及安全 HTML / video 输出。现有 Emacs 与后端测试继续运行。
