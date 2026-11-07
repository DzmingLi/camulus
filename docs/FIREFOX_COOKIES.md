# 公共 Firefox Cookie 能力

`Reader.Credentials.Firefox` 是 Camulus 提供给后端的只读凭据能力，不属于某一个网站后端。

用户通过 Emacs 可定制选项 `camulus-firefox-profile` 指定 Firefox profile。Camulus 启动时创建 provider，并将它的读取函数交给需要浏览器凭据的后端：

```ocaml
let provider = Reader.Credentials.Firefox.create ~profile () in
Douban_backend.create_live ~cookies:(Reader.Credentials.Firefox.get provider) ()
```

后端决定请求哪个 URL；公共 provider 负责读取 Firefox 数据库、匹配域名和路径、过滤过期 Cookie 与非默认容器、处理数据库锁和 WAL 快照。读取发生在请求时，因此 Firefox 中后续登录或 Cookie 更新能在下一次请求中生效。后端不再接受 Firefox profile 路径，也不直接打开浏览器数据库。

配置没有 profile 时，创建 provider 不会报错；只有后端实际请求 Cookie 才提示配置缺失。读取不写回 Firefox，也不把 Cookie 复制到 Camulus 的配置或持久状态中。锁定数据库需要临时快照时，目录和文件分别使用 0700、0600 权限，并在读取后删除。媒体下载仍使用不带 Cookie 的独立请求。

当前知乎和豆瓣均使用这项公共能力，由 Firefox 管理登录和 Cookie 更新；Camulus 不维护它们的独立登录会话。

## 外部 Emacs 包

`camulus-firefox-cookies-get` 接受一个 HTTPS URL，返回字符串键值对 alist，可以直接用作 `zhihu.el` 的 Cookie 回调：

```elisp
(with-eval-after-load 'camulus_native
  (setopt camulus-firefox-profile "~/.config/mozilla/firefox/default/"))
(autoload 'camulus-firefox-cookies-get "camulus" nil nil)
(with-eval-after-load 'zhihu
  (setopt zhihu-cookie-function #'camulus-firefox-cookies-get))
```

该函数由 Ecaml 定义，调用时按需加载原生模块，不启动时间线或同步任务。每次调用读取当前 profile 选项和 Firefox 数据库，不缓存凭据。它遵循外部包的同步回调约定，读取期间会短暂占用 Emacs 主线程；Eio 后端仍使用异步 `get`，不能用 `get_sync` 替代。没有配置或读取失败会向调用方报错。
