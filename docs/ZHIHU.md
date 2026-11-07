# 知乎

知乎后端使用 Camulus 的[公共 Firefox Cookie 能力](FIREFOX_COOKIES.md)，与 `zhihu.el` 共享配置的 Firefox profile。

先在该 profile 的默认容器中登录知乎，再配置：

```elisp
(with-eval-after-load 'camulus_native
  (setopt camulus-firefox-profile "~/.config/mozilla/firefox/default/"))
```

更改 profile 路径后重启 Camulus 运行时。同一 profile 中的 Cookie 更新无需重启：每次远端请求重新读取有效 Cookie。Camulus 只读 Firefox 数据库，不写回 Cookie，也不维护独立登录会话。原生扫码、密码、短信登录入口和会话文件选项已移除；旧版保存的会话文件不再读取。

注册的组支持 `answers.USER`、`articles.people.USER` 和 `articles.org.USER`。注册与订阅仍独立；收藏继续持久保存正文和图片。API 请求由 OCaml 签名，图片下载不携带 Cookie。

缺少 `d_c0`、登录过期或遇到服务端验证时会报告错误。请在配置的 Firefox profile 中登录或完成验证，然后重新刷新，不会将认证失败当成空列表。
