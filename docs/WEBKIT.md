# 内联视频

正文中的视频由 `reader_media.ml` 管理。Nix 的 `emacs-camulus` 包自动引入 `emacs-webkit`；图形环境下按需加载 `webkit-module`，终端或模块不可用时显示外部播放链接。直接安装原生模块时，需要将 emacs-webkit 加入 Emacs 的 load-path。

播放器附着在正文的占位字符上，随窗口滚动、尺寸和 frame 变化调整位置。重绘保留仍在使用的播放器和播放进度，移除内容或关闭 buffer 时释放资源。

WebKit 只接收本地生成的视频页面，消息通道只处理视频宽高，用来调整占位尺寸。远端媒体 URL 通过 HTTP(S) 校验；页面消息不会被当作 Lisp 函数执行。

知乎和豆瓣读取配置的 Firefox Cookie，不使用 WebKit 登录。
