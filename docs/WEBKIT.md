# 公共 WebKit 能力

Camulus 的 `Reader_webkit` 模块向后端提供浏览器视图能力。Nix 的 `emacs-camulus` 包自动引入 `emacs-webkit`；模块按需加载，终端中仍可使用其他阅读功能。直接安装原生模块时，需要将 emacs-webkit 加入 Emacs 的 load-path。

公共接口位于 `src/frontends/emacs/reader_webkit.mli`：

- `available ()`：检查当前图形环境并按需加载 WebKit 模块。内联视频也使用这个入口。
- `open_page`：打开专用页面，指定标题、URL、页面加载后执行的 JavaScript、固定消息通道，以及收到消息、关闭和错误的回调。
- `close`：关闭视图、释放浏览器，并在适用时返回原来的 buffer；重复关闭不会重复触发回调。

这些函数在 Emacs 所在线程调用。后端在 worker 中执行自己的网络协议，通过已有事件机制请求界面打开页面；公共 WebKit 模块不包含服务名、登录接口、Cookie 规则或验证码协议。

每个 `open_page` 视图拥有独立的 WebKit context，不加载日常浏览器 Cookie，也不将 Cookie 写入持久文件。消息桥不把页面内容插入 buffer、不记录票据，也不将网页传来的名字作为 Lisp 函数执行。只接受调用方声明的消息通道，并限制消息大小和顶层页面来源；调用方仍需校验消息内容。

知乎使用 Firefox Cookie，不再通过 WebKit 执行登录或人机验证。公共 WebKit 能力仍供阅读器和其他后端使用。

测试覆盖消息分片、跨站消息拒绝、大小限制、未知消息名、私密消息不进入 buffer，以及关闭后的回调和资源清理。实际页面交互需要图形环境。
