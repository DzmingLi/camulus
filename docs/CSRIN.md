# CS.RIN.RU

`src/backends/csrin/` 是原 nncsrin 的 OCaml 移植，不依赖 Gnus、Emacs URL 或外部 curl。HTTP 在 Eio 工作 domain 中运行，Ecaml 模块提供浏览、登录和撰写交互。

## 浏览与配置

`M-x camulus-csrin` 输入 `forum.14` 或 `topic.12`；留空发现当前身份可见的论坛。`M-x camulus-csrin-forums` 也可直接发现论坛。配置入口示例：

```json
{
  "version": 1,
  "entries": [
    {"name": "CS.RIN.RU", "source": "csrin", "group": "forum.14"}
  ],
  "backend_settings": {
    "csrin": {"site": "https://cs.rin.ru/forum/"}
  }
}
```

正式来源仅允许 `https://cs.rin.ru/forum/` 与 `https://csrin.org/forum/`；同一后端会话使用一个站点和身份。换镜像不改变 topic/post 身份。改变站点配置需要重启运行时。HTTP loopback 仅供本地集成测试。

声明入口不订阅论坛。需要汇入统一首页时显式设置 `timeline: true`，也可以将单个 `topic.ID` 声明为入口。当前没有远端追踪设置接口。

论坛和话题列表来自 Atom 的近期更新范围，不表示整个论坛历史。列表直接按原生 topic 身份合并，不抓取正文；`RET` 才读取第一页确定真正的首楼，再逐页展开完整讨论，同级按来源时间排序。phpBB 没有明确的逐楼回复关系时，后续楼层挂在真实首楼下，不根据引用文本猜测父楼。

打开讨论时渐进交付正文与附件。默认最多 100 页，分页重复、页面不可读或超过限制会报错，保留已显示内容且不标记完成。收藏完整讨论必须全部成功；单楼收藏独立下载这一楼及其附件。图片与 `download/file.php` 附件保存在各楼下，可下载和离线归档。已读、时间线移除和收藏继续使用公共持久状态。

当前不提供组内搜索，`/` 会明确报告不支持。HTML 日期按旧后端的默认 `+0100` 解释，Atom 日期使用自带时区。

## 登录

`M-x camulus-csrin-login` 使用 auth-source：

```text
machine cs.rin.ru port https login YOUR_USERNAME password YOUR_PASSWORD
```

镜像使用对应主机名。缺少凭据时提示用户名和密码，不将密码写入配置。会话 Cookie 只存在工作进程内存中；重启后重新登录。若已配置 `backend_settings.browser.firefox_profile`，也可读取该浏览器中匹配站点的登录 Cookie。

支持站点普通的 securitytoken Cookie 持久性检查，不执行任意网页 JavaScript。CAPTCHA、登录挑战及站点限制需要在浏览器完成；不会绕过它们。带认证信息的重定向必须留在同一来源；外站图片请求不携带论坛 Cookie。

## 撰写和草稿

在组列表 `c` 新建话题、`r` 回复，阅读窗口也支持 `r` 和 `c`。新话题要求 `forum.ID` 入口；回复从打开的帖子获取实际论坛和 topic。编辑 buffer 替换左侧列表窗口，右侧讨论保留。正文支持纯文本或 BBCode，尚不支持上传附件、编辑帖子、私信和跨组发帖。

- 编辑暂停 0.75 秒后自动保存。
- `C-x C-s` 手动保存，`C-c C-k` 保存并关闭。
- `C-c C-c` 明确提交一次。
- 组列表 `D` 或 `M-x camulus-csrin-drafts` 选择并恢复草稿。

草稿保存到 `$XDG_STATE_HOME/camulus/csrin-drafts/`，默认 `~/.local/state/camulus/csrin-drafts/`，文件权限 0600。Forum、Topic、Subject 是草稿上下文，可检查但应避免随意更改。提交前持久记录 `X-Camulus-Delivery: submitting`；若超时、取消或回执不明确，草稿保留，重新打开也会提示先检查网站。成功得到同源的 topic 发布回执才清理草稿。

每次提交先读取新鲜的 posting 表单和 CSRF/session 字段，不复用旧 token。提交 POST 不自动重试；没有联网自动发送队列。普通关闭 buffer 仍使用 Emacs 的保存提示，未完成的发送不会因关闭阅读窗口而重发。

## 验证范围

`make test` 包含协议模拟测试和本地 phpBB HTTP 服务的真实 Ecaml 集成：覆盖来源身份、时区、首楼关系、逐页交付、重复页和页数上限、Cookie 检查、登录、CSRF、二进制附件归档、草稿自动保存、左右窗口布局、成功清理与不确定结果保留。测试不会向真实论坛发帖。

Gnus 导入器现可将 `nncsrin:forum.ID` / `nncsrin:topic.ID` 映射为 Camulus 入口；不导入旧数字文章号、已读区间或正文缓存。旧实现来源说明保存在后端 `NOTICE`。
