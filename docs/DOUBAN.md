# 豆瓣

阅读在线广播、帖子或某一楼评论时，顶部提供「赞 · l」「取消赞 · L」按钮，也可按 `l` / `L`。操作针对光标所在楼层；不能给未加载、不可用的楼层或离线归档直接点赞。

后端实现 `Backend.Liking`。每次操作立即发请求，收到确认才更新 ♥ 标记；失败直接报错，不加入离线队列、不自动重试。同一楼的请求未结束前不能重复提交。超时无法证明服务器未执行，界面提示刷新确认远端状态。较早发起的读取结果不会覆盖刚确认的操作；后续刷新仍以远端为准。

登录沿用公共 Firefox Cookie provider。只把匹配目标 URL 的 Cookie 发给豆瓣固定 HTTPS 地址。浏览器数据库中没有 `ck` 时，使用相同账号请求 `/mine/`，取出返回的 CSRF token，仅用于本次请求，不写回 Firefox、不保存独立会话。写请求不跟随重定向。

接口依据豆瓣网页当前使用的代码（2026-11-07）核对：

- [广播页面脚本](https://asset.doubanio.com/cuphead/sns-static/status/656.c.70cdb.a.js)：`POST /j/status/like`、`POST /j/status/unlike`，传 `sid` 和 CSRF header。
- [帖子 reaction 按钮](https://asset.doubanio.com/cuphead/vendors/js/actions/react_btn.7073482b761e.js)：`POST /rexxar/api/v2/group/topic/ID/react`，显式传 `reaction_type=1` 或 `0`。
- [评论脚本](https://asset.doubanio.com/cuphead/sns-static/comments/747.c.0a047.a.js)与页面 `_COMMENTS_CONFIG`：广播评论使用 `status/comment/ID/react`；帖子评论使用 `group/topic/TOPIC/comment/COMMENT/react`。

帖子正文的富文本段落依赖网页 CSS 保留原始换行。后端把段落文本节点中的换行转成 `<br>`，保留空行、嵌套行内格式与预格式化代码，不把 HTML 源码缩进变成额外空段落。

回归测试使用模拟 HTTP、临时 Cookie 数据和合成正文，覆盖各类目标、取消赞、CSRF、拒绝/重定向/超时、过期加载结果、按钮及段落。没有对真实账号内容执行测试点赞。
