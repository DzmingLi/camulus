# GitHub 与 Forge

GitHub 后端位于 `src/backends/github/`，支持 Issues、Pull Requests、Discussions、会话通知、回复和 PR review。协议请求使用公共 OCaml HTTP 传输；Forge 提供本地仓库关联与仓库操作，不承担 Camulus 的后台 HTTP 任务。

## 入口与订阅

浏览组是 `owner/repo/issues`、`owner/repo/pulls` 或 `owner/repo/discussions`，来源固定为 `github`。例如：

```elisp
(setopt camulus-groups
        '(((name . "Camulus Issues") (source . "github")
           (group . "DzmingLi/camulus/issues"))
          ((name . "Camulus PRs") (source . "github")
           (group . "DzmingLi/camulus/pulls"))))
(setopt camulus-github-notifications t)
```

注册仓库浏览入口不意味着关注整个仓库。首页可汇入 GitHub 自身的会话通知，过滤掉非话题通知；具体话题的 `s` 调用 GitHub 的 `SUBSCRIBED`、`UNSUBSCRIBED`、`IGNORED` 设置。`camulus-github-notifications` 只选择首页是否展示通知，不修改远端订阅。订阅优先级沿用 `(source="github", group="notifications")`。

`M-x camulus-github` 从已注册组中选择。`M-x camulus-github-browse` 可临时输入浏览组。和其他来源一样，进入时刷新，`g` 手动刷新，无定时更新。通知请求遵守 GitHub 的 `X-Poll-Interval`，使用条件请求；断网错误由公共界面报告，已缓存列表保留。

通知条目使用 GitHub 原生话题 node ID，与组浏览、正文、收藏共用身份。同一话题合并展示。文章时间始终来自 `createdAt`；通知的 ID 和 `updated_at` 组成 `activity`，使已移除话题在新通知到来时重新出现。通知组的 `d` 调用 GitHub 的 Done 接口；其他仓库组不提供删除远端话题的操作。

## 认证

默认通过 `ghub--username` 和 `ghub--token` 读取 **Forge 已使用的 token**，包括 auth-source 的延迟 secret。使用 `forge` package 身份，查找通常形如 `USER^forge` 的 auth-source 登录项，不另存 token。缺少凭据不会自动创建 token，也不会影响其他后端。

安装 Forge，并按其说明配置 GitHub 用户和 token。若需要使用独立 auth-source 条目，可以 `setopt camulus-github-auth-user "LOGIN"`；这时后端读取 `api.github.com` 上该登录项的 secret。凭据只在 Emacs 主线程解析，再以普通 OCaml 配置交给 worker。更换凭据后重新启动 Camulus worker。

## 本地仓库关联

安装 Forge 后，在本地 Git 仓库、Magit 或 Forge buffer 中执行：

```text
M-x camulus-github-repository
```

选择 Issues、PRs 或 Discussions，直接进入该仓库的 Camulus 列表。仓库身份遵循 Forge 选择的 remote，包括 Forge 的远端选择配置。已关联的 worktree 也成为该列表的 `default-directory`。

GitHub 列表和正文中：

| 按键 | 行为 |
| --- | --- |
| `F` | 在 Forge 中打开当前话题 |
| `M` | 打开该话题所属仓库的本地 Magit status |
| `r` | 回复当前内容（正文界面） |
| `v` | 打开 PR review |
| `D` | 恢复本地草稿（列表界面） |

统一首页的正文也提供这些 GitHub 操作；首页列表可通过 `M-x camulus-github-forge-topic` 或 `M-x camulus-github-magit` 操作当前条目。

反向导航以 Forge 数据库的仓库身份及 `worktree` 为准，不维护第二份路径映射。Forge 会验证记录目录的 remote，失效或没有关联时明确提示先访问本地仓库。查找关联不会自动 clone、checkout、登记或 fetch。`F` 是显式打开话题的操作：Forge 优先使用已缓存话题，缺失时按 Forge 自身规则获取该话题。

## 写入与 review

回复 buffer 替换列表窗口，保留右侧讨论。使用公共 `Writing` 生命周期自动保存，`C-c C-c` 发送，`C-c C-k` 保存并关闭。草稿位于 `camulus-github-drafts-directory`，支持重新打开。

PR review 展示 REST 返回的文件 patch 与 GraphQL review discussions：`c` 保存当前行或选中范围的评论，`x` 移除草稿评论，`r` 回复已有 review discussion，`t` 切换 resolved，`C-c C-c` 编写总体评价并选择 COMMENT、APPROVE 或 REQUEST_CHANGES。评论随总体 review 一次提交。提交前重新核对 head commit，拒绝把旧 diff 评论提交到新版本。

在 diff 行上按 `o`，通过 Magit 打开本地对象库中的确切 base/head commit 和行号，使用只读 blob，不修改 checkout；缺少对应 commit 时需先使用 Magit 获取对象。不会用当前工作区文件冒充旧版本代码。

所有写入立即提交，不自动重试。失败保留草稿；超时后再次提交前应检查远端结果。真实账户检查只做读取，写入协议由模拟传输测试覆盖。

## 模块职责与验证

- `github_backend.ml`：GitHub 身份、GraphQL/REST、分页、回复树、通知、追踪、回复和 review。
- `github_emacs.ml`：可选能力注册、worker 任务、配置、编辑和 review 界面。
- `forge.ml`：Emacs 主线程上的 Forge/Magit 导航，不参与协议或另建关联数据库。

`test/github_test.ml` 使用注入传输验证协议与失败路径；`test/github-integration.el` 验证 Ecaml 命令边界；`test/github-forge.el` 使用真实 Forge SQLite 数据库和临时 Git 仓库验证双向关联与缓存话题，包含在 Nix `checks.x86_64-linux.github`。`test/github_live.ml` 为可选只读检查，token 从标准输入传入且不输出。
