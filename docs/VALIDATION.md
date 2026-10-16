# 验证记录

本页记录第一阶段运行时验证。后续真实 Reddit 后端及端到端联调见 [REDDIT.md](REDDIT.md)。

2026-10-15，在当前 Linux 主机的锁定开发环境中执行 `make test`，退出码 **0**。当前复现命令为 `nix develop -c make test`。

环境：Emacs 31.1、OCaml 5.3.0、Ecaml 0.17.0、Eio 1.6。测试启动全新 `emacs -Q --batch --module-assertions`，没有加载用户配置，也没有连接正在使用的 Emacs daemon。

## 结果

| 检查 | 结果 |
| --- | --- |
| 独立 OCaml 测试 | 通过：100,000 次跨 domain FIFO、容量限制、取消、任务隔离、32 个活动任务上限、GC、信号保护、积压时退出 |
| 原生模块实际加载 | 通过，Emacs 输出 `Loaded Ecaml.` |
| 9 项 ERT 集成测试 | 全部通过，0 unexpected，约 1.86 秒 |
| 带活动后台任务退出 Emacs | 通过，退出 hook 回收 worker，进程退出码 0 |

ERT 覆盖：

1. 250ms 后台任务提交立即返回，Emacs 定时器继续运行，native pipe 实际触发 filter，worker domain ID 非 0。
2. 显式取消及重复取消。
3. 关闭视图后丢弃其结果，服务任务继续完成。
4. 已排队的旧视图结果也被过滤。
5. 一个任务抛出异常，其他任务仍正常完成。
6. 后台分配及 OCaml GC 与前台 Ecaml 值回收、Emacs GC 同时进行；Emacs 子进程正常返回指定退出码。
7. 暂停消费结果后停止运行时，退出不依赖队列腾出空间。
8. 连续 5 次启动/停止后，文件描述符数量未超出断言容差。
9. 删除通知 process 后，工作 domain 未因断管信号退出，定时补偿仍能获取结果。

信号断言比较的是 **Ecaml 已加载、Eio worker 尚未启动** 时的 SIGPIPE/SIGCHLD 配置与 worker 运行/停止后的配置。它不声称验证了 Ecaml 初始化前后的全部信号变化。

原始最终测试输出见 test-results.txt（本地验证输出）。

## 发现并解决的问题

- Dune 外部构建目录需传绝对路径，Makefile 已规范化。
- Ecaml 0.17 的 obarray 类型检查不兼容当前 Emacs 31，已添加 `obarrayp` 补丁。
- 通知管道提前关闭可能产生 SIGPIPE，已加入线程级保护及断管测试。
- 等待发布终态结果的 fiber 也占用活动任务名额，避免积压时绕过任务上限。

## 依赖来源与结论边界

- Ecaml 源码：`v0.17.0`，检视版本 `64fde0c0195e9f9ab02947f39776ded5b02c6ba2`。
- Eio 源码：`v1.6`，检视版本 `1fc0efa41ccfb3818b09f54feec90ec29b47f1b6`。
- 本机 Nixpkgs 源：`/nix/store/2010s04mqpq7q1kch0ddc9b429s2vh9v-source`；NAR hash：`sha256:19y4py973w62z4qb7gzn0chjs67ihxy8rbjb7g71iyk8yz7z5k8z`。
- 补丁和实现均保存在本项目中；未修改系统 Nix 配置或用户 Emacs 配置。

本次证明了这组版本下的进程内通信、调度及清理路径可以运行。尚未验证真实 HTTP/TLS、任意第三方 C 库、GUI/WebKit、长期压力、多平台或多个独立 OCaml 原生模块共存；也没有实现持久同步队列或真实阅读器界面。生产使用仍需要进一步审计依赖补丁及宿主资源边界。
