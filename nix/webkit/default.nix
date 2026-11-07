{
  lib,
  emacsPackages,
  fetchFromGitHub,
  pkg-config,
  webkitgtk_4_1,
  glib,
  gtk3,
  gobject-introspection,
  mainThreadApiPatch ? ./main-thread-api.patch,
}:

## akirakyle/emacs-webkit: dynamic module bringing WebKitGTK as a real
## browser widget inside an Emacs window — works on pgtk Wayland where
## the built-in xwidget-webkit historically didn't.
##
## Consumer pattern (NixOS / home-manager):
##   programs.emacs.package = (pkgs.emacsPackagesFor pkgs.emacs-pgtk).emacsWithPackages
##     (epkgs: [ ... pkgs.nur.repos.dzmingli.emacs-webkit ... ]);
## On non-default emacs builds (e.g. emacs-pgtk), override trivialBuild:
##   pkgs.nur.repos.dzmingli.emacs-webkit.override {
##     emacsPackages = pkgs.emacsPackagesFor pkgs.emacs-pgtk;
##   }
emacsPackages.trivialBuild {
  pname = "emacs-webkit";
  version = "unstable-2026-05-11";

  src = fetchFromGitHub {
    owner = "akirakyle";
    repo = "emacs-webkit";
    rev = "4c5caa8e2c2baa09400d3c4a467d4799d735d388";
    hash = "sha256-bHrfc9bGKY57+KGDRH5CdRflWH5va4jzGkMzXRrapg4=";
  };

  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ webkitgtk_4_1 glib gtk3 gobject-introspection ];
  patches = [ ./main-thread-finalizer.patch mainThreadApiPatch ];

  ## Upstream Makefile hardcodes `LIBS = gtk+-3.0 webkit2gtk-4.0`, but
  ## nixpkgs only ships webkit2gtk-4.1 now (4.0 dropped). API differences
  ## are minimal (mostly libsoup2 → libsoup3 ABI), so just rewrite the pkg name.
  postPatch = ''
    substituteInPlace Makefile \
      --replace-fail 'webkit2gtk-4.0' 'webkit2gtk-4.1'

    ## 删掉开发者 scratch 和需要外部依赖的可选模块，避免 emacsWithPackages
    ## 的 native-compile pass 把它们也编一遍：
    ##   tests.el                   —— 顶层引用 ~/git/... 撞 /homeless-shelter
    ##   webkit-ace.el              —— 依赖 ace-window
    ##   evil-collection-webkit.el  —— 依赖 evil-collection
    rm -f tests.el webkit-ace.el evil-collection-webkit.el

    ## The init directory may be an immutable Nix store path. Put cookies and
    ## other WebKit state under the user's XDG state directory instead.
    substituteInPlace webkit.el \
      --replace-fail '(locate-user-emacs-file "webkit/")' \
                     '(expand-file-name "emacs/webkit/" (or (getenv "XDG_STATE_HOME") (expand-file-name "~/.local/state")))'

    ## The native-compile pass runs with an unwritable HOME.  Create the state
    ## directory only when WebKit is loaded interactively.
    substituteInPlace webkit.el \
      --replace-fail '(make-directory webkit--user-dir t)' \
                     '(unless noninteractive (make-directory webkit--user-dir t))'

    ## The preview hook may run while another Wayland surface owns GTK
    ## focus.  Pass the selected PGTK frame pointer to the native module so
    ## embedding does not depend on global GTK focus at that instant.
    substituteInPlace webkit.el \
      --replace-fail '                           webkit-own-window))' \
                     "                           webkit-own-window
                           (and (eq (window-system) 'pgtk)
                                (string-to-number (frame-parameter nil 'window-id)))))"

    substituteInPlace webkit-module.c \
      --replace-fail 'GtkFixed *fixed = find_focused_fixed_widget ();' \
                     'GtkFixed *fixed = NULL;
      if (n > 2 && env->is_not_nil (env, args[2]))
        {
          intmax_t window_id = env->extract_integer (env, args[2]);
          for (GList *l = gtk_window_list_toplevels (); l != NULL; l = l->next)
            if (l->data == (void *)window_id)
              {
                fixed = find_fixed_widget (gtk_container_get_children
                                           (GTK_CONTAINER (l->data)));
                break;
              }
        }
      else
        fixed = find_focused_fixed_widget ();'
    substituteInPlace webkit-module.c \
      --replace-fail 'mkfn (env, 2, 2, webkit_new, "webkit--new", "", NULL);' \
                     'mkfn (env, 2, 3, webkit_new, "webkit--new", "", NULL);'

    ## webkit.el 顶层还调 (org-link-set-parameters ...) 注册 org link 类型，
    ## 但 `ol` 包不在 native-compile 上下文里 → void-function。
    ## with-eval-after-load 'ol 让它推迟到 org-link 加载后再注册。
    substituteInPlace webkit.el \
      --replace-fail "(org-link-set-parameters \"webkit\" :store #'webkit-org-store-link)" \
                     "(with-eval-after-load 'ol (org-link-set-parameters \"webkit\" :store #'webkit-org-store-link))"

    ## Reduce the WebKit resource cache.  This is not a memory limit and does
    ## not repair native reference/descriptor leaks.  The terminate function
    ## below is available to callers for explicit process recycling.
    substituteInPlace webkit-module.c \
      --replace-fail 'WebKitWebContext *context = webkit_web_context_new ();' \
                     'WebKitWebContext *context = webkit_web_context_new ();
  webkit_web_context_set_cache_model (context, WEBKIT_CACHE_MODEL_DOCUMENT_VIEWER);'

    ## A2a. 新增 webkit_terminate_web_process（仿 webkit_reload）：杀掉 webview
    ## 的 web process，下次 load 自动重起，透明释放累积内存、webview 不死。
    substituteInPlace webkit-module.c \
      --replace-fail 'webkit_reload (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)' \
                     'webkit_terminate_web_process (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  if (c != NULL)
    webkit_web_view_terminate_web_process (c->view);
  return Qnil;
}

static emacs_value
webkit_reload (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)'

    ## A2b. 注册到 elisp 名 webkit--terminate-web-process。
    substituteInPlace webkit-module.c \
      --replace-fail '  mkfn (env, 1, 1, webkit_reload, "webkit--reload", "", NULL);' \
                     '  mkfn (env, 1, 1, webkit_reload, "webkit--reload", "", NULL);
  mkfn (env, 1, 1, webkit_terminate_web_process, "webkit--terminate-web-process", "", NULL);'
  '';

  ## trivialBuild 默认 buildPhase 是 batch-byte-compile *.el，会跑两次（先它，
  ## 后 make 我们要的）。覆盖：先 make 出 webkit-module.so，再 byte-compile。
  buildPhase = ''
    runHook preBuild
    make
    emacs -L . --batch \
      --eval "(setq byte-compile-error-on-warn nil)" \
      -f batch-byte-compile *.el
    runHook postBuild
  '';

  ## 覆盖 installPhase：除了 .el / .elc，还要把 webkit-module.so 和 hints.{js,css}
  ## 装到 $LISPDIR。必须在这里装（不能 postInstall），因为 generic.nix 的 postInstall
  ## 提前 prepend 了 native-compile pass，那时 webkit.el (require 'webkit-module)
  ## 需要 .so 已经在 load-path 上。
  installPhase = ''
    runHook preInstall

    LISPDIR=$out/share/emacs/site-lisp
    install -d $LISPDIR
    install -m644 *.el *.elc       $LISPDIR
    install -m755 webkit-module.so $LISPDIR
    install -m644 hints.js hints.css $LISPDIR

    runHook postInstall
  '';

  meta = with lib; {
    description = "Emacs dynamic module wrapping WebKitGTK as a real browser widget";
    homepage = "https://github.com/akirakyle/emacs-webkit";
    license = licenses.gpl3Plus;
    platforms = platforms.linux;
  };
}
