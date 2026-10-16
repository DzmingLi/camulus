EMACS ?= emacs
BUILD_DIR ?= _build
MODULE_DIR = $(abspath $(BUILD_DIR)/default/src)
BUILD_ABS = $(abspath $(BUILD_DIR))

.PHONY: build test demo
build:
	dune build --build-dir $(BUILD_ABS) src/camulus_native.so

test: export XDG_CONFIG_HOME = $(BUILD_ABS)/test-config
test: export XDG_STATE_HOME = $(BUILD_ABS)/test-state
test: build csrin-test
	python3 test/gnus-import-test.py
	dune runtest --build-dir $(BUILD_ABS)
	timeout 60s $(EMACS) -Q --batch --module-assertions -L $(MODULE_DIR) -L lisp -l test/integration.el
	timeout 10s $(EMACS) -Q --batch --module-assertions -L $(MODULE_DIR) -L lisp -l test/exit.el
	timeout 30s $(EMACS) -Q --batch --module-assertions -L $(MODULE_DIR) -L lisp -l test/reddit-integration.el
	timeout 30s $(EMACS) -Q --batch --module-assertions -L $(MODULE_DIR) -L lisp -l test/entries-integration.el
	timeout 30s $(EMACS) -Q --batch --module-assertions -L $(MODULE_DIR) -L lisp -l test/reader-presentation.el
	python3 test/setup-mail-fixture.py $(BUILD_ABS)/mail-fixture
	MAIL_FIXTURE=$(BUILD_ABS)/mail-fixture timeout 90s python3 test/smtp-fixture.py $(BUILD_ABS)/mail-fixture $(EMACS) -Q --batch --module-assertions -L $(MODULE_DIR) -L lisp -L test -l test/mail-integration.el
	MAIL_FIXTURE=$(BUILD_ABS)/mail-fixture timeout 90s python3 test/smtp-fixture.py $(BUILD_ABS)/mail-fixture $(EMACS) -Q --batch --module-assertions -L $(MODULE_DIR) -L lisp -L test -l test/mail-workflow.el
	python3 test/setup-mail-fixture.py $(BUILD_ABS)/state-mail-fixture
	MAIL_FIXTURE=$(BUILD_ABS)/state-mail-fixture timeout 90s $(EMACS) -Q --batch --module-assertions -L $(MODULE_DIR) -L lisp -l test/content-state-integration.el

	timeout 60s python3 test/rss-fixture.py $(EMACS) -Q --batch --module-assertions -L $(MODULE_DIR) -L lisp -l test/rss-integration.el

	timeout 60s python3 test/rss-fixture.py $(EMACS) -Q --batch --module-assertions -L $(MODULE_DIR) -L lisp -l test/jsonfeed-integration.el

	timeout 60s python3 test/discourse-fixture.py $(EMACS) -Q --batch --module-assertions -L $(MODULE_DIR) -L lisp -l test/discourse-integration.el

.PHONY: reader-demo
reader-demo: build
	$(EMACS) -Q -L $(MODULE_DIR) -L lisp -l camulus -f camulus

# Live checks are opt-in and read-only.
.PHONY: reddit-live mail-live
reddit-live: build
	timeout 180s $(EMACS) -Q --batch --module-assertions -L $(MODULE_DIR) -L lisp -l test/reddit-live.el

mail-live: build
	timeout 180s $(EMACS) -Q --batch --module-assertions -L $(MODULE_DIR) -L lisp -L test -l test/mail-live.el

.PHONY: csrin-test
csrin-test: build
	timeout 60s python3 test/csrin-fixture.py $(EMACS) -Q --batch --module-assertions -L $(MODULE_DIR) -L lisp -l test/csrin-integration.el
