EMACS ?= emacs
BUILD_DIR ?= _build
MODULE_DIR = $(abspath $(BUILD_DIR)/default/src)
BUILD_ABS = $(abspath $(BUILD_DIR))

.PHONY: build
build:
	dune build --build-dir $(BUILD_ABS) src/camulus_native.so

.PHONY: reader-demo
reader-demo: build
	$(EMACS) -Q -L $(MODULE_DIR) -L lisp -l camulus -f camulus

# Focused regression checks for the GitHub protocol and Ecaml/Forge boundary.
.PHONY: test
test: build
	dune runtest --build-dir $(BUILD_ABS)
	XDG_STATE_HOME=$(BUILD_ABS)/github-test-state timeout 30s $(EMACS) -Q --batch --module-assertions -L $(MODULE_DIR) -L lisp -l test/github-integration.el
