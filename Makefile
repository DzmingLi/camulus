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
