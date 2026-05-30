SHELL := /bin/bash

SBCL ?= sbcl
APP_ROOT ?= $(CURDIR)
DOCKER ?= docker
IMAGE ?= claw-lisp-dev
QL_SETUP = --eval '(load "/root/quicklisp/setup.lisp")'
SBCL_BASE = $(SBCL) --no-userinit --non-interactive --eval "(require :asdf)" $(QL_SETUP) --eval "(push \#P\"$(APP_ROOT)/\" asdf:*central-registry*)"

.PHONY: lisp-load
lisp-load:
	$(SBCL_BASE) --eval "(asdf:load-system :claw-lisp)"

.PHONY: lisp-cli
lisp-cli:
	$(SBCL_BASE) --eval "(asdf:load-system :claw-lisp-cli)" --eval "(uiop:quit (claw-lisp.cli:main))"

.PHONY: lisp-test
lisp-test:
	$(SBCL_BASE) --eval "(asdf:load-system :claw-lisp/test)" --eval "(uiop:quit (claw-lisp.tests:run-tests))"

.PHONY: docker-build
docker-build:
	$(DOCKER) build -t $(IMAGE) .

.PHONY: docker-shell
docker-shell:
	$(DOCKER) run --rm -it -v "$(APP_ROOT):/workspace" -w /workspace $(IMAGE) bash

.PHONY: docker-load
docker-load:
	$(DOCKER) run --rm -v "$(APP_ROOT):/workspace" -w /workspace $(IMAGE) make lisp-load

.PHONY: docker-cli
docker-cli:
	$(DOCKER) run --rm -v "$(APP_ROOT):/workspace" -w /workspace $(IMAGE) make lisp-cli

.PHONY: docker-test
docker-test:
	$(DOCKER) run --rm -v "$(APP_ROOT):/workspace" -w /workspace $(IMAGE) make lisp-test
