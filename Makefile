SHELL := /bin/bash

SBCL ?= sbcl
APP_ROOT ?= $(CURDIR)
DOCKER ?= docker
IMAGE ?= claw-lisp-dev
SBCL_CACHE_VOLUME ?= claw-lisp-sbcl-cache
QL_SETUP = --eval '(load "/root/quicklisp/setup.lisp")'
SBCL_BASE = $(SBCL) --noinform --no-userinit --non-interactive \
	--eval "(declaim (sb-ext:muffle-conditions style-warning sb-ext:compiler-note))" \
	--eval "(require :asdf)" $(QL_SETUP) \
	--eval "(push \#P\"$(APP_ROOT)/\" asdf:*central-registry*)"
DOCKER_RUN = $(DOCKER) run --rm -v "$(APP_ROOT):/workspace" \
	-v "$(SBCL_CACHE_VOLUME):/root/.cache/common-lisp" -w /workspace $(IMAGE)

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
	$(DOCKER_RUN) -it bash

.PHONY: docker-load
docker-load:
	$(DOCKER_RUN) make lisp-load

.PHONY: docker-cli
docker-cli:
	$(DOCKER_RUN) make lisp-cli

.PHONY: docker-test
docker-test:
	$(DOCKER_RUN) make lisp-test
