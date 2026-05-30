FROM debian:bookworm-slim

ENV DEBIAN_FRONTEND=noninteractive
ENV APP_HOME=/workspace

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    git \
    make \
    rlwrap \
    sbcl \
 && rm -rf /var/lib/apt/lists/*

# Install Quicklisp
RUN curl -sS -o /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp \
 && sbcl --no-userinit --non-interactive \
         --disable-debugger \
         --load /tmp/quicklisp.lisp \
         --eval '(quicklisp-quickstart:install :path "/root/quicklisp" :proxy nil)' \
 && rm /tmp/quicklisp.lisp

# Install Quicklisp dependencies
RUN sbcl --no-userinit --non-interactive \
         --eval '(load "/root/quicklisp/setup.lisp")' \
         --eval '(ql:quickload "dexador" :silent t)' \
         --eval '(ql:quickload "yason" :silent t)' \
         --eval '(ql:quickload "ironclad" :silent t)' \
         --eval '(ql:quickload "fiveam" :silent t)' \
         --eval '(quit)'

WORKDIR ${APP_HOME}

COPY claw-lisp.asd claw-lisp-cli.asd ${APP_HOME}/
COPY lisp ${APP_HOME}/lisp
COPY Makefile ${APP_HOME}/Makefile

CMD ["bash"]
