#!/bin/sh
echo PRE-SETUP
echo -e "Host *\n\tStrictHostKeyChecking no\n" >> /root/.ssh/config
adduser -D $USERNAME
apk update
apk add grep git make perl \
    ncurses-libs ncurses-terminfo ncurses-terminfo-base ca-certificates openssl libssh2 openssh-client \
    erlang erlang-dev erlang-asn1 erlang-public-key erlang-ssl \
    erlang-mnesia erlang-crypto erlang-sasl erlang-tools erlang-syntax-tools \
    libstdc++ \
    gcc musl-dev g++ \
    bash
