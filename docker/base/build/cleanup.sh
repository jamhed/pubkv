#!/bin/sh
cd $HOME/pubkv
make clean
rm -rf deps
apk del grep git make perl \
      erlang erlang-dev erlang-asn1 erlang-public-key erlang-ssl \
      erlang-mnesia erlang-crypto erlang-sasl
rm -rf /tmp/*
