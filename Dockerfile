# Run pubkv in a container

FROM alpine:3.3
MAINTAINER Roman Galeev <jamhedd@gmail.com>

ENV HOME /home/user
ENV USERNAME user
ENV SRCDIR /src

RUN adduser -D $USERNAME \
    && apk update \
    && apk add grep git make perl \
      ncurses-libs ncurses-terminfo ncurses-terminfo-base ca-certificates openssl libssh2 \
      erlang erlang-dev erlang-asn1 erlang-public-key erlang-ssl \
      erlang-mnesia erlang-crypto erlang-sasl \
    && mkdir -p $SRCDIR \
    && cd $SRCDIR \
    && git clone https://github.com/jamhed/pubkv.git \
    && cd $SRCDIR/pubkv \
    && make \
    && make clean \
    && rm -rf $SRCDIR/pubkv/deps \
    && apk del grep git make perl \
      erlang erlang-dev erlang-asn1 erlang-public-key erlang-ssl \
      erlang-mnesia erlang-crypto erlang-sasl \
    && chown -Rh $USERNAME:$USERNAME $SRCDIR/

WORKDIR $HOME
USER $USERNAME
ENTRYPOINT [ "/src/pubkv/runner" ]
