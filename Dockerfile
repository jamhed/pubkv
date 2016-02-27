# Run pubkv in a container

FROM alpine:3.3
MAINTAINER Roman Galeev <jamhedd@gmail.com>

ENV HOME /home/user
ENV USERNAME user

RUN adduser -D $USERNAME \
    && apk update \
    && apk add grep git make perl \
      ncurses-libs ncurses-terminfo ncurses-terminfo-base ca-certificates openssl libssh2 \
      erlang erlang-dev erlang-asn1 erlang-public-key erlang-ssl \
      erlang-mnesia erlang-crypto erlang-sasl \
    && cd $HOME \
    && git clone https://github.com/jamhed/pubkv.git \
    && cd pubkv \
    && make \
    && make clean \
    && rm -rf $SRCDIR/pubkv/deps \
    && apk del grep git make perl \
      erlang erlang-dev erlang-asn1 erlang-public-key erlang-ssl \
      erlang-mnesia erlang-crypto erlang-sasl \
    && chown -Rh $USERNAME:$USERNAME $HOME

ENV PATH "$HOME/pubkv/bin:$PATH"
WORKDIR $HOME/pubkv
USER $USERNAME
ENTRYPOINT [ "runner" ]
