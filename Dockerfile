# Run pubkv in a container

FROM alpine:3.3
MAINTAINER Andrey Arapov <andrey.arapov@nixaid.com>

RUN apk update \
    && apk add grep git make perl \
               erlang erlang-dev erlang-asn1 erlang-public-key erlang-ssl \
               erlang-mnesia erlang-crypto erlang-sasl

ENV HOME /home/user
ENV USERNAME user
ENV SRCDIR /src
RUN adduser -D $USERNAME

RUN mkdir -p $SRCDIR \
    && cd $SRCDIR \
    && git clone https://github.com/jamhed/pubkv.git \
    && cd $SRCDIR/pubkv \
    && make \
    && make clean \
    && rm -rf $SRCDIR/pubkv/deps \
    && apk del erlang-dev perl make git \
    && chown -Rh $USERNAME:$USERNAME $SRCDIR/

WORKDIR $HOME
USER $USERNAME
ENTRYPOINT [ "/src/pubkv/_rel/pubkv_release/bin/pubkv_release", "console" ]
