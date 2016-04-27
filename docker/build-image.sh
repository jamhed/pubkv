#!/bin/sh
BRANCH=`bin/current-branch`
COMMIT=`bin/last-commit`
cp ~/.ssh/id_rsa* base/
bin/last-commit > base/version
docker build --build-arg BRANCH=$BRANCH -f base/Dockerfile.$BRANCH $1 -t registry.jamhed.tk/pubkv-$BRANCH base/
rm -f base/id_rsa*
