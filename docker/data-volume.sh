#!/bin/sh
BRANCH=`bin/current-branch`
LOCALNAME=pubkv-$BRANCH
docker create -v /home/user/pubkv/data --name pubkv-data registry.jamhed.tk/$LOCALNAME /bin/true
