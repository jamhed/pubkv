#!/bin/sh
BRANCH=`bin/current-branch`
LOCALNAME=pubkv-$BRANCH
./build-image.sh
./data-volume.sh
docker stop $LOCALNAME
docker rm $LOCALNAME
docker run -td --net priv --name $LOCALNAME --volumes-from pubkv-data registry.jamhed.tk/$LOCALNAME
bin/docker-cleanup.sh
