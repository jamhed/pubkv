#!/bin/sh
docker images | grep none | while read a b c d; do docker rmi -f $c; done
