#!/bin/sh
su -m "$USERNAME" -c "cd $HOME/pubkv && git pull && make"
