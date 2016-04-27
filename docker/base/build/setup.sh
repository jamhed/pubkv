#!/bin/sh
cd $HOME 
git clone ssh://git@gogs.jamhed.tk:10022/jamhed/pubkv.git
cd pubkv
make
 
cp -a /root/.ssh $HOME/
chown -Rh $USERNAME:$USERNAME $HOME
