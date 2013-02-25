#!/bin/sh

sudo /etc/init.d/riak stop && \
    sudo rm -rf /var/lib/riak/bitcask && \
    sudo /etc/init.d/riak start