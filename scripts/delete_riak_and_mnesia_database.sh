#!/bin/sh

sudo /etc/init.d/riak stop && \
    sudo rm -rf /var/lib/riak/bitcask && \
    sudo /bin/bash -c "ulimit -n 4096; /etc/init.d/riak start" && \
    rm -f ../database/*
