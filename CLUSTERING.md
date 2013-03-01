# Macaba clustering

## Database clustering

Please note, that if you don't have CPU limitation or requirement to have
redundant web application nodes, you can keep running Macaba on single node, and
extend RIAK instead. That is very easy and data is self balancing between nodes.

For growing out posts and images database see:
http://docs.basho.com/riak/latest/cookbooks/Basic-Cluster-Setup/

After you configured cluster, all that's left to do is open macaba_erlang_node
config file, and add some more IP addresses for multiple RIAK nodes and restart
Macaba Erlang node.

## Erlang node clustering

### This part of doc is TODO

This hasn't been tested yet! Continue reading at your own risk!

To extend Erlang node on multiple nodes is a bit more complicated. You will
need to do something like this (not tested!):

1. Stop Macaba node
1. Erase old Mnesia schema in database/ (it contains no data so its safe)
1. Configure multiple hosts in macaba.config
1. Ensure all nodes have same config copies (TODO: to organize this better)
1. Start all Macaba nodes
1. Configure your load balancer (or nginx) to balance requests to multiple
   Macaba nodes
