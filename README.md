macaba-server
=============

High-load anonymous message board server made in Erlang, designed with scalability
and extensibility in mind. Meant to be familiar for wakaba/futaba users.

Project status
==============

Planning-prealpha

Using RIAK - a key/value distributed and fault tolerant store, initially works on a single node, but
allows creating multiple-node cluster. Data is self-balanced when you add a node, and if some node
randomly crashes or goes down, data is rebalanced to survive the crash without degrading performance.

For synchronizing thread/post counters across multiple nodes gproc library is used. Normally it runs
in single-node mode with zero configuration required, but for running in cluster, see CLUSTERING.md

Prerequisites
=============

* Linux operating system (may possibly work on MacOSX or Windows but untested)

* Erlang/OTP R15 or R16, get from https://www.erlang-solutions.com/downloads/download-erlang-otp -
requires no configuration

* RIAK database, get from http://docs.basho.com/riak/latest/downloads/ - requires no configuration
just install and ensure its started by doing sudo /etc/init.d/riak restart

Compiling
=========

* Checkout from github by using git clone <url>
* run: make

This will download 'rebar', using rebar it will check out dependencies to 'deps/' subdirectory,
compile dependencies, compile macaba, and run locally on port 8000.
