macaba-server
=============

High-load anonymous message board server made in Erlang, designed with scalability
and extensibility in mind. Meant to be familiar for wakaba/futaba users.

Project status
==============

Planning-prealpha

Prerequisites
=============

* Linux operating system (may possibly work on MacOSX or Windows but untested)
* Erlang/OTP R15 or R16, get from https://www.erlang-solutions.com/downloads/download-erlang-otp - requires no configuration
* RIAK database, get from http://docs.basho.com/riak/latest/downloads/ - requires no configuration just install and ensure its started by doing sudo /etc/init.d/riak restart

Compiling
=========

* Checkout from github by using git clone <url>
* run: make

This will download 'rebar', using rebar it will check out dependencies to 'deps/' subdirectory,
compile dependencies, compile macaba, and run locally on port 8000.
