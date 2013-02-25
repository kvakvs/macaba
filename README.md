# macaba-server

Anonymous message board server made in Erlang, designed with high load,
scalability and extensibility in mind. The database storage concept is meant to
be familiar for wakaba/futaba users:

* site contains boards, boards contain threads, threads contain posts;

* posts use an unique monotonically increasing counter, one per board);

* posts contain optional image attachment (or several attachments)

* name field is optional, supports special hash tags for identifying without
discovering the user's identity;

* post body has a special markup which is postprocessed to HTML markup.

Currently only HTTP HTML interface is supported. Websocket and REST API is in
plans for beta.

## Project status

### Pre-Alpha, almost there

Using RIAK - a key/value distributed and fault tolerant store, allows to create
multiple-node cluster. Data in RIAK is self-balanced when you add a node, and
if some node randomly crashes or goes down, data is rebalanced to survive the
crash without degrading performance.

For synchronizing thread/post counters across multiple nodes Mnesia memory
tables are used. Hopefully this will be fast enough to allow for up to hundreds
posts per board per second in a multinode setup.

## TODO

### Tier 1 (important) TODO

*   Full Wakaba markup support
*   Words/regex blacklisting
*   Captcha support
*   User blacklisting, flood detection, basic DDOS resistance
*   Authentication, permanent authentication, VIP passwords?
*   Moderation UI (bans, reporting for rules violation, thread management -
    locking pinning, deleting, reviewing, etc)
*   Administration UI (boards list, bringing board offline)

### Tier 2 (optional) TODO

*   Themes support, porting 1-2 popular themes from Wakaba
*   REST API for custom UIs and user applications
*   Websocket API for custom UIs and live update

### Tier 3 (even more optional) TODO

*   Tripcodes support
*   TOR/proxy detection
*   Fighting Unicode quirks (reverse text direction)
*   A torrent-tracker module
*   Video embedding support (configurable)

## Prerequisites

* Linux operating system (may possibly work on MacOSX or Windows but untested)

* Erlang/OTP R15 or R16, get from
https://www.erlang-solutions.com/downloads/download-erlang-otp - requires no
additional configuration.

* RIAK database, get from http://docs.basho.com/riak/latest/downloads/ -
requires no configuration just install and ensure its started by doing
`sudo /etc/init.d/riak restart`

## Compiling

* Checkout from github by using `git clone git://github.com/kvakvs/macaba.git`

* $ `make run` full recompile and start (or `make runf` to run without
recompiling all deps)

This will download 'rebar', using rebar it will check out dependencies to
'deps/' subdirectory, compile dependencies, compile macaba, and run locally
on port 8000.
