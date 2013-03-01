# macaba

Anonymous message board server made in Erlang, designed with high load,
scalability and extensibility in mind. The forum layout concept is designed to
be familiar for wakaba users:

*   site &rarr; boards &rarr; threads &rarr; posts &rarr; images and text
*   posts have monotonically increasing ids, one counter per board
*   name field is optional, supports special hash tags for identifying without
    discovering the user's identity
*   post body uses special markup which is processed to HTML

## Project status

### Pre-Alpha, almost there

Using RIAK - a key/value distributed and fault tolerant store, allows to create
multiple-node cluster. Data in RIAK is self-balanced when you add a node, and
if some node randomly crashes or goes down, data is rebalanced to survive the
crash without degrading performance.

For synchronizing thread/post counters across multiple nodes Mnesia memory
tables are used. Hopefully this will be fast enough to allow for up to hundreds
posts per board per second in a multinode setup.

## Features

### Done right now

*   Versioned data model (reading data always works no matter how old the data
    version was)
*   Distributed monotonically growing post counters using Mnesia memory tables
*   Designed for running as a cluster, or as a single node
*   Basic board functions: board index, thread index, thread view
*   Basic plain HTML scriptless theme (later REST API, Websocket)
*   File upload (1 per post)
*   Full Wakaba markup support

### Tier 1 (important) TODO

*   Deleting posts by user
*   Captcha support, flood detection, basic DDOS resistance
*   Words/regex blacklisting, image hash/similarity? blacklisting
*   User blacklisting & banning
*   Authentication, sessions
*   Users participation - reporting posts and threads
*   Moderation UI (bans, thread management - locking pinning, deleting,
    reviewing, etc)

### Tier 2 (do it later) TODO

*   Admin UI (configuring boards list, global board options, maintenance mode,
    read-only mode)
*   Themes support, porting 1-2 popular themes from Wakaba
*   REST API for custom UIs and user applications
*   Websocket API for custom UIs and live update
*   Modular support for: post parsing, themes, additional site sections, post
    display (color or text codes for same person detection)

### Tier 3 (far future) TODO

*   Multiple file upload
*   Tripcodes support
*   TOR/proxy detection
*   Fighting Unicode quirks (reverse text direction)
*   A torrent-tracker module (tourettes or peasy or something else?)
*   Video embedding support (configurable)
*   Permanent authentication, VIP passwords?

## Prerequisites

*  Linux operating system (Ubuntu works, Debian and other should be easy),
   MacOSX _may_ work, but you're on your own building dependencies!
*  Erlang/OTP R15 or R16, get from
   https://www.erlang-solutions.com/downloads/download-erlang-otp - requires no
   additional configuration.
*  RIAK database, get from http://docs.basho.com/riak/latest/downloads/ -
   requires no configuration just install and ensure its started by doing
   `sudo /etc/init.d/riak restart`
*  Imagemagick for making thumbnails and image analysis, (install
   libmagickwand-dev on Ubuntu)
*  A working C/C++ compiler to build dependencies (install build-essential on
   Ubuntu)

## Compiling

*   Checkout from github by using `git clone git://github.com/kvakvs/macaba.git`
*   $ `make run` will do full recompile and start (or `make runf` to run without
    recompiling all deps)
*   Open http://localhost:12000/ with your browser

This will download 'rebar', check out dependencies to 'deps/' subdirectory,
compile dependencies, compile macaba, and run locally on port 12000
(configurable in macaba.config).
