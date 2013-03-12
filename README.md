# macaba

Anonymous message board server made in Erlang, designed with high load,
scalability and extensibility in mind. The forum layout concept is designed to
be familiar for wakaba users:

*   site &rarr; boards &rarr; threads &rarr; posts &rarr; images and text
*   posts have monotonically increasing ids, one counter per board
*   name field is optional, supports special hash tags for identifying without
    discovering the user's identity
*   post body uses special markup which is processed to HTML

## Documentation

REST API: http://docs.macaba.apiary.io/

## Project status

### Alpha, you can try it

Using RIAK - a distributed and fault tolerant key/value store, allows to create
a single-node or a multiple-node cluster. Data in RIAK is self-balanced when
you add a node, and if some node randomly crashes or goes down, data is
rebalanced automatically to survive the crash without degrading performance.

For synchronizing thread/post counters across multiple nodes Mnesia memory
tables are used. Hopefully this will be fast enough to allow for up to hundreds
posts per board per second in a multinode setup. NOTE: By some people Mnesia
can be seen as a SPOF (a single point of failure). It is totally possible to
rewrite post count generation entierly with RIAK and/or existing Erlang tools.

## Features

### Done

*   Designed for running as a single node or as multiple nodes
*   Basic board functions: board index, thread index, thread view, new thread,
    reply
*   Basic plain HTML scriptless theme (later REST API, Websocket)
*   File upload (1 per post)
*   WakabaMark (no ^H or spoiler support as of yet) optional Markdown support
*   Board limits, delete when thread is sinking
*   Deleting posts by user, optional deletion of attachments only
*   Simple plugins system. Extension modules and hooks are work-in-progress
*   Board post/reply/upload tests (not perfect, but good to have!), WakabaMark
    markup tests
*   Preview function for markup in HTML mode (ajax inside inline div)
*   REST API design in progress http://docs.macaba.apiary.io/
*   poster_id, a base62 encoded hash of ip/user-agent
*   Admin UI
    *   Login form and login handler located at /admin
    *   Sessions support

### In progress

*   Admin UI
    *   Mod: Thread management - locking, pinning, deleting, moving
    *   Admin: Configuring boards list and board options
    *   Admin: Site options, maintenance mode, read-only mode

### Tier 1 TODO: important

*   Captcha support, flood detection, basic DDOS resistance
*   Different backends for storing attachments (static file system, S3)
*   Import quickstart scripts for Wakaba/Kusaba

### Tier 2 TODO: do it later

*   Users participation - reporting posts and threads
*   Admin UI
    *   Mod: Reviewing reported posts
    *   Mod: Words/regex blacklisting, image hash/similarity? blacklisting
    *   Mod: User blacklisting, bans, ban page with ban reason
*   Themes support, porting 1-2 popular themes from Wakaba
*   REST API for custom UIs and user applications -
    *   get/list/create board/thread/post
    *   check update for board/thread
    *   markup preview function
*   Websocket API for custom UIs and live update -
    *   constant update flow for board/thread
*   (in progress) Modular support for: post parsing, themes, additional
    site sections, post display

### Tier 3 TODO: want, but in future

*   Video embedding support (configurable)
*   Multiple file upload (almost done!)
*   Tripcodes support
*   TOR/proxy detection
*   Fighting Unicode quirks (reverse text direction)
*   A torrent-tracker module (tourettes or peasy or something else?)
*   Permanent authentication, VIP passwords
*   International board mode (geoip flags)
*   Oekaki drawing support
*   Mobile-friendly template (entirely new mobile mode?)

## Installing

### Before you start

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

### Building

*   Checkout from github by using `git clone git://github.com/kvakvs/macaba.git`
*   $ `make run` will do full recompile and start (or `make runf` to run without
    recompiling all deps)
*   Open http://localhost:12000/ with your browser

This will download 'rebar', check out dependencies to 'deps/' subdirectory,
compile dependencies, compile macaba, and run locally on port 12000
(configurable in macaba.config).
