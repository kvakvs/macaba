# macaba

Anonymous message board server made in Erlang, designed with high load,
scalability and extensibility in mind. The forum layout concept is designed to
be familiar for wakaba users:

*   site &rarr; boards &rarr; threads &rarr; posts &rarr; images and text
*   posts have monotonically increasing ids, one counter per board
*   name field is optional, supports special hash tags for identifying without
    discovering the user's identity
*   post body uses special markup which is processed to HTML

### Documentation

REST API: http://docs.macaba.apiary.io/

### Project status - Alpha

You can download and run it. Production use is not recommended

### Features

[Features and TODO list](https://github.com/kvakvs/macaba/blob/master/FEATURES.md)
can be found here.

### Demo

I have configured minimal free instance on Amazon EC2 and started a running copy of
the board there. Board is in demo mode, i.e. resets database fully every 15 minutes.
You can try it here: http://macaba.longcat.info admin login and password are "1"

## Installing

### Before you start

*   Linux operating system (Ubuntu works, Debian and other should be easy),
    MacOSX _may_ work, but you're on your own building dependencies!
*   Erlang/OTP R15 or R16, get from
    https://www.erlang-solutions.com/downloads/download-erlang-otp - requires no
    additional configuration.
*   RIAK database, get from http://docs.basho.com/riak/latest/downloads/ -
    requires no configuration just install and ensure its started by doing
    `sudo /etc/init.d/riak restart`
*   Imagemagick for making thumbnails and image analysis,
    (`apt-get install libmagickwand-dev` on Ubuntu)
    *   NOTE: this step is **optional** if you choose to not
        install thumbnailer, disable it in the `macaba.config` file, option
        `board.thumbnailer` also you can comment out 'eim' dependency in
        `apps/macaba/rebar.config` and `rm -rf deps/eim` to get rid of build
        errors
*   A working C/C++ compiler to build dependencies
    (`apt-get install build-essential` on Ubuntu)

### Building

*   Checkout from github by using `git clone git://github.com/kvakvs/macaba.git`
*   $ `make run` will do full recompile and start (or `make runf` to run without
    recompiling all deps)
*   Open http://localhost:12000/ with your browser

This will download 'rebar', check out dependencies to 'deps/' subdirectory,
compile dependencies, compile macaba, and run locally on port 12000
(configurable in `macaba.config`).
