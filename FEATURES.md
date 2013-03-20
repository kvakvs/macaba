# Features

## Done

*   General board features
    *   Designed for running as a single node or as multiple nodes
    *   Basic board functions: board index, thread index, thread view, new
        thread, reply
    *   Basic plain HTML scriptless theme (later REST API, Websocket)
    *   File upload (1 per post). Image previews via Imagemagick (can be
        disabled in config)
    *   WakabaMark (no ^H support as of yet) - bold, italic, inline code, code
        quotes, spoilers using [spoiler][/spoiler], URL, ordered and unordered
        lists
    *   Markdown support (can replace WakabaMark)
    *   Board limits, delete when thread is sinking
    *   Deleting posts by user, optional deletion of attachments only
    *   Optional per board poster_id, a base62 encoded hash of ip/user-agent
*   Moderator/Administrator UI
    *   Login form and login handler located at /admin
    *   Sessions support
    *   Admin: Site options, maintenance mode, boards list and board options
*   REST API
    *   Preview function for post markup
*   Other features
    *   Simple plugins system. Extension modules and hooks are work-in-progress
    *   REST API work in progress http://docs.macaba.apiary.io/
*   Quality and testing
    *   Board post/reply/upload tests (not perfect, but good to have!)
    *   WakabaMark markup tests and some other unit tests

## In progress

*   Mod: Thread management - locking, pinning, deleting, moving
*   REST: Thread and post manage REST calls

## Tier 1 TODO: important

*   Mod: Words/regex blacklisting, image hash/similarity? blacklisting
*   Mod: User blacklisting, bans, ban page with ban reason
*   Timestamps, Etags, If-modified-since, Expires HTTP headers support:
    *   board_dynamic
    *   thread_dynamic
    *   post view as separate resource (related to tier2 todo for post resource)
    *   attachments
*   Different backends for storing attachments (static file system, S3)
*   Modules/hooks support for: post parsing, themes, additional
    site sections, post display
*   Import quickstart scripts for Wakaba/Kusaba

## Tier 2 TODO: do it later

*   Captcha support, flood detection, basic DDOS resistance
*   Thread viewing mode (pagination, last X posts), post as separate resource
*   Mod-deleted posts should be visible in thread
*   Users participation - reporting posts and threads, Mod: Reviewing reported
    posts
*   Themes support, porting 1-2 popular themes from Wakaba
*   REST API for custom UIs and user applications -
    *   get/list/create board/thread/post
    *   check update for board/thread
    *   markup preview function
*   Websocket API for custom UIs and live update for board/thread
*   X-Frame-Options to disallow embedding at least for authenticated users

## Tier 3 TODO: want, but in future

*   Multiple file upload (almost done!)
*   TOR/proxy detection
*   Fighting Unicode quirks (reverse text direction)
*   A torrent-tracker module (tourettes or peasy or something else?)
*   Permanent authentication, VIP passwords
*   Mobile-friendly template (entirely new mobile mode?)

## Plugin ideas

*   International board mode (geoip flags)
*   Tripcodes support
*   Video embedding support (configurable)
*   Oekaki drawing support
*   Masking post number last 2-3 digits to fight "get" hunting
*   Masking last digits of user IP for mods
