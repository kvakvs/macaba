# TODO technical debt list

*   Move types from HRL file to ERL files and export them.
*   HTTP client tests for empty board, new thread, post reply, upload image,
    upload bad file, check all possible error conditions, etc.
*   Attach size limit per board
*   ETag/If-mod-since support for attach and possibly thread/board serving
*   Extend image object keys to be unique over board, not whole site
*   Memory overflow protection if multiple slow clients request same big file
*   Big upload protection (waiting for support in Cowboy)

# Tech Debt Done

*   Extend board object keys, add (site name and) board to all object keys
*   When posting to thread, check that thread exists
