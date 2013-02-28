# TODO technical debt list

*   ETag/If-mod-since support for attach and possibly thread/board serving
*   HTTP client tests for empty board, new thread, post reply, upload image,
    upload bad file, check all possible error conditions, etc.
*   Extend board object keys, add (site name and) board to all object keys
*   Extend image object keys as well
*   Memory overflow protection if multiple slow clients request same big file
*   Big upload protection (waiting for support in Cowboy)

# Tech Debt Done

*   When posting to thread, check that thread exists
