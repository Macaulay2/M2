-- No longer crashes: 13 Jan 2008

(matrix {{2.}}) ^ 1000

crash:

    i1 : (matrix {{2.}}) ^ 1000
    -- SIGSEGV
    -- stack trace:
    level 0 -- return addr: 0x080522d0 -- frame: 0xbf8b8ad8
    level 1 -- return addr: 0xffffe420 -- frame: 0xbf8b8af8
    level 2 -- return addr: 0x0816fa8a -- frame: 0xbf8b8df8
    level 3 -- return addr: 0x32383634 -- frame: 0xbf8b8e98
    Segmentation fault (core dumped)
