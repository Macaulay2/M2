--		Copyright 1994 by Daniel R. Grayson

-- the reason we need Handles to be mutable is so the 'new' command will not
-- copy the Handle after it's return from the initialization routine.  If it
-- does, then the old one will eventually get freed, and since it was registered
-- with the garbage collector, the engine will be told to release the object
-- with the corresponding handle.

load "cmdnames.m2"

assert(class ggaddress === String)
ggnewh := concatenate(ggaddress,ggtonet)
newHandle = ggcmds -> toHandle convert(ConvertInteger, sendgg (ggcmds,ggnewh))

handle = x -> x.handle

