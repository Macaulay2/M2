--		Copyright 1994 by Daniel R. Grayson

-- the reason we need Handles to be mutable is so the 'new' command will not
-- copy the Handle after it's return from the initialization routine.  If it
-- does, then the old one will eventually get freed, and since it was registered
-- with the garbage collector, the engine will be told to release the object
-- with the corresponding handle.

load "cmdnames.m2"

document { quote Handle,
     TT "Handle", " -- the class of all Handles.",
     PARA,
     "This concept is mainly for internal use.",
     PARA,
     "A handle is essentially a small integer by means of which the ", TO "engine", "
     refers to the algebraic entities in it.  A Handle (capitalize), on the other
     hand, is a hash table, and in it, under the key ", TO "value", " is stored
     the handle.",
     PARA,
     "One advantage of a Handle is that it can be registered with the
     Boehm garbage collector for last minute action at the time the Handle
     is about to be destroyed.",
     PARA,
     MENU {
	  TO "newHandle"
	  }
     }

assert(class ggaddress === String)
ggnewh := concatenate(ggaddress,ggtonet)
newHandle = ggcmds -> toHandle convert(ConvertInteger, sendgg (ggcmds,ggnewh))

document { quote newHandle,
     TT "newHandle x", " -- passes the commands ", TT "x", " to the engine 
     with ", TO "sendgg", ", pops an object off the engine's stack and 
     produces the handle.",
     SEEALSO ("toHandle", "Handle" )
     }

document { quote toHandle,
     TT "toHandle i", " -- convert the integer i to a ", TO "Handle", ".",
     PARA,
     "No checking is done to ensure that the integer i actually has
     been assigned by the ", TO "engine", " to one of its objects."
     }

handle = x -> x.handle

document { quote handle,
     TT "handle x", " -- produces the ", TO "Handle", " for the object x.",
     PARA,
     "The corresponding symbol is used as a key under which to store
     the handle."
     }
