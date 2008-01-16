--		Copyright 1993-1999 by Daniel R. Grayson

-- documentation is in doc.m2 because this file loaded early

Command = new SelfInitializingType of BasicList
Command.synonym = "command"
new Command from Function := Command => (command,f) -> command {f}
new Command from String   := Command => (command,cmdname) -> command {
     x -> (
	  if x === ()
	  then run cmdname
	  else run (cmdname | " " | toString x))}
Command#AfterEval = x -> Thing#AfterEval x#0 ()
Command Thing := (x,y) -> x#0 y


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
