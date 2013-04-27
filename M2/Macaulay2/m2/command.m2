--		Copyright 1993-1999,2009 by Daniel R. Grayson

chkrun = cmd -> (
     r := run cmd;
     if r == 2			-- 2 is the return value for Mac OS X and linux
     then (
	  stderr << newline;
	  error("run: subprocess interrupted: return code " | toString r);
	  );
     r) 

Command = new SelfInitializingType of BasicList
Command.synonym = "command"
globalAssignment Command
new Command from Function := Command => (command,f) -> command {f}
new Command from String   := Command => (command,cmdname) -> command {
     x -> (
	  if x === ()
	  then chkrun cmdname
	  else chkrun (cmdname | " " | toString x))}
Command#AfterEval = x -> Thing#AfterEval x#0 ()
Command Thing := (x,y) -> x#0 y


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
