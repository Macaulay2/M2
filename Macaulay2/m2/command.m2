--		Copyright 1993-1999 by Daniel R. Grayson

-- documentation is in doc.m2 because this file loaded early

SelfInitializingType = new Type of Type
SelfInitializingType.synonym = "self initializing type"
SelfInitializingType Thing := (T,z) -> new T from z

SelfInitializingType\VisibleList := (T,z) -> (i -> T i) \ z
List/SelfInitializingType := VisibleList/SelfInitializingType := (z,T) -> z / (i -> T i)
SelfInitializingType \\ Thing := (T,z) -> T z
Thing // SelfInitializingType := (z,T) -> T z

Command = new SelfInitializingType of BasicList
Command.synonym = "command"
new Command from Function := Command => (command,f) -> command {f}
new Command from String   := Command => (command,cmdname) -> command {
     x -> (
	  if x === ()
	  then run cmdname
	  else run (cmdname | " " | toString x))}
Command#(Standard,AfterEval) = x -> Thing#(Standard,AfterEval) x#0 ()
Command Thing := (x,y) -> x#0 y


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
