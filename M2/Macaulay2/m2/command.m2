--		Copyright 1994 by Daniel R. Grayson

-- documentation is in doc.m2 because this file loaded early

SelfInitializingType = new Type of Type
SelfInitializingType.name = "SelfInitializingType"
SelfInitializingType.symbol = quote SelfInitializingType
SelfInitializingType Thing := (T,z) -> new T from z

Command = new SelfInitializingType of BasicList
Command.name = "Command"
new Command from Function := (command,f) -> command {f}
new Command from String   := (command,cmdname) -> command {
     x -> (
	  if x === ()
	  then run cmdname
	  else run (cmdname | " " | string x))}
AfterEval Command := x -> x#0 ()
Command Thing := (x,y) -> x#0 y

