--		Copyright 1994 by Daniel R. Grayson

-- documentation is in doc.m2 because this file loaded early

SelfInitializingType = new Type of Type
SelfInitializingType.name = "SelfInitializingType"
SelfInitializingType.Symbol = symbol SelfInitializingType
SelfInitializingType Thing := (T,z) -> new T from z
SelfInitializingType\List := (T,z) -> (i -> T i) \ z
List/SelfInitializingType := (z,T) -> z / (i -> T i)

Command = new SelfInitializingType of BasicList
Command.name = "Command"
GlobalAssignHook Command := (X,x) -> if not Symbols#?x then Symbols#x = X
GlobalReleaseHook Command := (X,x) -> (
     stderr << "warning: " << string X << " redefined" << endl;
     if Symbols#x === X then remove(Symbols,x);
     )
new Command from Function := Command => (command,f) -> command {f}
new Command from String   := Command => (command,cmdname) -> command {
     x -> (
	  if x === ()
	  then run cmdname
	  else run (cmdname | " " | string x))}
AfterEval Command := x -> x#0 ()
Command Thing := (x,y) -> x#0 y

