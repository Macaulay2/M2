--		Copyright 1993-1999 by Daniel R. Grayson

location := method(SingleArgumentDispatch=>true)
location Sequence := args -> (
     r := lookup args;
     if r === null then error "no such method found"
     else locate r
     )
location Thing := locate

netLocation := method(SingleArgumentDispatch=>true) 
netLocation Nothing := null -> (stderr << "source code not available" << endl;)
netLocation Sequence := (filename,start,stop) -> (
     wp := set characters " \t);";
     file := get filename;
     if file === null then error ("couldn't find file ", filename);
     file = lines file;
     while (
	  file#?stop 
     	  and (				  -- can improve this
	       l := set characters file#stop;
	       l #? ")" and isSubset(l, wp)
	       )
	  ) do stop = stop + 1;
     while stop >= start and file#(stop-1) === "" do stop = stop-1;
     stack prepend(
	  concatenate("-- ",filename, ":", toString start, if stop > start then ("-" ,toString stop)),
	  apply(start-1 .. stop-1, i -> file#i)
	  )
     )

code = method(SingleArgumentDispatch=>true)
code Symbol := code Sequence := code Function := args -> netLocation location args
code List := v -> stack apply(v,i -> try code i else "-- source code not available : " | toString i)
code Command := cmd -> code cmd#0

EDITOR := () -> if getenv "EDITOR" != "" then getenv "EDITOR" else "vi"
editMethod := method(SingleArgumentDispatch=>true)
editMethod String := filename -> (
     editor := EDITOR();
     run concatenate(
	  if getenv "DISPLAY" != "" and editor != "emacs" then "xterm -e ",
	  editor, " ", filename))
EDIT := method(SingleArgumentDispatch=>true)
EDIT Nothing := arg -> (stderr << "source code not available" << endl;)
EDIT Sequence := (filename,start,stop) -> (
     editor := EDITOR();
     run concatenate(
	  if getenv "DISPLAY" != "" and editor != "emacs" then "xterm -e ",
	  editor,
	  " +",toString start,
	  " ",
	  filename
	  -- resolve filename -- the old way
	  ))
editMethod Function := args -> EDIT location args
editMethod Command := c -> editMethod c#0
editMethod Sequence := args -> (
     editor := EDITOR();
     if args === () 
     then run concatenate(
	  if getenv "DISPLAY" != "" and editor != "emacs" then "xterm -e ",
	  editor)
     else EDIT location args
     )
edit = Command editMethod

-----------------------------------------------------------------------------
methods = F -> (
     seen := new MutableHashTable;
     found := new MutableHashTable;
     if instance(F,Type) then (
	  seen#F = true;
	  scan(pairs F, (key,meth) -> (
		    if class meth === Function 
		    then if class key === Sequence and member(F,key)
		    then found#key = true
		    else found#(key,F) = true
		    )));
     if class F === Sequence then (
	  tallyF := tally F;
	  scanPairs(symbolTable(),
	       (Name,sym) -> (
		    x := value sym;
		    if instance(x,Type) and not seen#?x then (
			 seen#x = true;
			 scan(pairs x, (key,meth) -> (
				   if class meth === Function 
				   and class key === Sequence and tallyF <= tally key
				   then found#key = true))))))
     else (
	  scanPairs(symbolTable(),
	       (Name,sym) -> (
		    x := value sym;
		    if instance(x,Type) and not seen#?x then (
			 seen#x = true;
			 scan(pairs x, (key,meth) -> (
				   if class meth === Function then
				   if key === F then found#(F,x) = true
				   else if class key === Sequence and member(F,key)
				   then found#key = true))))));
     sort keys found)

