--		Copyright 1995 by Daniel R. Grayson

-- search := (filename,fun) -> (
--      if substring(filename,0,1) === "/" 
--      or substring(filename,0,2) === "./" 
--      or substring(filename,0,3) === "../" then try fun filename else null
--      else (
-- 	  ret := null;
-- 	  select(1,path, 
-- 	       dir -> (
-- 	       	    fn := if dir == "." then filename else dir | "/" | filename;
-- 	       	    try (ret = fun fn; true) else false));
-- 	  ret))
-- getpath := (filename) -> search(filename,get)
-- resolve := (filename) -> search(filename,fn -> (close openIn fn; fn))

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
	  concatenate("-- ",filename, ":", name start, if stop > start then ("-" ,name stop)),
	  apply(start-1 .. stop-1, i -> file#i)
	  )
     )

code = method(SingleArgumentDispatch=>true)
code Symbol := code Sequence := code Function := args -> netLocation location args
code List := v -> stack apply(v,i -> try code i else "-- source code not available : " | name i)
code Command := cmd -> code cmd#0

EDITOR := () -> if getenv "EDITOR" != "" then getenv "EDITOR" else "vi"
edit = method(SingleArgumentDispatch=>true)
edit String := filename -> (
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
	  " +",name start,
	  " ",
	  filename
	  -- resolve filename -- the old way
	  ))
edit Function := args -> EDIT location args
edit Command := c -> edit c#0
edit Sequence := args -> (
     editor := EDITOR();
     if args === () 
     then run concatenate(
	  if getenv "DISPLAY" != "" and editor != "emacs" then "xterm -e ",
	  editor)
     else EDIT location args
     )
edit = Command edit

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
	       (Name,symbol) -> (
		    x := value symbol;
		    if instance(x,Type) and not seen#?x then (
			 seen#x = true;
			 scan(pairs x, (key,meth) -> (
				   if class meth === Function 
				   and class key === Sequence and tallyF <= tally key
				   then found#key = true))))))
     else (
	  scanPairs(symbolTable(),
	       (Name,symbol) -> (
		    x := value symbol;
		    if instance(x,Type) and not seen#?x then (
			 seen#x = true;
			 scan(pairs x, (key,meth) -> (
				   if class meth === Function then
				   if key === F then found#(F,x) = true
				   else if class key === Sequence and member(F,key)
				   then found#key = true))))));
     sort keys found)

