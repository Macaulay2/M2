--		Copyright 1993-1999 by Daniel R. Grayson

getSourceLines := method(SingleArgumentDispatch=>true) 
getSourceLines Nothing := null -> null
getSourceLines Sequence := (filename,start,startcol,stop,stopcol) -> if filename =!= "stdio" then (
     wp := set characters " \t);";
     file := (
	  if filename === "layout.m2"
	  then startupString1
	  else if filename === "startup.m2"
	  then startupString2
	  else (
	       if not fileExists filename then error ("couldn't find file ", filename);
	       get filename
	       )
	  );
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

limit := 4
optionedFunction := {} >>> sin
composedFunction := sin @@ sin
memoizedFunction := memoize sin
isOptionedFunction := f -> sameFunctionBody(f,optionedFunction)
isComposedFunction := f -> sameFunctionBody(f,composedFunction)
isMemoizedFunction := f -> sameFunctionBody(f,memoizedFunction)

codeFunction := (f,depth) -> (
     if depth <= limit and locate f =!= null then stack(
	  try getSourceLines locate f else concatenate("source code file '",first locate f,"' not available"),
	  if isOptionedFunction f then (
	       "-- original function f:", codeFunction(last frame f,depth+1)
	       )
	  else if isComposedFunction f then (
	       "-- left hand function f:" , codeFunction((frame f)#0,depth+1),
	       "-- right hand function g:", codeFunction((frame f)#1,depth+1)
	       )
	  else if isMemoizedFunction f then (
	       "-- original function f:", codeFunction(first frame f,depth+1)
	       )
	  )
     )
code = method(SingleArgumentDispatch=>true)
code Nothing := null -> null
code Symbol := code Pseudocode := s -> getSourceLines locate s
code Sequence := s -> code lookup s
code Function := f -> codeFunction(f,0)
code List := v -> stack apply(v,code)
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
EDIT Sequence := (filename,start,startcol,stop,stopcol) -> (
     editor := EDITOR();
     if 0 != run concatenate(
	  if getenv "DISPLAY" != "" and editor != "emacs" then "xterm -e ",
	  editor,
	  " +",toString start,
	  " ",
	  filename
	  ) then error "command returned error code")
editMethod Function := args -> EDIT locate args
editMethod Command := c -> editMethod c#0
editMethod Sequence := args -> (
     editor := EDITOR();
     if args === () 
     then run concatenate(
	  if getenv "DISPLAY" != "" and editor != "emacs" then "xterm -e ",
	  editor)
     else EDIT locate args
     )
edit = Command editMethod

-----------------------------------------------------------------------------
methods = method(SingleArgumentDispatch => true)
methods Type := F -> (
     seen := new MutableHashTable;
     found := new MutableHashTable;
     seen#F = true;
     scan(pairs F, (key,meth) -> (
	       if class meth === Function then (
		    if class key === Sequence and member(F,key) then found#key = true
	       	    else if class key === Function then found#(key,F) = true
		    else if class key === Symbol and operatorSet#?key then found#(key,F) = true
		    )
	       )
	  );
     scan(flatten(pairs \ globalDictionaries),
	  (Name,sym) -> (
	       x := value sym;
	       if instance(x,Type) and not seen#?x then (
		    seen#x = true;
		    scan(pairs x, (key,meth) -> (
			      if class meth === Function then
			      if key === F then found#(F,x) = true
			      else if class key === Sequence and member(F,key)
			      then found#key = true)))));
     sort keys found)

methods Sequence := F -> (
     seen := new MutableHashTable;
     found := new MutableHashTable;
     tallyF := tally F;
     scan(flatten(pairs \ globalDictionaries),
	  (Name,sym) -> (
	       x := value sym;
	       if instance(x,Type) and not seen#?x then (
		    seen#x = true;
		    scan(pairs x, (key,meth) -> (
			      if class meth === Function 
			      and class key === Sequence and tallyF <= tally key
			      then found#key = true)))));
     sort keys found)

methods Thing := F -> (
     if F === HH then return join(methods homology, methods cohomology);
     seen := new MutableHashTable;
     found := new MutableHashTable;
     scan(flatten(pairs \ globalDictionaries),
	  (Name,sym) -> (
	       x := value sym;
	       if instance(x,Type) and not seen#?x then (
		    seen#x = true;
		    scan(pairs x, (key,meth) -> (
			      if class meth === Function then
			      if key === F then found#(F,x) = true
			      else if class key === Sequence and member(F,key)
			      then found#key = true)))));
     sort keys found)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
