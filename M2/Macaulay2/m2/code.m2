--		Copyright 1993-1999, 2008 by Daniel R. Grayson

getSourceLines = method(Dispatch => Thing) 
getSourceLines Nothing := null -> null
getSourceLines Sequence := x -> ((filename,start,startcol,stop,stopcol) -> if filename =!= "stdio" and filename =!= "a string" then (
     wp := set characters " \t\r);";
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
     if #file < stop then error("line number ",toString stop, " not found in file ", filename);
     while stop >= start and file#(stop-1) === "" do stop = stop-1;
     stack prepend(
	  concatenate(filename, ":", toString start, if stop > start then ("-" ,toString stop),": --source lines:"),
	  apply(start-1 .. stop-1, i -> file#i)
	  )
     )) x

limit := 4
indent := n -> "| "^(height n, depth n) | n

codeFunction := (f,depth) -> (
     if depth <= limit then (
	  if locate f === null then concatenate("function '", toString f, "': source code not available")
	  else stack(
	       syms := flatten \\ sortByHash \ values \ drop(localDictionaries f,-1);
	       getSourceLines locate f,
	       if #syms > 0 then indent listSymbols syms,
	       if codeHelper#?(functionBody f) 
	       then toSequence apply(
		    codeHelper#(functionBody f) f, 
		    (comment,val) -> indent stack (
			      comment, 
			      if instance(val, Function) then codeFunction(val,depth+1) else net val
			      )))))
code = method(Dispatch => Thing)
code Nothing := null -> null
code Symbol := code Pseudocode := s -> getSourceLines locate s
code Sequence := s -> (
     f := lookup s;
     if f =!= null then "-- code for method: " | formatDocumentTag s || code f 
     else "-- no method function found: " | formatDocumentTag s
     )
code Function := f -> codeFunction(f,0)
code List := v -> stack between_"---------------------------------" apply(v,code)
code Command := cmd -> code cmd#0

EDITOR := () -> if getenv "EDITOR" != "" then getenv "EDITOR" else "vi"
editMethod := method(Dispatch => Thing)
editMethod String := filename -> (
     editor := EDITOR();
     run concatenate(
	  if getenv "DISPLAY" != "" and editor != "emacs" then "xterm -e ",
	  editor, " ", filename))
EDIT := method(Dispatch => Thing)
EDIT Nothing := arg -> (stderr << "--warning: source code not available" << endl;)
EDIT Sequence := x -> ((filename,start,startcol,stop,stopcol) -> (
     editor := EDITOR();
     if 0 != run concatenate(
	  if getenv "DISPLAY" != "" and editor != "emacs" then "xterm -e ",
	  editor,
	  " +",toString start,
	  " ",
	  filename
	  ) then error "command returned error code")) x
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
methods = method(Dispatch => Thing, TypicalValue => VerticalList)
methods Command := c -> methods c#0
methods Type := F -> (
     seen := new MutableHashTable;
     found := new MutableHashTable;
     seen#F = true;
     scan(pairs F, (key,meth) -> (
	       if instance(meth, Function) then (
		    if class key === Sequence and member(F,key) then found#key = true
	       	    else if instance(key, Function) then found#(key,F) = true
		    else if instance(key, Symbol) and instance(key,Keyword) then found#(key,F) = true
		    )
	       )
	  );
     scan(flatten(pairs \ dictionaryPath),
	  (Name,sym) -> (
	       x := value sym;
	       if instance(x,Type) and not seen#?x then (
		    seen#x = true;
		    scan(pairs x, (key,meth) -> (
			      if instance(meth, Function) then
			      if key === F then found#(F,x) = true
			      else if class key === Sequence and member(F,key)
			      then found#key = true)))));
     -- sort -- too slow
     new VerticalList from sortByName keys found)

methods Sequence := F -> (
     seen := new MutableHashTable;
     found := new MutableHashTable;
     tallyF := tally F;
     scan(flatten(pairs \ dictionaryPath),
	  (Name,sym) -> (
	       x := value sym;
	       if instance(x,Type) and not seen#?x then (
		    seen#x = true;
		    scan(pairs x, (key,meth) -> (
			      if instance(meth, Function) 
			      and class key === Sequence and tallyF <= tally key
			      then found#key = true)))));
     -- sort -- too slow
     new VerticalList from sortByName keys found)

methods Thing := F -> (
     if F === HH then return join(methods homology, methods cohomology);
     seen := new MutableHashTable;
     found := new MutableHashTable;
     if nullaryMethods#?(1:F) then found#(1:F) = true;
     scan(flatten(pairs \ dictionaryPath),
	  (Name,sym) -> (
	       x := value sym;
	       if instance(x,Type) and not seen#?x then (
		    seen#x = true;
		    scan(pairs x, (key,meth) -> (
			      if instance(meth, Function) then
			      if key === F or class key === Sequence and #key === 2 and (key#0 === F or F === symbol =) and key#1 === symbol =
			      then found#(key,x) = true
			      else if class key === Sequence and (
				   member(F,key) 
				   or 
				   key#?0 and class key#0 === Sequence and member(F,key#0)
				   )
			      then found#key = true)))));
     -- sort -- too slow
     new VerticalList from sortByName keys found)

debuggerUsageMessage = ///--debugger commands that leave the debugger:
    break               -- leave the debugger, returning to top level
    end                 -- abandon the code, enter debugger one level up
    step 0              -- bypass current expression
    step                -- execute current expression, step 1 line
    step n              -- execute current expression, step n lines
    step -n             -- execute current expression, trace n microsteps
    continue            -- execute current expression, continue
    return              -- return null as value of current expression
    return x            -- return x as value of current expression
-- other debugger commands:
    listLocalSymbols    -- display local symbols and their values
    listUserSymbols     -- display user symbols and their values
    code current        -- source code of current expression
    value current       -- execute current expression, obtain value
    disassemble current -- microcode of current expression///

inDebugger = false
addStartFunction(() -> inDebugger = false)
debuggerHook = entering -> (
     if entering then (
	  pushvar(symbol inDebugger, true);
	  << code current << endl;
	  )
     else (
	  popvar symbol inDebugger;
	  )
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
