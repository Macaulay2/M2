--		Copyright 1993-1999, 2008 by Daniel R. Grayson

getSourceLines = method(Dispatch => Thing) 
getSourceLines Nothing := null -> null
getSourceLines Sequence := x -> (
     (filename,start,startcol,stop,stopcol,pos,poscol) -> if filename =!= "stdio" then (
	  wp := set characters " \t\r);";
	  file := (
	       if match("startup.m2.in$", filename) then startupString
	       else if filename === "currentString" then currentString
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
	       concatenate(filename, ":", 
		    toString start, ":", toString (startcol+1),
		    "-",
		    toString stop, ":", toString (stopcol+1),
		    ": --source code:"),
	       apply(start-1 .. stop-1, i -> file#i)
	       )
	  )) x

limit := 4
indent := n -> "| "^(height n, depth n) | n

codeFunction := (f,depth) -> (
     if depth <= limit then (
	  if locate f === null then concatenate("function ", toString f, ": source code not available")
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

previousMethodsFound := null
code ZZ := i -> code previousMethodsFound#i

EDITOR := () -> if getenv "EDITOR" != "" then getenv "EDITOR" else "vi"
editMethod := method(Dispatch => Thing)
editMethod String := filename -> (
     editor := EDITOR();
     chkrun concatenate(
	  if getenv "DISPLAY" != "" and editor != "emacs" then "xterm -e ",
	  editor, " ", filename))
EDIT := method(Dispatch => Thing)
EDIT Nothing := arg -> (stderr << "--warning: source code not available" << endl;)
EDIT Sequence := x -> ((filename,start,startcol,stop,stopcol,pos,poscol) -> (
     editor := EDITOR();
     if 0 != chkrun concatenate(
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
     then chkrun concatenate(
	  if getenv "DISPLAY" != "" and editor != "emacs" then "xterm -e ",
	  editor)
     else EDIT locate args
     )
edit = Command editMethod

-----------------------------------------------------------------------------
methods = method(Dispatch => Thing, TypicalValue => NumberedVerticalList)
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
     previousMethodsFound = new NumberedVerticalList from sortByName keys found)

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
     previousMethodsFound = new NumberedVerticalList from sortByName keys found)

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
     previousMethodsFound = new NumberedVerticalList from sortByName keys found)

hooks = method()
hooks   (MutableHashTable,Thing) := (obj,key) -> previousMethodsFound = new NumberedVerticalList from obj#key
hooks   (HashTable,Thing) := (obj,key) -> previousMethodsFound = new NumberedVerticalList from (obj.cache)#key
hooks   (Symbol) := (sym) -> previousMethodsFound = new NumberedVerticalList from (value sym)


debuggerUsageMessage = ///--debugger activation depth control:
    errorDepth=3   	-- activate at positions in user code (default)
    errorDepth=2   	-- activate also at positions in packages
    errorDepth=1   	-- activate also at positions in Core
    errorDepth=0   	-- activate also at positions in the loader
--debugging control:
    return              -- bypass current expression, return null, stop
    return x            -- bypass current expression, return x, stop
    step                -- step 1 line
    step n              -- step n lines
    step (-n)           -- trace n microsteps
    end (or eof char)   -- enter debugger one level up
    continue            -- leave the debugger, continuing execution
                        -- with current expression
    break               -- leave the debugger, returning to top level
--debugging information:
    listLocalSymbols    -- display local symbols and their values
    listUserSymbols     -- display user symbols and their values
    current             -- the current expression; initially, the one
    	      	   	-- that produced an error
    code current        -- source code of current expression
    value current       -- execute current expression, obtain value
    disassemble current -- display microcode of current expression
    currentString       -- the string being evaluated by 'value', if
                        -- an error occurred within it
-- emacs commands in *M2* buffer:
    RET                 -- on an file/position line, go to source///

inDebugger = false
addStartFunction(() -> inDebugger = false)
debuggerHook = entering -> (
     if entering then (
	  pushvar(symbol inDebugger, true);
	  c := code current;
	  if c =!= null then << c << endl;
	  )
     else (
	  popvar symbol inDebugger;
	  )
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
