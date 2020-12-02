--		Copyright 1993-1999, 2008 by Daniel R. Grayson

-----------------------------------------------------------------------------
-- code
-----------------------------------------------------------------------------

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

-- stores previously listed methods or hooks, to be used by (code, ZZ)
previousMethodsFound := null

code = method(Dispatch => Thing)
code Nothing    := identity
code Symbol     :=
code Pseudocode := s -> getSourceLines locate s
code Sequence   := s -> (
    key := select(s, x -> not instance(x, Option));
    -- handle strategies
    func := if not #key === #s then (
	opts := new OptionTable from toList select(s, x -> instance(x, Option));
	if opts.?Strategy then (
	    strategy := opts.Strategy;
	    store := getHookStore(key, false);
	    if store =!= null and store#?key
	    and store#key.HookAlgorithms#?strategy
	    then store#key.HookAlgorithms#strategy));
    if func =!= null or (func = lookup key) =!= null
    then "-- code for method: "          | formatDocumentTag key || code func
    else "-- no method function found: " | formatDocumentTag key)
code Function   := f -> codeFunction(f, 0)
code Command    := C -> code C#0
code List       := L -> stack between_"---------------------------------" apply(L, code)
code ZZ         := i -> code previousMethodsFound#i

-----------------------------------------------------------------------------
-- edit
-----------------------------------------------------------------------------

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
-- methods
-----------------------------------------------------------------------------

searchAllDictionaries := (T, f) -> (
    seen := new MutableHashTable;
    scan(flatten \\ pairs \ dictionaryPath, (name, sym) -> (
	    if instance(v := value sym, T) and not seen#?v then ( seen#v = true; f(v)))))

isUnaryAssignmentOperator  := key ->  instance(key, Sequence) and #key === 2 and key#1 === symbol=
isBinaryAssignmentOperator := key -> (instance(key, Sequence) and #key === 3
    and isUnaryAssignmentOperator key#0 and instance(key#1, Type) and instance(key#2, Type))

thingMethods := (T, F) -> nonnull apply(pairs T, (key, func) -> if instance(func, Function) then
    -- TODO: unary methods are installed as T#f, change it to T#(f, T), then simplify this
    if key === F                                               then (key, T) else -- unary method, e.g quotient
    -- TODO: unary assignments operators are installed as T#(s, symbol=), change it to T#((s, symbol=), T), then simplify this
    if isUnaryAssignmentOperator key and member(F,        key) then (key, T) else -- unary assignment method, e.g symbol=
    if instance(key, Sequence)       and member(F, splice key) then  key)

sequenceMethods := (T, F, tallyF) -> nonnull apply(pairs T, (key, func) -> if instance(func, Function) then
    if isBinaryAssignmentOperator key and tallyF <= tally splice  key     then  key     else -- e.g T#((symbol SPACE, symbol=), T, T)
    if  isUnaryAssignmentOperator key and tallyF <= tally splice (key, T) then (key, T) else -- e.g T#(symbol+, symbol=)
    if instance(key, Keyword)         and tallyF <= tally splice (key, T) then (key, T) else -- e.g T#(symbol #)
    if instance(key, Function)        and tallyF <= tally splice (key, T) then (key, T) else -- e.g T#resolution
    if instance(key, Sequence)        and tallyF <= tally         key     then  key)

methods = method(Dispatch => Thing, TypicalValue => NumberedVerticalList)
methods Command  := c -> methods c#0
methods Type     := F -> methods sequence F
methods Sequence := F -> (
    found := new MutableHashTable;
    tallyF := tally splice F;
    searchAllDictionaries(Type, T -> scan(sequenceMethods(T, F, tallyF), key -> found#key = true));
    scan(select(F, e -> instance(e, Type)), T -> scan(sequenceMethods(T, F, tallyF), key -> found#key = true));
    previousMethodsFound = new NumberedVerticalList from sortByName keys found)

methods ScriptedFunctor := -- TODO: OO and other scripted functors aren't supported
methods Symbol :=
methods Thing  := F -> (
    if F === HH then return join(methods homology, methods cohomology);
    found := new MutableHashTable;
    -- TODO: either finish or remove nullaryMethods
    if nullaryMethods#?(1:F) then found#(1:F) = true;
    searchAllDictionaries(Type, T -> scan(thingMethods(T, F), key -> found#key = true));
    previousMethodsFound = new NumberedVerticalList from sortByName keys found)

-----------------------------------------------------------------------------
-- hooks
-----------------------------------------------------------------------------

listHooks := (key, opts) -> (
    -- list global hooks
    if key === () then return hooks(GlobalHookStore, opts);
    if instance(key#0, MutableHashTable)
    -- get the store from the first argument
    then (store := key#0; key = if key#?1 then key#1 else null)
    -- get the store from the key
    else  store  = getHookStore(key, false);
    new NumberedVerticalList from (
	alg := if opts.?Strategy then opts.Strategy;
	type := class alg;
	store = if store#?key then store#key;
	-- if no hooks have been installed, return empty list
	if store === null then {} else
	-- if Strategy is not given, list all available hooks
	if alg === null then apply(store.HookPriority, alg -> splice(key, Strategy => alg)) else
	-- if Strategy is given, and it is among the known strategies, list only that hook
	if store.HookAlgorithms#?alg  then { splice(key, Strategy => alg)  } else
	-- otherwise, if the class of alg is a known strategy, list only that hook
	if store.HookAlgorithms#?type then { splice(key, Strategy => type) } else {}))

hooks = method(Dispatch => Thing, Options => {Strategy => null})
hooks ZZ        := opts -> i   -> hooks previousMethodsFound#i
hooks List      := opts -> L   -> previousMethodsFound = join apply(toSequence L, key -> listHooks(key, opts))
hooks Thing     := opts -> key -> previousMethodsFound = hooks(methods key, opts)
hooks Symbol    := opts -> sym -> previousMethodsFound = hooks(1:sym, opts)
hooks Sequence  := opts -> key -> previousMethodsFound = listHooks(key, opts)
hooks HashTable := opts -> store -> previousMethodsFound = join(
    if store.?cache then store = store.cache;
    if store.?Hooks then store = store.Hooks;
    apply(toSequence keys store, key -> listHooks((store, key), opts)))

-----------------------------------------------------------------------------
-- debugger
-----------------------------------------------------------------------------
-- TODO: move to debugging?

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
-- This is called from interp.dd
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
