--		Copyright 1993-1999, 2008 by Daniel R. Grayson

-- TODO: needs "document.m2" for formatDocumentTag, but this causes a loop
needs "debugging.m2" -- for FilePosition
needs "gateway.m2"
needs "lists.m2"
needs "methods.m2"
needs "nets.m2"

-----------------------------------------------------------------------------
-- code
-----------------------------------------------------------------------------

limit := 4

-- TODO: also show _where_ a method is declared
codeFunction := (key, func, level) -> if level <= limit then (
    l := locate func;
    c := code l;
    dicts := localDictionaries func;
    symbs := flatten apply(#dicts - 1, i -> sortByHash values dicts#i);
    hooks := if key === null then {} else apply(listHooks(key, null), code);
    DIV flatten {
	if c =!= null then c else SPAN{ synonym class func, " ", func, ": ",if l =!= null and hasAttribute(func,ReverseDictionary) then l else ""," source code not available"},
	    if #symbs > 0 then INDENT listSymbols symbs,
	    if codeHelper#?(functionBody func) then apply(
		codeHelper#(functionBody func) func,
		(comment, val) -> INDENT {
		    comment, BR{},
		    if instance(val, Function) then codeFunction(key, val, level+1) else hold val -- hold for OptionTable or Option
		    }),
	    if #hooks>0 then INDENT hooks
	    }
	)

-- stores previously listed methods, hooks, or tests to be used by (code, ZZ)
previousMethodsFound = null

codeAddress = pos -> ( pos, ": --source code:" ) -- [addr]:[line]:[char]-[line]:[char]:
codeContent = (s, e, filelines) -> PRE M2CODE stack filelines_{s-1 .. e-1}

-- e.g. see code methods(map, Module, List)
dedupMethods = L -> (
    L = new MutableList from L;
    scan(reverse(0..#L-2),
	-- since we use sortByLocation in methods, we assume
	-- that methods with identical code are adjacent in L
	i -> if last L#i === last L#(i+1) then (
	    tag := DIV drop(remove(L, i), -1);
	    L#i = join(tag, L#i)));
    toList L)

code = method(Dispatch => Thing)
code Nothing    := identity
code FilePosition := x -> (
    filename := x#0; start := x#1; stop := x#3 ?? x#1;
     (
	  wp := set characters " \t\r);";
	  file := (
	       if match("startup\\.m2\\.in$", filename) then startupString
	       else if filename === "currentString" then (
		    if currentString === null
		    then error "code no longer available"
		    else currentString)
	       else if filename === "stdio" then (
		    start = 1;
		    stop += 1 - x#1;
		    toString stack apply(x#1..x#1+stop-1,
			i -> getHistory(i + historyOffset)))
	       else (
		    if not fileExists filename then error ("couldn't find file ", filename);
		    get filename
		    )
	       );
	  file = lines file;
	  if #file < stop then error("line number ",toString stop, " not found in file ", filename);
	  DIV splice { codeAddress(x), codeContent(start, stop, file) }
	  ))
code Symbol     :=
code Pseudocode := s -> code locate s
code Sequence   := s -> (
    key := select(s, x -> not instance(x, Option));
    -- handle strategies
    mesg := "-- code for method: ";
    func := if not #key === #s then (
	mesg = "-- code for strategy: ";
	opts := new OptionTable from toList select(s, x -> instance(x, Option));
	if opts.?Strategy then (
	    strategy := opts.Strategy;
	    store := getHookStore(key, false);
	    if store =!= null and store#?key
	    and store#key.HookAlgorithms#?strategy
	    then store#key.HookAlgorithms#strategy));
    -- TODO: say "strategies for method: ..."
    if func =!= null or (func = lookup key) =!= null
    then DIV { DIV { mesg, formatDocumentTag s }, codeFunction(s, func, 0) }
    else "-- no method function found: " | formatDocumentTag key)
code Function   := f -> codeFunction(null, f, 0)
code Command    := C -> code C#0
code List       := L -> DIV between_(HR{}) dedupMethods apply(L, code)
code ZZ         := i -> code previousMethodsFound#i

-----------------------------------------------------------------------------
-- edit
-----------------------------------------------------------------------------
-- TODO: update this

editMethod = method(Dispatch => Thing)
editMethod String := filename -> (
     editor := getViewer("EDITOR", "emacs");
     chkrun concatenate(
	  if getenv "DISPLAY" != "" and editor != "emacs" then "xterm -e ",
	  editor, " ", filename))
editMethod Nothing := arg -> (stderr << "--warning: source code not available" << endl;)
editMethod FilePosition := x -> (
     filename := x#0; start := x#1;
     editor := getViewer("EDITOR", "emacs");
     if 0 != chkrun concatenate(
	  if getenv "DISPLAY" != "" and editor != "emacs" then "xterm -e ",
	  editor,
	  " +",toString start,
	  " ",
	  filename
	  ) then error "command returned error code")
editMethod Command := c -> editMethod c#0
editMethod Function := args -> editMethod locate args
editMethod Sequence := args -> (
    if args === () then (
	editor := getViewer("EDITOR", "emacs");
	chkrun concatenate(
	    if getenv "DISPLAY" != "" and editor != "emacs" then "xterm -e ",
	    editor)
	)
    else editMethod locate args
     )
editMethod ZZ := i -> editMethod previousMethodsFound#i
edit = Command editMethod

-----------------------------------------------------------------------------
-- methods
-----------------------------------------------------------------------------
-- TODO: https://github.com/Macaulay2/M2/issues/1331

-- Find in all dictionaries the symbols v
-- whose value has type T and execute f(v)
searchAllDictionaries := (T, f) -> (
    seen := new MutableHashTable;
    scan(flatten \\ pairs \ dictionaryPath, (name, sym) -> (
	    if instance(v := value sym, T) and not seen#?v then ( seen#v = true; f(v)))))

isUnaryAssignmentOperator  = key -> (instance(key, Sequence) and #key === 2
    and(false and isUnaryAssignmentOperator key#0 and instance(key#1, Type) or key#1 === symbol=))
isBinaryAssignmentOperator = key -> (instance(key, Sequence) and #key === 3
    and isUnaryAssignmentOperator key#0 and instance(key#1, Type) and instance(key#2, Type))

thingMethods := (T, F) -> nonnull apply(pairs T, (key, func) -> if instance(func, Function) then
    -- TODO: unary methods are installed as T#f, change it to T#(f, T), then simplify this
    if key === F                                               then (key, T) else -- unary method, e.g quotient
    -- TODO: unary assignments operators are installed as T#(s, symbol=), change it to T#((s, symbol=), T), then simplify this
    if isUnaryAssignmentOperator key and isMember(F,        key) then (key, T) else -- unary assignment method, e.g symbol=
    if instance(key, Sequence)       and isMember(F, splice key) then  key)

sequenceMethods := (T, F, tallyF) -> nonnull apply(pairs T, (key, func) -> if instance(func, Function) then
    if isBinaryAssignmentOperator key and tallyF <= tally splice  key     then  key     else -- e.g T#((symbol SPACE, symbol=), T, T)
    if  isUnaryAssignmentOperator key and tallyF <= tally splice (key, T) then (key, T) else -- e.g T#(symbol+, symbol=)
    if instance(key, Keyword)         and tallyF <= tally splice (key, T) then (key, T) else -- e.g T#(symbol #)
    if instance(key, Function)        and tallyF <= tally splice (key, T) then (key, T) else -- e.g T#resolution
    if instance(key, Sequence)        and tallyF <= tally         key     then  key     else
    if key === NewMethod              and tallyF <= tally splice (key, T) then (key, T))

-- Note: even though HypertextContainer is not an exported type,
-- but (net, HypertextContainer) is callable through `net help()`
-- However, (editMethod, String) is not callable as a method.
isCallable = key -> all(key, e -> instance(e, Type)
    or isBinaryAssignmentOperator e
    or  isUnaryAssignmentOperator e
    or isPackageLoaded toString package' e)

methods = method(Dispatch => Thing, TypicalValue => NumberedVerticalList)
methods Manipulator := M -> methods class M
methods Command  := c -> methods c#0
methods Type     := F -> methods sequence F
methods Sequence := F -> (
    found := new MutableHashTable;
    tallyF := tally splice F;
    searchAllDictionaries(Type, T -> scan(sequenceMethods(T, F, tallyF), key -> found#key = true));
    -- this line makes so `methods parent class help()` shows (net, HypertextContainer)
    -- despite the fact that HypertextContainer is not exported by default.
    scan(select(F, e -> instance(e, Type)), T -> scan(sequenceMethods(T, F, tallyF), key -> found#key = true));
    previousMethodsFound = new NumberedVerticalList from sortByLocation select(keys found, isCallable))

methods ScriptedFunctor := -- TODO: OO and other scripted functors aren't supported
-- FIXME: why is 'methods Format' giving two things?
methods Symbol :=
methods Thing  := F -> (
    if F === HH
    then return previousMethodsFound = join \\ methods \ (homology, cohomology);
    found := new MutableHashTable;
    -- TODO: either finish or remove nullaryMethods
    if nullaryMethods#?(1:F) then found#(1:F) = true;
    searchAllDictionaries(Type, T -> scan(thingMethods(T, F), key -> found#key = true));
    previousMethodsFound = new NumberedVerticalList from sortByLocation keys found)

-- this one is here because it needs previousMethodsFound
options ZZ := i -> options previousMethodsFound#i
locate  ZZ := i -> locate  previousMethodsFound#i

-----------------------------------------------------------------------------
-- hooks
-----------------------------------------------------------------------------

listHooks = (key, opts) -> (
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
	  if c =!= null then print c;
	  )
     else (
	  popvar symbol inDebugger;
	  )
     )

-----------------------------------------------------------------------------
-- disassemble and pseudocode
-----------------------------------------------------------------------------
-- Pseudocode and PseudocodeClosure are types defined in d/classes.dd
-- pseudocode FunctionClosure produces a PseudocodeClosure
-- pseudocode FunctionBody produces the Pseudocode inside (e.g. pseudocode functionBody f)

-- commented because "a pseudocode" doesn't sound so great
-- Pseudocode.synonym = "pseudocode"
-- PseudocodeClosure.synonym = "pseudocode closure"

-- the main way to traverse a Pseudocode or PseudocodeClosure
Pseudocode _ ZZ := (x, i) -> (
    x = last toList x;
    if class x =!= Sequence or not x#?i then error "no such member";
    x = x#i;
    if class x === List then x = last x;
    x)

-- printing helpers
CodeSequence := new Type of VisibleList
html     CodeSequence := x -> html VerticalList x
net      CodeSequence := x -> stack apply(x, net)
toString CodeSequence := x -> demark(" ", apply(x, toString))

CodeList := new Type of VisibleList
html     CodeList := x -> html RowExpression between(" ", x)
net      CodeList := x -> horizontalJoin between(" ", apply(x, net))
toString CodeList := x -> concatenate("(", demark(" ", apply(x, toString)), ")")

fmtCode = method(Dispatch => Thing)
fmtCode Thing      := identity
fmtCode List       := l -> new CodeList     from apply(l, fmtCode)
fmtCode Sequence   := s -> new CodeSequence from apply(s, fmtCode)
fmtCode Pseudocode := fmtCode @@ toList

-- printing
html     Pseudocode := html @@ fmtCode
net      Pseudocode :=  net @@ fmtCode
toString Pseudocode := disassemble

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
