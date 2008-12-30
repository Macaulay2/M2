--		Copyright 1993-2002 by Daniel R. Grayson

olderror := error
error = args -> olderror (
     -- this is the body of the "error" function, which prints out error messages
     args = sequence args;
     apply(args, x -> 
	  if class x === String then x
	  else if class x === Symbol then ("'", toString x, "'")
	  else silentRobustString(40,3,x)
	  ),
     apply(args, x -> if class x === Symbol then ("\n", symbolLocation x, ": here is the first use of '",toString x, "'") else "")
     )
protect symbol error

callCount := new MutableHashTable

on = { CallLimit => 100000, Name => null } >> opts -> f -> (
     fb := functionBody f;
     depth := 0;
     totaltime := 0.;
     if not callCount#?fb then callCount#fb = 0;
     limit := opts.CallLimit;
     if not instance(f, Function) then error("expected a function");
     fn := if opts.Name =!= null then opts.Name else try toString f else "{*function*}";
     x -> (
	  saveCallCount := callCount#fb = callCount#fb+1;
     	  << fn << " (" << saveCallCount << ")";
	  if depth > 0 then << " [" << depth << "]";
	  << " called with ";
	  try << class x << " ";
	  try << x else "SOMETHING";
	  << endl;
	  if callCount#fb > limit then error "call limit exceeded";
	  depth = depth + 1;
     	  r := timing f x;
	  timeused := r#0;
	  value := r#1;
	  depth = depth - 1;
	  if depth === 0 then totaltime = totaltime + timeused;
     	  << fn << " (" << saveCallCount << ")";
	  if depth > 0 then << " [" << depth << "]";
	  if timeused > 1. then << " used " << timeused << " seconds";
	  if totaltime > 1. and depth === 0 then << " (total " << totaltime << " seconds)";
	  << " returned " << class value << " " << value << endl;
     	  value)
     )

notImplemented = x -> error "not implemented yet"

benchmark = (s) -> (
     n := 1;
     local firsttime;
     while (
	  s1 := concatenate("timing scan(",toString n,", iBenchmark -> (",s,";null;null))");
	  s2 := concatenate("timing scan(",toString n,", iBenchmark -> (      null;null))");
	  collectGarbage();
	  t1 := first value s1;
	  t2 := first value s2;
	  t := t1 - t2;
	  if n === 1 then firsttime = t;
	  collectGarbage();
	  -- the second timing result is better
	  value s1;
	  value s2;
	  collectGarbage();
	  t1 = first value s1;
	  t2 = first value s2;
	  t = t1 - t2;
	  t < 1 and t2 < 5)
     do (
	  n = if t > 0.01 then floor(2*n/t+.5) else 100*n;
	  );
     t = t/n;
     if firsttime > 3*t then (
	  stderr << "--warning: code being benchmarked ran longer the first time (" << firsttime << " seconds)" << endl;
	  stderr << "--         perhaps results were cached" << endl;
	  );
     t)

-----------------------------------------------------------------------------
Descent = new Type of MutableHashTable
net Descent := x -> stack sort apply(pairs x,
     (k,v) -> (
	  if #v === 0
	  then net k
	  else net k | " : " | net v
	  ))
justTypes := syms -> select(apply(syms, value), s -> instance(s, Type))
allThingsWithNames := syms -> select(apply(syms, value), s -> hasAttribute(s,ReverseDictionary))
     
show1 := method()
show1(Sequence,Function) := show1(List,Function) := (types,pfun) -> (
     world := new Descent;
     install := v -> (
	  w := (
	       if pfun === parent and v === Thing
	       or pfun === class and v === Type
	       then world
	       else install pfun v);
	  if hasAttribute(v,PrintNet) then v = getAttribute(v,PrintNet) else
	  if hasAttribute(v,PrintNames) then v = getAttribute(v,PrintNames) else
	  if hasAttribute(v,ReverseDictionary) then v = getAttribute(v,ReverseDictionary);
	  if w#?v then w#v else w#v = new Descent
	  );
     scan(types, install);
     world)
show1(Thing,Function) := (X,pfun) -> show1({X},pfun)
showUserStructure = Command(() -> show1(justTypes userSymbols(), parent))
allValues = () -> unique join(flatten(values \ dictionaryPath), select(getAttributes ReverseDictionary,
	  s -> (
	       n := toString s;
	       isGlobalSymbol n and s === getGlobalSymbol n
	       )
	  ))
showStructure = Command(types -> show1(if types === () then justTypes allValues() else types, parent))
showClassStructure = Command(types -> show1(if types === () then allThingsWithNames allValues() else types, class))
ancestors = X -> while true list (local Z; if Z === Thing then break ; Z = X; X = parent X; Z)
-----------------------------------------------------------------------------

typicalValues#frame = MutableList

symbolLocation = s -> (
     t := locate s;
     if t =!= null then t#0 | ":" | toString t#1| ":" | toString (t#2+1) | "-" | toString t#3| ":" | toString (t#4+1)
     else "")

select2 := (type,syms) -> apply(
     sort apply(
	  select(syms, sym -> mutable sym and instance(value sym,type)),
	  symb -> (hash symb, symb)
	  ),
     (h,s) -> s)

ls := f -> flatten \\ sortByHash \ values \ localDictionaries f
localSymbols = method()
localSymbols Pseudocode :=
localSymbols Symbol :=
localSymbols Dictionary :=
localSymbols Function := ls

-- make this work eventually:
-- localSymbols() := () -> if current === null then ls() else ls current
-- meanwhile: (see also method123())
-- nullaryMethods # (1 : localSymbols) = () -> if current =!= null then ls current else error "not in debugger (i.e., current not set)"
-- also meanwhile:
installMethod(localSymbols, () -> if current =!= null then ls current else error "not in debugger (i.e., current not set)")

localSymbols(Type,Symbol) :=
localSymbols(Type,Dictionary) :=
localSymbols(Type,Function) :=
localSymbols(Type,Pseudocode) := (X,f) -> select2(X,localSymbols f)

localSymbols Type := X -> select2(X,localSymbols ())

robust := y -> silentRobustNet(55,4,3,y)
abbreviate := x -> (
     if instance(x, Function) and match("^--Function.*--$", toString x) then "..."
     else robust x)
listSymbols = method()
listSymbols Dictionary := d -> listSymbols values d
listSymbols List := x -> (
     netList(Boxes=>false, HorizontalSpace => 1, prepend(
	  {"symbol" || "------","", "class" || "-----", "", "value" || "-----", "location of symbol" || "------------------"},
	  apply (x, s -> {toString s,":", robust class value s, "--", abbreviate value s, symbolLocation s}))))

listLocalSymbols = Command(f -> listSymbols localSymbols f)

userSymbols = type -> (
     if type === () then type = Thing;
     select2(type,join(values User#"private dictionary",toList select(vars (0 .. 51), s -> value s =!= s))))

listUserSymbols = Command ( type -> listSymbols userSymbols type )

clearOutput = Command (() -> scan(join({global oo, global ooo, global oooo}, values OutputDictionary), s -> s <- s ))
clearAll = Command (() -> ( 
	  clearOutput(); 
	  scan(getGlobalSymbol "a" .. getGlobalSymbol "Z", i -> i <- i);
	  scan(values User#"private dictionary", i -> erase(i <- i));
	  ))

generateAssertions = method(TypicalValue => Net)
generateAssertions String := s -> generateAssertions select(lines s, x -> not match("^[[:space:]]*(--.*)?$",x))
generateAssertions List := y -> stack apply(y, 
     lin -> ( 
	  t := try value lin else local oops;
	  concatenate if t === null then lin
	  else if t === local oops
	  then ("assert( (try ", lin, " else oops) === oops )")
	  else ("assert( ("    , lin,            ") === ", toExternalString t, " )")
	  ))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
