--		Copyright 1993-2002 by Daniel R. Grayson

symbolLocation = s -> (
     t := locate s;
     if t =!= null then t#0 | ":" | toString t#1| ":" | toString (t#2+1) | "-" | toString t#3| ":" | toString (t#4+1)
     else "")

processArgs := args -> concatenate (
     args = sequence args;
     apply(args, x -> 
	  if class x === String then x
	  else if class x === Symbol then ("'", toString x, "'")
	  else silentRobustString(40,3,x)
	  ),
     apply(args, x -> if class x === Symbol then ("\n", symbolLocation x, ": here is the first use of '",toString x, "'") else "")
     )
olderror := error
error = args -> (
     -- this is the body of the "error" function, which prints out error messages
     olderror processArgs args)
protect symbol error

warningMessage0 = (args,deb) -> (
     args = processArgs args;
     h := hash args % 10000;
     if debugWarningHashcode === h
     then error args
     else (
	  stderr << "warning: " << args << endl;
     	  if deb then stderr << "       : debug with expression   debug " << h << "   or with command line option   --debug " << h << endl;
	  );
     )
warningMessageNoDebug = args -> warningMessage0(args,false)
warningMessage = args -> warningMessage0(args,true)

callCount := new MutableHashTable

onprint = n -> (
     n = horizontalJoin apply(nonnull deepSplice n,net);
     << (stack( height n + depth n : "-- " ))^(height n - 1) | n << endl;
     )

on = { CallLimit => 100000, Name => null, GenerateAssertions => false } >> opts -> f -> (
     fb := functionBody f;
     calldepth := 0;
     totaltime := 0.;
     if not callCount#?fb then callCount#fb = 0;
     limit := opts.CallLimit;
     if not instance(f, Function) then error("expected a function");
     fn := if opts.Name =!= null then opts.Name else try toString f else "-*function*-";
     x -> (
	  saveCallCount := callCount#fb = callCount#fb+1;
	  onprint(fn, " (", saveCallCount, ")",
 	       if calldepth > 0 then (" [", calldepth, "]"),
	       " called with ",
	       try (net class x, ": "),
	       try net x else "SOMETHING");
	  if callCount#fb > limit then error "call limit exceeded";
	  calldepth = calldepth + 1;
     	  r := timing f x;
	  timeused := r#0;
	  val := r#1;
	  calldepth = calldepth - 1;
	  if calldepth === 0 then totaltime = totaltime + timeused;
	  onprint(fn, " (", saveCallCount, ")",
	       if calldepth > 0 then (" [", calldepth, "]"),
	       if timeused > 1. then (" used ", timeused, " seconds"),
	       if totaltime > 1. and calldepth === 0 then (" (total ", totaltime, " seconds)"),
	       " returned ", try (class val, ": "), try net val else "SOMETHING");
	  if opts.GenerateAssertions then (
	       try printx := toExternalString x then
	       try printval := toExternalString val then
	       << "assert( " << fn << "(" << printx << ") === (" << printval << "))" << endl
	       else null
	       else null
	       );
	  if instance(val,Function) then val = on(val,opts);
     	  val)
     )

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
texMath Descent := x -> "\\left|\\begin{array}{l}" | concatenate sort apply(pairs x,
     (k,v) -> (
	  if #v === 0
	  then texMath net k -- sucks but no choice
	  else texMath net k | " : " | texMath v
	  ) | "\\\\") | "\\end{array}\\right."
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
--	  if hasAttribute(v,PrintNet) then v = getAttribute(v,PrintNet) else
--	  if hasAttribute(v,PrintNames) then v = getAttribute(v,PrintNames) else
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

select2 := (type,syms) -> apply(
     sort apply(
	  select(syms, sym -> mutable sym and instance(value sym,type) and value sym =!= sym),
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
     if instance(x, Function) and match("^-\\*Function.*\\*-$", toString x) then "..."
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
     select2(type,values User#"private dictionary"))

listUserSymbols = Command ( type -> listSymbols userSymbols type )

clearOutput = Command (() -> scan(join({global oo, global ooo, global oooo}, values OutputDictionary), s -> s <- s ))
clearAll = Command (() -> ( 
	  clearOutput(); 
	  scan(values User#"private dictionary", X -> globalAssign(X,X));
	  ))

generateAssertions = method(TypicalValue => Net)
generateAssertions String := s -> generateAssertions select(lines s, x -> not match("^[[:space:]]*(--.*)?$",x))
generateAssertions List := y -> (
     nogens := {PolynomialRing, QuotientRing,Function};
     good := t -> (
	  not mutable t
	  and
	  all(nogens, X -> not instance(t,X))
	  );
     stack apply(y, 
	  lin -> ( 
	       t := try value lin else local oops;
	       concatenate if t === local oops
	       then ("assert( (try ", lin, " else oops) === oops );")
	       else if good t
	       then (
		    ts := try toExternalString t;
		    if ts === null
		    then (
			 tn := try net t;
			 if tn === null
			 then (lin, " -- toExternalString and net fail")
			 else ("assert( net ("    , lin,            ") === ", toExternalString tn, " ); -- toExternalString fails")
			 )
		    else ("assert( ("    , lin,            ") === ", ts, " );")
		    )
	       else lin
	       )))^-1

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
