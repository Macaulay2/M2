--		Copyright 1993-2002 by Daniel R. Grayson

olderror := error
erase symbol error
error = args -> olderror (
     -- this is the body of the "error" function, which prints out error messages
     apply(sequence args, x -> if class x === String then x else silentRobustString(40,3,x) )
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
     fn := if opts.Name =!= null then opts.Name else try toString f else "--function--";
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
     while (
	  s1 := concatenate("timing scan(",toString n,", i -> (",s,";null;null))");
	  s2 := concatenate("timing scan(",toString n,", i -> (      null;null))");
	  collectGarbage();
	  value s1;
	  value s2;
	  collectGarbage();
	  value s1;
	  value s2;
	  collectGarbage();
	  t1 := first value s1;
	  t2 := first value s2;
	  t := t1 - t2;
	  t < 1 and t2 < 5)
     do (
	  n = if t > 0.01 then floor(2*n/t+.5) else 100*n;
	  );
     t/n)

-----------------------------------------------------------------------------
Descent := new Type of MutableHashTable
net Descent := x -> stack sort apply(pairs x,
     (k,v) -> (
	  if #v === 0
	  then net k
	  else net k | " : " | net v
	  ))
select1 := syms -> select(apply(syms, value), s -> instance(s, Type))
     
show1 := method(SingleArgumentDispatch => true)
show1 Sequence := show1 List := types -> (
     world := new Descent;
     install := v -> (
	  w := if v === Thing then world else install parent v;
	  if w#?v then w#v else w#v = new Descent
	  );
     scan(types, install);
     net world)
show1 Thing := X -> show1 {X}
showUserStructure = Command(() -> show1 select1 userSymbols())
showStructure = Command(types -> show1 if types === () then select1 flatten(values \ dictionaryPath) else types)

-----------------------------------------------------------------------------

typicalValues#frame = MutableList

symbolLocation = s -> (
     t := locate s;
     if t =!= null then t#0 | ":" | toString t#1| ":" | toString t#2 | "-" | toString t#3| ":" | toString t#4
     else "")

sortByHash = v -> last \ sort \\ (i -> (hash i, i)) \ v

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
-- localSymbols() := () -> if errorCode === null then ls() else ls errorCode
-- meanwhile: (see also method123())
-- nullaryMethods # (1 : localSymbols) = () -> if errorCode =!= null then ls errorCode else error "not in debugger (i.e., errorCode not set)"
-- also meanwhile:
installMethod(localSymbols, () -> if errorCode =!= null then ls errorCode else error "not in debugger (i.e., errorCode not set)")

localSymbols(Type,Symbol) :=
localSymbols(Type,Dictionary) :=
localSymbols(Type,Function) :=
localSymbols(Type,Pseudocode) := (X,f) -> select2(X,localSymbols f)

localSymbols Type := X -> select2(X,localSymbols ())

vbar := (ht,dp) -> " "^(ht-1)				    -- sigh
upWidth := (wid,n) -> n | horizontalJoin(wid - width n : " "^(height n - 1))
joinRow := x -> horizontalJoin mingle(x,#x-1:vbar(max\\height\x,max\\depth\x))
netTable = x -> (
     if not isTable x then error "expected a table";
     if #x == 0 or #x#0 == 0 then return stack();
     colwids := max \ transpose applyTable(x,width);
     x = joinRow \ apply(x, row -> apply(colwids,row,upWidth));
     ( stack x -- stack mingle(x,#x-1:"") -- try it without the blank lines
	  )^(height x#0 -1))
robust := y -> silentRobustNet(55,4,3,y)
abbreviate := x -> (
     if instance(x, Function) and match("^--Function.*--$", toString x) then "..."
     else robust x)
listSymbols = method()
listSymbols Dictionary := d -> listSymbols values d
listSymbols List := x -> (
     netTable prepend(
	  {"symbol" || "------","", "class" || "-----", "", "value" || "-----", "location of symbol" || "------------------"},
	  apply (x, s -> {toString s,":", robust class value s, "--", abbreviate value s, symbolLocation s})))

listLocalSymbols = Command(f -> listSymbols localSymbols f)

userSymbols = type -> (
     if type === () then type = Thing;
     select2(type,values User#"private dictionary"))

listUserSymbols = Command ( type -> listSymbols userSymbols type )

clearOutput = Command (() -> (
	  global oo <- global ooo <- global oooo <- null;
	  scan(values OutputDictionary, s -> ( s <- null; erase s ));
	  ))

clearAll = Command (() -> ( 
	  clearOutput(); 
	  scan(values User#"private dictionary", i -> (i <- i; erase i));
	  )
     )

generateAssertions = method(TypicalValue => Net)
generateAssertions String := s -> generateAssertions select(lines s, x -> not match("^[[:space:]]*(--.*)?$",x))
generateAssertions List := y -> stack apply(y, 
     lin -> ( 
	  t := try value lin else local oops;
	  concatenate if t === local oops
	  then ("assert( (try ", lin, " else oops) === ", toString t, " )")
	  else ("assert( ("    , lin,            ") === ", toString t, " )")
	  ))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
