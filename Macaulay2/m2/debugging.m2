--		Copyright 1993-2002 by Daniel R. Grayson

olderror := error
erase symbol error
error = args -> olderror apply(
     sequence args, x -> if class x === String then x else toString x
     )
protect symbol error

on = { CallLimit => 100000, Name => null } ==> opts -> f -> (
     depth := 0;
     totaltime := 0.;
     callCount := 0;
     limit := opts.CallLimit;
     if class f =!= Function then error("expected a function");
     fn := if opts.Name =!= null then opts.Name else try toString f else string f;
     x -> (
	  saveCallCount := callCount = callCount+1;
     	  << fn << " (" << saveCallCount << ")";
	  if depth > 0 then << " [" << depth << "]";
	  << " called with ";
	  try << class x << " ";
	  try << x else "SOMETHING";
	  << endl;
	  if callCount > limit then error "call limit exceeded";
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

assert = x -> if not x then error "assertion failed"

notImplemented = x -> error "not implemented yet"

benchmark = (s) -> (
     n := 1;
     while (
	  s1 := concatenate("timing scan(",string n,", i -> (",s,";null;null))");
	  s2 := concatenate("timing scan(",string n,", i -> (      null;null))");
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
showStructure = Command(types -> show1 if types === () then select1 flatten(values \ globalDictionaries) else types)

-----------------------------------------------------------------------------

select2 := (type,syms) -> apply(
     sort apply(
	  select(syms, sym -> mutable sym and instance(value sym,type)),
	  symb -> (hash symb, symb)
	  ),
     (h,s) -> s)

userSymbols = type -> (
     if type === () then type = Thing;
     select2(type,values UserDictionary))

list2 := syms -> stack apply(syms, s ->  toString s | ": " | toString class value s)

listUserSymbols = Command ( type -> list2 userSymbols type )

clearOutput = Command (() -> scan(keys Output.Dictionary, s -> ( s <- null; erase s )))

clearAll = Command (() -> ( 
     	  unmarkAllLoadedFiles();
	  clearOutput(); 
	  scan(userSymbols(), i -> i <- i);
	  )
     )

erase symbol unmarkAllLoadedFiles			    -- symbol was created in setup.m2

typicalValues#frame = MutableList

pos := s -> (
     t := locate s;
     if t =!= null then t#0 | ":" | toString t#1)

truncate := s -> (
     narrowed := false;
     if width s > 45
     then (
	  s = stack ( apply( unstack s, l -> substring(l,0,45)));
	  s = s | (stack ( height s + depth s : "|" ))^(height s - 1);
     	  narrowed = true;
	  );
     if height s + depth s  > 4 
     then (
	  s = stack take(unstack s,4);
	  if narrowed
	  then s = s || concatenate(width s - 1 : "-", "+")
	  else s = s || concatenate(width s : "-");
	  );
     s)

sortByHash := v -> last \ sort (v / (i -> (hash i, i)))

abbreviate := x -> (
     if class x === Function and match("^--Function.*--$", toString x) then "..."
     else x)

listLocalVariables = Command(
     x -> (
	  if breakLoopFrame === null then error "no break loop active";
	  Table (apply ( reverse flatten (sortByHash \ values \ localDictionaries breakLoopFrame) , s -> {s,":",net class value s, "=", truncate net abbreviate value s, pos s}))))

