--		Copyright 1993-1999 by Daniel R. Grayson

indeterminates =  new MutableHashTable

varIndices := new MutableHashTable

varName := i -> (
     if 0 <= i and i < 26 then ascii(97 + i)
     else if 26 <= i and i < 52 then ascii(65 + i - 26)
     else if i < 0
     then "X" | toString(-i)
     else "x" | toString(i-52))

vars ZZ := i -> (
     if indeterminates#?i then indeterminates#i else (
	  x := getSymbol varName i;
	  indeterminates#i = x;
	  varIndices#x = i;
	  x))

vars List := vars Sequence := args -> apply(flatten splice args, j -> vars j)

isUserSymbol := (s,a) -> User#"private dictionary"#?s and User#"private dictionary"#s === a

reverseVars := a -> (
     if varIndices#?a then varIndices#a
     else (
	  s := toString a;
	  if isUserSymbol(s,a) and match("^[a-zA-Z]$",s) then (
	       first ascii s + if match("^[A-Z]$",s) then 26 - first ascii "A" else - first ascii "a"
	       )
     	  else error(a, " is not one of the symbols known to 'vars'")
     	  )
     )

Symbol .. Symbol := (a,z) -> if a === z then 1:a else vars( reverseVars a .. reverseVars z )
Symbol ..< Symbol := (a,z) -> if a === z then () else vars( reverseVars a ..< reverseVars z )

succS = new MutableHashTable;
for i from 0 to 50 do succS#(varName i) = varName(i+1)
succ = method()
succ(ZZ,ZZ) := (x,y) -> x+1 === y
succ(Symbol,Symbol) := (x,y) -> (
     (s,t) := (toString x, toString y);
     isUserSymbol(s,x) and isUserSymbol(t,y) and succS#?s and succS#s === t)
succ(IndexedVariable,IndexedVariable) := (x,y) -> x#0 === y#0 and succ(x#1,y#1)
succ(Thing,Thing) := x -> false
runLengthEncode = method(Dispatch => Thing)
runLengthEncode VisibleList := x -> (
     if #x === 0 then return x;
     dupout := true;
     while first(dupout,dupout = false) do x = new class x from (
	  i0 := null;
	  lastout := oi := symbol oi;
	  m := 0;
	  dupin := null;
	  for i in append(x,symbol x) list 
	  (o -> (if lastout === o then dupout = true else lastout = o; o))(
	       if i === oi and dupin =!= false then (dupin = true; m = m+1; continue)
	       else if succ(oi,i) and dupin =!= true then (
		    if dupin === null then i0 = oi;
		    dupin = false; 
		    oi = i;
		    m = m+1; 
		    continue)
	       else first(
		    if oi === symbol oi then (oi = i; m = 1 ; continue) else
		    if m === 1 then hold oi else if dupin === true then hold m : hold oi else hold i0 .. hold oi,
		    (dupin = null; oi = i; m = 1))));
     x)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
