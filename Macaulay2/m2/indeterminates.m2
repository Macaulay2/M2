--		Copyright 1993-1999 by Daniel R. Grayson

indeterminates =  new MutableHashTable

indices := new MutableHashTable

vars ZZ := i -> (
     if indeterminates#?i then indeterminates#i else (
	  x := (
	       if 0 <= i and i < 26 then getGlobalSymbol ascii(97 + i)
	       else if 26 <= i and i < 52 then getGlobalSymbol ascii(65 + i - 26)
	       else 
	       value (
		    if i < 0 then "global X" | toString(-i)
	       	    else "global x" | toString(i-52)
		    )
	       );
	  indeterminates#i = x;
	  indices#x = i;
	  x))

vars List := vars Sequence := args -> apply(flatten splice args, j -> vars j)

reverseVars := a -> (
     if indices#?a 
     then indices#a
     else (
	  s := toString a;
	  if #s === 1 and s =!= "'" then (
	       i := first ascii s;
	       if i < 97 then i - 65 + 26 else i - 97
	       )
     	  else error(s, " is not one of the symbols known to 'vars'")
     	  )
     )

Symbol .. Symbol := (a,z) -> vars( reverseVars a .. reverseVars z )

nometh2 := (n,x,y) -> error (
     "no method '", toString n, "' found for ", toString x,
     " (of class ", toString class x, ") and ", toString y,
     " (of class ", toString class y, ")"
     )

Thing .. Thing := (a,z) -> (
     aa := null;
     zz := null;
     try (
	  aa = baseName a;
	  zz = baseName z;
	  )
     else nometh2(symbol .., a,z);
     if a === aa and z === zz then nometh2(symbol .., a,z);
     if instance(aa,Symbol) and instance(zz,Symbol)
     or instance(aa,IndexedVariable) and instance(zz,IndexedVariable)
     then value \ (aa .. zz)
     else aa .. zz
     )


succS = new MutableHashTable;
for i from 0 to 50 do succS#(vars i) = vars(i+1)
succ = method()
succ(ZZ,ZZ) := (x,y) -> x+1 === y
succ(Symbol,Symbol) := (x,y) -> succS#?x and succS#x === y
succ(IndexedVariable,IndexedVariable) := (x,y) -> x#0 === y#0 and succ(x#1,y#1)
succ(Thing,Thing) := x -> false
RLE = method(Dispatch => Thing)
RLE VisibleList := x -> (
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
