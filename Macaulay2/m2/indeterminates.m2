--		Copyright 1993-1999 by Daniel R. Grayson

indeterminates :=  new MutableHashTable

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

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
