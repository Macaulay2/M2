--		Copyright 1994 by Daniel R. Grayson

indeterminates :=  new MutableHashTable

indices := new MutableHashTable

vars Symbol := a -> (
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

vars ZZ := i -> (
     if indeterminates#?i then indeterminates#i else (
	  x := value (
	       "global " |
	       if 0 <= i and i < 26 then ascii(97 + i)
	       else if 26 <= i and i < 52 then ascii(65 + i - 26)
	       else if i < 0 then "X" | toString(-i)
	       else "x" | toString(i-52)
	       );
	  indeterminates#i = x;
	  indices#x = i;
	  x))

vars List := vars Sequence := args -> apply(flatten splice args, j -> vars j)

Symbol .. Symbol := (a,z) -> vars( vars a .. vars z )

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
     else nometh2(quote .., a,z);
     if a === aa and z === zz then nometh2(quote .., a,z);
     toSequence apply(aa .. zz, value)
     )

document { "..",
     TT "m .. n", " -- produces a sequence of integers in the range from m to 
     n inclusive. If n is less than m then the result is an empty sequence.",
     PARA,
     "The most confusing thing about this operator is that it is not a syntactic
     construction, and so the resulting sequences do not splice themselves into
     enclosing lists.  See ", TO "splice", " for that.",
     PARA,
     EXAMPLE {
	  "1..5",
      	  "{1..5}",
      	  "toList(1..5)",
      	  "{10..10}",
      	  "{10..8}",
      	  "{3..5,8..10}",
      	  "splice {3..5,8..10}",
	  },
     PARA,
     NOINDENT,
     TT "a .. i", " -- produces a sequence of symbols for use as 
     variables in polynomial rings.",
     EXAMPLE "a .. i",
     PARA,
     TT "x_0 .. x_9", " -- produces a sequence of indexed variables for use in
     polynomial rings.",
     EXAMPLE {
	  "x_0 .. x_9",
      	  "x_(t_0) .. x_(t_5)",
      	  "x_a .. x_e",
	  },
     PARA,
     NOINDENT,
     "Note: can be used with sequences or lists to produce rectangular intervals.",
     EXAMPLE {
	  "(0,0)..(1,3)",
      	  "p_(0,a) .. p_(1,c)",
	  },
     }

