--		Copyright 1994 by Daniel R. Grayson

indeterminates :=  {
     quote a,quote b,quote c,quote d,quote e,quote f,quote g,quote h,
     quote i,quote j,quote k,quote l,quote m,quote n,quote o,quote p,
     quote q,quote r,quote s,quote t,quote u,quote v,quote w,quote x,
     quote y,quote z,
     quote A,quote B,quote C,quote D,quote E,quote F,quote G,quote H,
     quote I,quote J,quote K,quote L,quote M,quote N,quote O,quote P,
     quote Q,quote R,quote S,quote T,quote U,quote V,quote W,quote X,
     quote Y,quote Z
     }

Index := new MutableHashTable
scan(#indeterminates, i -> Index#(indeterminates#i) = i )
vars ZZ := i -> indeterminates#i
vars List := vars Sequence := args -> apply(flatten args,j->indeterminates#j)
Symbol .. Symbol := (a,z) -> (
     vars(
	  (if Index#?a then Index#a else error("not an indeterminate: ", name a))
	  .. 
	  (if Index#?z then Index#z else error("not an indeterminate: ", name z))
	  )
     )

nometh2 := (n,x,y) -> error (
     "no method '", name n, "' found for ", name x,
     " (of class ", name class x, ") and ", name y,
     " (of class ", name class y, ")"
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

document { quote "..",
     TT "m .. n", " -- produces a sequence of integers in the range from m to 
     n inclusive. If n is less than m then the result is an empty sequence.",
     PARA,
     "The most confusing thing about this operator is that it is not a syntactic
     construction, and so the resulting sequences do not splice themselves into
     enclosing lists.  See ", TO "splice", " for that.",
     PARA,
     EXAMPLE "1..5",
     EXAMPLE "{1..5}",
     EXAMPLE "toList(1..5)",
     EXAMPLE "{10..10}",
     EXAMPLE "{10..8}",
     EXAMPLE "{3..5,8..10}",
     EXAMPLE "splice {3..5,8..10}",
     PARA,
     NOINDENT,
     TT "a .. i", " -- produces a sequence of ", TO "indeterminates", " for use as 
     variables in polynomial rings.",
     EXAMPLE "a .. i",
     PARA,
     TT "x_0 .. x_9", " -- produces a sequence of indexed variables for use in
     polynomial rings.",
     EXAMPLE "x_0 .. x_9",
     EXAMPLE "x_(t_0) .. x_(t_5)",
     EXAMPLE "x_a .. x_e",
     PARA,
     NOINDENT,
     "Note: can be used with sequences or lists to produce rectangular intervals.",
     EXAMPLE "(0,0)..(1,3)",
     EXAMPLE "p_(0,a) .. p_(1,c)",
     PARA,
     SEEALSO{ "lists, arrays, and sequences"}
     }

document { "indeterminates",
     "Certain symbols are preferred for use as variables in polynomial rings.
     These are the symbols whose name consists of a single letter that are not
     used for some other purpose.  The single-letter symbols that have another
     use are : ", 
     concatenate 
     ((s) -> mingle(s,append(#s-2 : ", ", ", and ")))
     toList (
	  set characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
	  - set apply(indeterminates, name)
	  ),
     ".",
     PARA,
     SEEALSO {"vars", ".."}     
     }
