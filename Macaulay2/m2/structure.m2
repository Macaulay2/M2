--		Copyright 1994 by Daniel R. Grayson

document { quote pack,
     TT "pack(v,n)", " -- packs the elements of the list or sequence
     ", TT "v", " into a table ", TT "n", " at a time.",
     PARA,
     "It produces, from a list ", TT "v", ", a list of lists formed by packing the 
     elements of ", TT "v", " into lists ", TT "n", " at a time.  The last of the lists
     produced may have fewer than ", TT "n", " elements."
     }

document { quote join,
     TT "join(u,v,...)", " -- joins the elements of the lists or
     sequences u, v, ... into a single list.",
     PARA,
     "The class of the result is the same as the class of the first argument.
     If there is just one argument, and it's mutable, a copy is returned.",
     EXAMPLE "join({1,2,3},{7,8,9})",
     PARA,
     "The operator ", TO (quote |, List, List), " can be used as a synonym."
     }

document { quote take,
     TT "take(v,n)    ", " -- yields a list containing the first n elements of the list v.",
     BR,NOINDENT,
     TT "take(v,-n)", "    -- yields a list containing the last n elements of the list v.",
     BR,NOINDENT,
     TT "take(v,{m,n})", " -- yields a list containing the elements of the list v 
     in positions m through n.",
     PARA,
     SEEALSO "drop"
     }

first = x -> x#0

document { quote first,
     TT "first v", " -- yields the first element of the list v.",
     PARA,
     "See also ", TO "last", "."
     }

last = x -> x#-1

document { quote last,
     TT "last v", " -- yields the last element of the list v.",
     PARA,
     "See also ", TO "first", "."
     }

positions = (v,f) -> (
     apply(select(pack(mingle{v,0 .. #v-1},2), p -> f p#0), p -> p#1)
     )

document { quote positions,
     TT "positions(v,f)", " -- yields a list of integers giving the positions of the
     elements of the list v which yield the value true when
     the function f is applied."
     }

position = (v,f) -> (
     ret := null;
     select(
	  1, 
	  apply(#v, i -> (i,v#i)), 
	  (i,x) -> f(x) and (ret = i; true));
     ret)

TEST "
assert( 3 === position({a,b,c,d,e,f},i->i===d ) )
"

document { quote position,
     TT "position(v,f)", " -- returns the index of the first element of v satisfying 
     the condition f, or null if there is none."
     }

delete = (x,v) -> select(v, i -> i != x)

document { quote delete,
     TT "delete(x,v)", " -- removes any occurrences of the expression ", TT "x", "
     from the list ", TT "v", ".",
     PARA,
     SEEALSO "member"
     }

accumulate = (f,x,v) -> prepend(x,apply(v, y -> (x = f(x,y))))

document { quote accumulate,
     TT "accumulate(f,x0,{x1,...,xn})", " -- computes the list 
     ", TT "{x0,f(x0,x1),f(f(x0,x1),x2),f(f(f(x0,x1),x2),x3),...}", ".",
     PARA,
     EXAMPLE "accumulate(plus,1,{10,100,1000})"
     }
