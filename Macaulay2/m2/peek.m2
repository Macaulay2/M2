--		Copyright 1994 by Daniel R. Grayson

peek2 := method()

peek2(Symbol,ZZ) := (s,depth) -> if depth === 0 then string s else name s
peek2(Thing,ZZ) := (s,depth) -> net s
peek2(BasicList,ZZ) := (s,depth) -> (
     if depth === 0 then net s
     else horizontalJoin(
	  if class s =!= List then net class s else "",
	  "{",
	  horizontalJoin between (",",
	       apply(s,
	       	    value -> peek2(value,depth-1))),
	  "}"
	  )
     )
peek2(String, ZZ) := (s,depth) -> if depth === 0 then s else format s

boxNet := x -> ( 
     s := concatenate("+", width x : "-", "+");
     n := height x + depth x;
     if n === 0 then (
     	  (s || x || s) ^ (height x)
	  )
     else (
     	  t := (verticalJoin (n : "|")) ^ (height x - 1);
     	  (s || t | x | t || s) ^ (height x)
	  )
     )

peek2(Net, ZZ) := (s,depth) -> if depth === 0 then s else boxNet s

peek2(Sequence,ZZ) := (s,depth) -> (
     if depth === 0 then net s
     else horizontalJoin(
	  if #s === 0 then "()"
	  else if #s === 1 then "seq (" 
	  else (
	       "(",
	       horizontalJoin between (",", 
	       	    apply(s,
	       	    	 value -> peek2(value,depth))),
	       ")"
	       )))

precOption := precedence ( 1 => 2 )

peek2(HashTable,ZZ) := (s,depth) -> (
     if depth === 0 
     then net s
     else horizontalJoin(
	  net class s,
	  if parent s =!= Nothing 
	  then horizontalJoin(" of ", net parent s)
	  else "",
	  "{",
	  verticalJoin sort apply(
	       pairs s,
	       (key,value) -> horizontalJoin splice (
		    if precedence key < precOption
		    then ("(",peek2(key,depth-1),")")
		    else peek2(key,depth-1),
		    " => ",
		    if precedence value < precOption
		    then ("(",peek2(value,depth-1),")")
		    else peek2(value,depth-1))),
	  "}"
	  ))

peek = method()

peek(Thing,ZZ) := (s,depth) -> peek2(s,depth)

peek(Thing) := s -> peek(s,1)

document {
     quote peek,
     TT "peek s", " -- displays contents of s, bypassing installed methods.",
     BR,
     NOINDENT, 
     TT "peek (s,n)", " -- displays contents of s to depth n, bypassing installed 
     methods.",
     PARA,
     "It applies the default output method to the object s,
     bypassing the installed method for objects of its class.",
     EXAMPLE {
	  "t = set {1,2,3}",
      	  "peek t",
      	  "new MutableHashTable from {a=>3, b=>44}",
      	  "peek oo"
	  }
     }

