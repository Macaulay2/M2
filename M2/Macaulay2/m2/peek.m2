--		Copyright 1993-1999 by Daniel R. Grayson

peek2 = method(TypicalValue => Net)

peek2(Nothing,ZZ) := (s,depth) -> "null"
peek2(Symbol,ZZ) := (s,depth) -> if depth === 0 then toString s else toExternalString s
peek2(Thing,ZZ) := (s,depth) -> net s
peek2(BasicList,ZZ) := (s,depth) -> (
     if depth === 0 then net s
     else horizontalJoin(
	  net class s,
	  "{", horizontalJoin between (",", apply(toList s, value -> peek2(value,depth-1))), "}" ) )
peek2(List,ZZ) := (s,depth) -> (
     if depth === 0 then net s
     else horizontalJoin(
	  "{", horizontalJoin between (",", apply(s, value -> peek2(value,depth))), "}" ) )
peek2(String, ZZ) := (s,depth) -> if depth === 0 then s else format s

boxNet := x -> ( 
     s := concatenate("+", width x : "-", "+");
     n := height x + depth x;
     if n === 0 then (
     	  (s || x || s) ^ (height x)
	  )
     else (
     	  t := (stack (n : "|")) ^ (height x - 1);
     	  (s || t | x | t || s) ^ (height x)
	  )
     )

peek2(Net, ZZ) := (s,depth) -> if depth === 0 then s else boxNet s

peek2(Sequence,ZZ) := (s,depth) -> (
     if depth === 0 then net s
     else horizontalJoin(
	  if #s === 0
	  then "()"
	  else if #s === 1 
	  then ("singleton (", peek2(s#0,depth), ")")
	  else ("(", horizontalJoin between (",", apply(s, value -> peek2(value,depth))), ")" )))

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
	  stack sort apply(
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

peek = s -> peek2(s,1)
typicalValues#peek = Net

seeParsing = args -> (
     x := new MutableHashTable;
     t := (p,s) -> if x#?p then x#p = append(x#p,s) else x#p = {s};
     q := getParsing symbol seeParsing;
     scan(values globalDictionary(), s -> if getParsing s =!= q and s =!= symbol " " then t(getParsing s,s));
     t(getParsing symbol args, "<SYMBOLS>"  );
     t(getParsing symbol " ", "<ADJACENCY>");
     new Table from prepend(
	  { "parsing\nprecedence", "binary\nbinding\nstrength","unary\nbinding\nstrength", "\noperators" },
 	  sort pairs x / (
	       (a,b) -> append(a/(i -> if i === -1 then "" else toString i),
		    concatenate(
			 between("  ",
			      sort(b/toString)))))))
seeParsing = new Command from seeParsing
