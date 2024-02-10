--		Copyright 1993-1999 by Daniel R. Grayson

needs "expressions.m2" -- for precedence
needs "hypertext.m2"
needs "methods.m2"

peek' = method(TypicalValue => Net)

peek'(ZZ,ZZ) := (depth,n) -> toString n
peek'(ZZ,Nothing) := (depth,s) -> "null"
peek'(ZZ,Symbol) := (depth,s) -> toString s
peek'(ZZ,Thing) := (depth,s) -> net s

peek'(ZZ,BasicList) := (depth,s) -> (
     if depth === 0 then net s
     else horizontalJoin(
	  net class s,
	  "{", horizontalJoin between (", ", apply(toList s, value -> peek'(depth-1,value))), "}" ) )

peek'(ZZ,HypertextParagraph) := peek'(ZZ,Hypertext) := (depth,s) -> (
     if depth === 0 then net s
     else horizontalJoin(
	  net class s,
	  "{", horizontalJoin between (", ", apply(toList s, value -> peek'(if instance(value,Hypertext) or instance(value,String) then depth else depth-1, value))), "}" ) )

peek'(ZZ,List) := (depth,s) -> (
     if depth === 0 then net s
     else horizontalJoin(
	  "{", horizontalJoin between (", ", apply(s, value -> peek'(depth,value))), "}" ) )
peek'(ZZ, String) := (depth,s) -> if depth === 0 then s else format s

formatNet := n -> (stack ((s -> substring(s,1,#s-2)) \ format \ unstack n))^(height n - 1)
peek'(ZZ,Net) := (depth,s) -> if depth === 0 then s else netList({{formatNet s}}, Boxes => true)
peek'(ZZ,Sequence) := (depth,s) -> (
     if depth === 0 then net s
     else horizontalJoin(
	  if #s === 0
	  then "()"
	  else if #s === 1 
	  then ("1 : (", peek'(depth,s#0), ")")
	  else ("(", horizontalJoin between (", ", apply(s, value -> peek'(depth,value))), ")" )))

precOption := precedence ( 1 => 2 )

peek'(ZZ,HashTable) := (depth,s) -> (
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
		    then ("(",peek'(depth-1,key),")")
		    else peek'(depth-1,key),
		    " => ",
		    if precedence value < precOption
		    then ("(",peek'(depth-1,value),")")
		    else peek'(depth-1,value))),
	  "}"
	  ))

peek'(ZZ,Dictionary) := (depth,d) -> (
     if depth === 0 then net d
     else horizontalJoin(
	  toString class d, "{", 
	  stack apply(sort pairs d, (lhs,rhs) -> horizontalJoin splice (peek lhs," => ",peek'(depth-1,rhs))),
	  "}"))

peek = s -> peek'(1,s)
typicalValues#peek = Net

ops = new MutableHashTable

seeParsing = args -> (					    -- let's eliminate this in favor of operatorAttributes
     x := new MutableHashTable;
     t := (p,s) -> (
	  if x#?p then x#p = append(x#p,s) else x#p = {s};
	  ops#s = true;
	  );
     q := getParsing symbol apply;
     scan(keys set join(values Core.Dictionary, values Core#"private dictionary"), s -> if getParsing s =!= q then t(getParsing s,s));
     t(getParsing symbol apply, "-*...symbols...*-");
     new Table from prepend(
	  { "parsing\nprecedence", "binary\nbinding\nstrength","unary\nbinding\nstrength", "\noperators" },
 	  sort pairs x / (
	       (a,b) -> append(a/(i -> if i === -1 then "" else toString i),
		    concatenate(
			 between("  ",
			      sort(b/toString)))))))

seeOperatorPrecedence = args -> (
     x := new MutableHashTable;
     t := (p,s) -> (
	  if x#?p then x#p = append(x#p,s) else x#p = {s};
	  ops#s = true;
	  );
     scan(allOperators, s -> t(first getParsing s,s));
     new Table from sort pairs x / ( (a,b) -> {toString a, concatenate( between_"  " sort(b/toString))}))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
