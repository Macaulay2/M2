--		Copyright 1993-1999 by Daniel R. Grayson

peek2 = method(TypicalValue => Net)

peek2(ZZ,ZZ) := (depth,n) -> toString n
peek2(ZZ,Nothing) := (depth,s) -> "null"
peek2(ZZ,Symbol) := (depth,s) -> if depth === 0 then toString s else toExternalString s
peek2(ZZ,Thing) := (depth,s) -> net s

peek2(ZZ,BasicList) := (depth,s) -> (
     if depth === 0 then net s
     else horizontalJoin(
	  net class s,
	  "{", horizontalJoin between (",", apply(toList s, value -> peek2(depth-1,value))), "}" ) )

peek2(ZZ,MarkUpList) := (depth,s) -> (
     if depth === 0 then net s
     else horizontalJoin(
	  net class s,
	  "{", horizontalJoin between (",", apply(toList s, value -> peek2(if instance(value,MarkUpList) or instance(value,String) then depth else depth-1, value))), "}" ) )

peek2(ZZ,List) := (depth,s) -> (
     if depth === 0 then net s
     else horizontalJoin(
	  "{", horizontalJoin between (",", apply(s, value -> peek2(depth,value))), "}" ) )
peek2(ZZ, String) := (depth,s) -> if depth === 0 then s else format s

boxNets = method(SingleArgumentDispatch => true)
boxNets List := boxNets Sequence := nets -> (
     nets = net \ nets;
     wid := if #nets === 0 then 0 else max \\ width \ nets;
     side := stack between("+", apply(nets, n -> (stack (height n + depth n : "|"^-1)) ^ (height n)));
     w := side | stack between(concatenate (wid:"-"), nets) | side;
     top := concatenate("+", width w - 2 : "-", "+");
     w = stack(top,w,top);
     if #nets > 0 then w = w ^ (height first nets);
     w)

-- boxNet := x -> ( 
--      s := concatenate("+", width x : "-", "+");
--      n := height x + depth x;
--      if n === 0 then (
--      	  (s || x || s) ^ (height x)
-- 	  )
--      else (
--      	  t := (stack (n : "|")) ^ (height x - 1);
--      	  (s || t | x | t || s) ^ (height x)
-- 	  )
--      )

peek2(ZZ,Net) := (depth,s) -> if depth === 0 then s else boxNets {s}
peek2(ZZ,Sequence) := (depth,s) -> (
     if depth === 0 then net s
     else horizontalJoin(
	  if #s === 0
	  then "()"
	  else if #s === 1 
	  then ("singleton (", peek2(depth,s#0), ")")
	  else ("(", horizontalJoin between (",", apply(s, value -> peek2(depth,value))), ")" )))

precOption := precedence ( 1 => 2 )

peek2(ZZ,HashTable) := (depth,s) -> (
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
		    then ("(",peek2(depth-1,key),")")
		    else peek2(depth-1,key),
		    " => ",
		    if precedence value < precOption
		    then ("(",peek2(depth-1,value),")")
		    else peek2(depth-1,value))),
	  "}"
	  ))

peek2(ZZ,Dictionary) := (depth,d) -> (
     if depth === 0 then net d
     else horizontalJoin(
	  "Dictionary{", 
	  stack apply(sort pairs d, (lhs,rhs) -> horizontalJoin splice (peek lhs," => ",peek2(depth-1,rhs))),
	  "}"))

peek = s -> peek2(1,s)
typicalValues#peek = Net

seeParsing = args -> (
     x := new MutableHashTable;
     t := (p,s) -> if x#?p then x#p = append(x#p,s) else x#p = {s};
     q := getParsing symbol seeParsing;
     scan(values Main.Dictionary, s -> if getParsing s =!= q and s =!= symbol " " then t(getParsing s,s));
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

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
