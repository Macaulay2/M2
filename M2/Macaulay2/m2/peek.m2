--		Copyright 1993-1999 by Daniel R. Grayson

peek' = method(TypicalValue => Net)

peek'(ZZ,ZZ) := (depth,n) -> toString n
peek'(ZZ,Nothing) := (depth,s) -> "null"
peek'(ZZ,Symbol) := (depth,s) -> if depth === 0 then toString s else toExternalString s
peek'(ZZ,Thing) := (depth,s) -> net s

peek'(ZZ,BasicList) := (depth,s) -> (
     if depth === 0 then net s
     else horizontalJoin(
	  net class s,
	  "{", horizontalJoin between (", ", apply(toList s, value -> peek'(depth-1,value))), "}" ) )

peek'(ZZ,MarkUpListParagraph) := peek'(ZZ,MarkUpList) := (depth,s) -> (
     if depth === 0 then net s
     else horizontalJoin(
	  net class s,
	  "{", horizontalJoin between (", ", apply(toList s, value -> peek'(if instance(value,MarkUpList) or instance(value,String) then depth else depth-1, value))), "}" ) )

peek'(ZZ,List) := (depth,s) -> (
     if depth === 0 then net s
     else horizontalJoin(
	  "{", horizontalJoin between (", ", apply(s, value -> peek'(depth,value))), "}" ) )
peek'(ZZ, String) := (depth,s) -> if depth === 0 then s else format s

vbar := (ht,dp) -> (stack (ht + dp : "|"^-1)) ^ ht;
boxList = method(SingleArgumentDispatch => true)
boxList List := boxList Sequence := nets -> (
     nets = net \ nets;
     wid := if #nets === 0 then 0 else max \\ width \ nets;
     side := stack between("+", apply(nets, n -> vbar(height n, depth n)));
     w := side | stack between(concatenate (wid:"-"), nets) | side;
     top := concatenate("+", width w - 2 : "-", "+");
     w = stack(top,w,top);
     if #nets > 0 then w = w ^ (height first nets);
     w)
upWidth := (wid,n) -> n | horizontalJoin(wid - width n : " "^(height n - 1))
joinRow := x -> horizontalJoin mingle(#x+1:vbar(max\\height\x,max\\depth\x),x)
boxTable = method(SingleArgumentDispatch => true)
boxTable List :=
boxTable Sequence := x -> (
     if not all(x, row -> class row === List) then error "expected a list or sequence of lists";
     n := max(length \ x);
     x = apply(x, row -> if #row == n then row else join(row, n-#row : ""));
     if #x == 0 or #x#0 == 0 then return boxList x;
     x = applyTable(x,net);
     colwids := max \ transpose applyTable(x,width);
     x = joinRow \ apply(x, row -> apply(colwids,row,upWidth));
     hbar := concatenate mingle(#colwids+1:"+",apply(colwids,wid -> wid:"-"));
     (stack mingle(#x+1:hbar,x))^(height x#0))
netJoinRow := x -> horizontalJoin between(" ",x)
netTable' = x -> (
     n := max(length \ x);
     x = apply(x, row -> if #row == n then row else join(row, n-#row : ""));
     if #x == 0 or #x#0 == 0 then return "";
     x = applyTable(x,net);
     colwids := max \ transpose applyTable(x,width);
     x = netJoinRow \ apply(x, row -> apply(colwids,row,upWidth));
     (stack x)^(height x#0-1))

peek'(ZZ,Net) := (depth,s) -> if depth === 0 then s else boxList {s}
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
	  "Dictionary{", 
	  stack apply(sort pairs d, (lhs,rhs) -> horizontalJoin splice (peek lhs," => ",peek'(depth-1,rhs))),
	  "}"))

peek = s -> peek'(1,s)
typicalValues#peek = Net

seeParsing = args -> (
     x := new MutableHashTable;
     t := (p,s) -> if x#?p then x#p = append(x#p,s) else x#p = {s};
     q := getParsing symbol apply;
     scan(keys set values Macaulay2Core.Dictionary, s -> if getParsing s =!= q and s =!= symbol " " then t(getParsing s,s));
     t(getParsing symbol apply, "<SYMBOLS>"  );
     t(getParsing symbol " ", "<ADJACENCY>");
     new Table from prepend(
	  { "parsing\nprecedence", "binary\nbinding\nstrength","unary\nbinding\nstrength", "\noperators" },
 	  sort pairs x / (
	       (a,b) -> append(a/(i -> if i === -1 then "" else toString i),
		    concatenate(
			 between("  ",
			      sort(b/toString)))))))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
