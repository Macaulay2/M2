--		Copyright 1996-2000 by Daniel R. Grayson

-- nets

Net#{Standard,AfterPrint} = identity

toString MutableHashTable := s -> (
     concatenate ( toString class s, if parent s =!= Nothing then (" of ", toString parent s), "{...", toString(#s), "...}"))
toString Type := X -> (
     if hasAnAttribute X then (
	  if hasAttribute(X,PrintNames) then return getAttribute(X,PrintNames);
	  if hasAttribute(X,ReverseDictionary) then return toString getAttribute(X,ReverseDictionary);
	  );
     concatenate(toString class X, " of ", toString parent X))
toString HashTable := s -> (
     concatenate (
	  "new ", toString class s,
	  if parent s =!= Nothing then (" of ", toString parent s),
	  " from {",
	  if # s > 0
	  then demark(", ", apply(pairs s, (k,v) -> toString k | " => " | toString v) )
	  else "",
	  "}"))
toString MutableList := s -> concatenate(toString class s,"{...",toString(#s),"...}")
toStringn := i -> if i === null then "" else toString i
toString BasicList := s -> concatenate(
     if class s =!= List then toString class s,
     "{", between(", ",apply(toList s,toStringn)), "}"
     )
toString Array := s -> concatenate ( "[", between(", ",toStringn \ toList s), "]" )
toString AngleBarList := s -> concatenate ( "<|", between(", ",toStringn \ toList s), "|>" )
toString Sequence := s -> (
     if # s === 1 then concatenate("1 : (",toString s#0,")")
     else concatenate("(",between(",",toStringn \ s),")")
     )
net Command := toString Command := toExternalString Command := f -> (
     if hasAttribute(f,ReverseDictionary) then return toString getAttribute(f,ReverseDictionary) else "-*Command*-"
     )

toExternalString Function := f -> (
     if hasAttribute(f,ReverseDictionary) then return toString getAttribute(f,ReverseDictionary);
     t := locate f;
     if t === null then error "can't convert anonymous function to external string"
     else error("can't convert anonymous function (",t#0, ":", toString t#1| ":", toString t#2, "-", toString t#3| ":", toString t#4,") to external string")
     )

net Function := toString Function := f -> (
     if hasAttribute(f,ReverseDictionary) then return toString getAttribute(f,ReverseDictionary);
     t := locate f;
     if t === null then "-*Function*-" 
     else concatenate("-*Function[", t#0, ":", toString t#1| ":", toString (t#2+1), "-", toString t#3| ":", toString (t#4+1), "]*-")
     )

net FunctionBody := toString FunctionBody := f -> (
     t := locate' f;
     if t === null then "-*FunctionBody*-" 
     else concatenate("-*FunctionBody[", t#0, ":", toString t#1| ":", toString (t#2+1), "-", toString t#3| ":", toString (t#4+1), "]*-")
     )

toExternalString Manipulator := f -> (
     if hasAttribute(f,ReverseDictionary) then return toString getAttribute(f,ReverseDictionary) else concatenate("new Manipulator from ",toExternalString toList f)
     )
toString Manipulator := f -> (
     if hasAttribute(f,ReverseDictionary) then return toString getAttribute(f,ReverseDictionary) else concatenate("new Manipulator from ",toString toList f)
     )
-----------------------------------------------------------------------------
toExternalString String := format

toString Net := x -> demark("\n",unstack x)
toExternalString Net := x -> if height x + depth x == 0 then
     concatenate("(horizontalJoin())", "^", toString height x) else
     concatenate(format toString x, "^", toString(height x - 1))

toExternalString MutableHashTable := s -> (
     if hasAttribute(s,ReverseDictionary) then return toString getAttribute(s,ReverseDictionary);
     error "anonymous mutable hash table cannot be converted to external string";
     )
toExternalString Type := s -> (
     if hasAttribute(s,ReverseDictionary) then return toString getAttribute(s,ReverseDictionary);
     error "anonymous type cannot be converted to external string";
     )
toExternalString HashTable := s -> (
     concatenate (
	  "new ", toExternalString class s,
	  if parent s =!= Nothing then (" of ", toExternalString parent s),
	  " from {",
	  if # s > 0
	  then demark(", ", apply(pairs s, (k,v) -> toExternalString k | " => " | toExternalString v) )
	  else "",
	  "}"))
toExternalString MutableList := s -> (
     error "anonymous mutable list cannot be converted to external string";
     -- concatenate("new ",toExternalString class s, " from {...", toString(#s), "...}" )
     )
mid := s -> (
     if #s === 1 then toExternalString s#0
     else between(",",apply(toSequence s,x -> if x === null then "" else toExternalString x))
     )
toExternalString BasicList := s -> concatenate(
     (if class s === List then "{" else ("new ", toString class s, " from {")), mid s, "}" )
toExternalString Array := s -> concatenate("[",mid s,"]")
toExternalString Sequence := s -> (
     if # s === 1 then concatenate("1:(",toExternalString s#0,")")
     else concatenate("(",mid s,")"))
-----------------------------------------------------------------------------
net Manipulator := toString
net Thing := toString
-----------------------------------------------------------------------------
net Symbol := toString
File << Symbol := File => (o,s) -> o << toString s		    -- provisional
File << Thing  := File => (o,s) -> o << toString s		    -- provisional
Nothing << Thing := File => (o,s) -> null
-----------------------------------------------------------------------------

Net == String := (n,s) -> (				    -- should install in engine
     height n === 1 and depth n === 0 and width n === length s and n#0 === s
     )
String == Net := (s,n) -> n == s

net String := x -> stack separate x			    -- was horizontalJoin
net RR := net Boolean := net File := net ZZ := net Database := toString
net000 := horizontalJoin ()
net Nothing := null -> "null" -- was net000

Net | Net := Net => horizontalJoin
Net || Net := Net => stack
String ^ ZZ := Net => (s,i) -> raise(horizontalJoin s,i)
Net ^ ZZ := Net => raise
String ^ Sequence := Net => (s,p) -> (
     (height,depth) -> (
	  tot := height + depth;
	  if tot <= 0 then (stack())^height
	  else (stack apply(tot,i->s))^(height-1)
	  )
     ) p

net Net := identity

comma := ", "

netn = i -> if i === null then net000 else net i

net Sequence := x -> horizontalJoin deepSplice (
     if #x === 0 then "()"
     else if #x === 1 then ("1 : (", net x#0, ")")
     else ("(", toSequence between(comma,apply(x,netn)), ")"))

net List := x -> horizontalJoin deepSplice (
     "{",
     toSequence between(comma,apply(x,netn)),
     "}")

embrace = n -> (
     h := height n;
     d := depth n;
     horizontalJoin("{"^(h,d), n, "}"^(h,d)))

VerticalList = new SelfInitializingType of List
VerticalList.synonym = "vertical list"
net VerticalList := x -> if #x === 0 then "{}" else embrace stack apply(x,net)
VerticalList.Wrap = x -> x

NumberedVerticalList = new SelfInitializingType of VerticalList
NumberedVerticalList.synonym = "numbered vertical list"
net NumberedVerticalList := x -> if #x === 0 then "{}" else embrace stack apply(#x,i -> net (i => x#i));

net Array := x -> horizontalJoin deepSplice (
     "[",
     toSequence between(comma,apply(x,netn)),
     "]")
net AngleBarList := x -> horizontalJoin deepSplice (
     "<|",
     toSequence between(comma,apply(x,netn)),
     "|>")
net BasicList := x -> horizontalJoin deepSplice (
      net class x, 
      "{",
      toSequence between(comma,apply(toList x,netn)),
      "}")
net MutableList := x -> (
     if #x > 0 
     then horizontalJoin ( net class x, "{...", toString(#x), "...}")
     else horizontalJoin ( net class x, "{}")
     )
net HashTable := x -> (
     horizontalJoin flatten (
     	  net class x,
	  "{", 
	  -- the first line prints the parts vertically, second: horizontally
 	  stack (horizontalJoin \ apply(sortByName pairs x,(k,v) -> (net k, " => ", net v))),
	  -- between(", ", apply(pairs x,(k,v) -> net k | "=>" | net v)), 
	  "}" 
     	  ))

net MutableHashTable := x -> (
     if hasAnAttribute x then (
	  if hasAttribute(x,PrintNet) then return getAttribute(x,PrintNet);
	  if hasAttribute(x,PrintNames) then return net getAttribute(x,PrintNames);
	  if hasAttribute(x,ReverseDictionary) then return toString getAttribute(x,ReverseDictionary);
	  );
     horizontalJoin ( net class x, if #x > 0 then ("{...", toString(#x), "...}") else "{}" ))
net Type := X -> (
     if hasAnAttribute X then (
	  if hasAttribute(X,PrintNet) then return getAttribute(X,PrintNet);
	  if hasAttribute(X,PrintNames) then return net getAttribute(X,PrintNames);
	  if hasAttribute(X,ReverseDictionary) then return toString getAttribute(X,ReverseDictionary);
	  );
     horizontalJoin ( net class X, " of ", net parent X))

-----------------------------------------------------------------------------

netList = method(Options => {
	  Boxes => true,
	  BaseRow => 0,
	  HorizontalSpace => 0,
	  VerticalSpace => 0,
	  Alignment => Left				    -- Center, Left, or Right or list of those
	  })

maxN := x -> if #x === 0 then 0 else max x

alignmentFunctions := new HashTable from {
     Left => (wid,n) -> n | horizontalJoin(wid - width n : " "^(- depth n)),
     Right => (wid,n) -> horizontalJoin(wid - width n : " "^(- depth n)) | n,
     Center => centerString
     }

netList VisibleList := o -> (x) -> (
     x = apply(x, row -> if instance(row,List) then row else {row});
     (br,hs,vs,bx,algn) := (o.BaseRow,o.HorizontalSpace,o.VerticalSpace,o.Boxes,splice o.Alignment);
     if br < 0
     or br >= #x+1					    -- this allows the base row to be absent
     then error "netList: base row out of bounds";
     x = apply(x, row->apply(row, netn));
     n := maxN(length \ x);
     if n == 0 then ( x={{net000}}; n=1; );
     if not instance(bx,List) then bx={bx,bx};
     bxrows := set if bx#0 === true then 0..#x else if bx#0 === false then {} else bx#0;
     bxcols := set if bx#1 === true then 0..n else if bx#1 === false then {} else bx#1;
     x = apply(x, row -> if #row == n then row else join(row, n-#row : stack()));
     colwids := max \ transpose applyTable(x,width);
     if not instance(algn,List) then algn = n : algn;
     algn = apply(algn, key -> alignmentFunctions#key);
     x = apply(x, row -> apply(n, i -> algn#i(colwids#i,row#i)));
     x = apply(#x, i -> (
	     h := max(height \ x#i);
	     if member(i,bxrows) then h = h + vs;
	     d := max(depth \ x#i);
	     if member(i+1,bxrows) or i<#x-1 then d = d + vs;
	     sep := "|"^(h,d);
	     nosep := ""^(h,d);
	     hsep := (spaces hs)^(h,d);
	     (if member(0,bxcols) then sep | hsep else nosep)
	     | (horizontalJoin mingle(x#i,apply(1..#colwids-1,j->if member(j,bxcols) then hsep|sep|hsep else hsep)))
	     | (if member(#colwids,bxcols) then hsep | sep else nosep)
	     ));
     colwids = apply(#colwids, i -> colwids#i+hs*(if member(i,bxcols) then 2 else if i<#colwids-1 or member(#colwids,bxcols) then 1 else 0));
     hbar := concatenate mingle(apply(#colwids+1,i->if member(i,bxcols) then "+" else ""),apply(colwids,wid -> wid:"-"));
     x = mingle(apply(#x+1,i->if member(i,bxrows) then hbar else net000),x);
     br = 2*br + 1;
     (stack x)^(
	  sum(0 .. br-1, i -> depth x#i)
	  +
	  sum(1 .. br, i -> try height x#i else 1)	    -- this allows the base row to be absent
	  ))

-- TODO: move to debugging, except for Net?
commentize = method(Dispatch => Thing)
commentize Nothing   := s -> ""
commentize BasicList := s -> commentize horizontalJoin s
commentize String    := s -> concatenate(" -- ", between("\n -- ", separate s))
commentize Net       := S -> (
    baseline := height S - if height S == -depth S then 0 else 1;
    (stack(commentize \ unstack S))^baseline)

printerr = msg -> (stderr << commentize msg << endl;) -- always return null
warning  = msg -> if debugLevel > 0 then (
    if msg =!= () then printerr("warning: " | msg);
    error "warning issued, debugLevel > 0");

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
