--		Copyright 1996-2000 by Daniel R. Grayson

-- strings

separateRegexp = method()
separateRegexp(String,String) := (re,s) -> separateRegexp(re,0,s)
separateRegexp(String,ZZ,String) := (re,n,s) -> (
     m := regex(re,s);
     if m#?n then prepend(substring(s,0,m#n#0), separateRegexp(re,n,substring(m#n#0+m#n#1,s))) else {s}	-- this is slow on large things!
     )

selectRegexp = method()
selectRegexp(String,String) := (re,s) -> selectRegexp(re,0,s)
selectRegexp(String,ZZ,String) := (re,n,s) -> (
     m := regex(re,s);
     if m#?n then substring(m#n#0,m#n#1,s) else error "regular expression didn't match")

-- nets

Net.AfterPrint = identity

toString MutableHashTable := s -> (
     concatenate ( toString class s, if parent s =!= Nothing then (" of ", toString parent s), "{...", toString(#s), "...}"))
toString Type := X -> (
     if PrintNames#?X then PrintNames#X
     else if ReverseDictionary#?X then return toString ReverseDictionary#X;
     concatenate(toString class X, " of ", toString parent X, "{...", toString(#X), "...}"))
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
toString BasicList := s -> concatenate(
     if class s =!= List then toString class s,
     "{", between(", ",apply(toList s,toString)), "}"
     )
toString Array := s -> concatenate ( "[", between(", ",toString \ toList s), "]" )
toString Sequence := s -> (
     if # s === 1 then concatenate("1 : (",toString s#0,")")
     else concatenate("(",between(",",toString \ s),")")
     )
net Command := toString Command := toExternalString Command := f -> (
     if ReverseDictionary#?f then return toString ReverseDictionary#f else "--Command--"
     )

toExternalString Function := f -> (
     if ReverseDictionary#?f then return toString ReverseDictionary#f;
     t := locate f;
     if t === null then error "can't convert anonymous function to external string"
     else error("can't convert anonymous function (",t#0, ":", toString t#1| ":", toString t#2, "-", toString t#3| ":", toString t#4,") to external string")
     )

net Function := toString Function := f -> (
     if ReverseDictionary#?f then return toString ReverseDictionary#f;
     t := locate f;
     if t === null then "--Function--" 
     else concatenate("--Function[", t#0, ":", toString t#1| ":", toString t#2, "-", toString t#3| ":", toString t#4, "]--")
     )

toExternalString Manipulator := f -> (
     if ReverseDictionary#?f then return toString ReverseDictionary#f else concatenate("new Manipulator from ",toExternalString toList f)
     )
toString Manipulator := f -> (
     if ReverseDictionary#?f then return toString ReverseDictionary#f else concatenate("new Manipulator from ",toString toList f)
     )
-----------------------------------------------------------------------------
toExternalString String := format

toString Net := x -> demark("\n",unstack x)
toExternalString Net := x -> concatenate(format toString x, "^", toString(height x - 1))

toExternalString MutableHashTable := s -> (
     if ReverseDictionary#?s then return toString ReverseDictionary#s;
     error "anonymous mutable hash table cannot be converted to external string";
     )
toExternalString Type := s -> (
     if ReverseDictionary#?s then return toString ReverseDictionary#s;
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
toExternalString BasicList := s -> concatenate(
     (if class s === List then "{" else ("new ", 
	       toString class s,			    -- was toExternalString !
	       " from {")),
     between(", ",apply(toList s,toExternalString)),
     "}" )
toExternalString Array := s -> concatenate ( "[", between(", ",toExternalString \ toList s), "]" )
toExternalString Sequence := s -> (
     if # s === 1 then concatenate("1 : (",toExternalString s#0,")")
     else concatenate("(",between(",",toExternalString \ s),")")
     )
-----------------------------------------------------------------------------
describe = method()
describe Thing := net

net Manipulator := toString
net Thing := toString
-----------------------------------------------------------------------------
net Symbol := toString
File << Symbol := File => (o,s) -> o << toString s		    -- provisional
File << Thing  := File => (o,s) -> o << toString s		    -- provisional
-----------------------------------------------------------------------------
net Option := z -> net z#0 | " => " | net z#1

Net == Net := (x,y) -> x === y
Net == String := (n,s) -> (
     height n === 1 and depth n === 0 and width n === length s and n#0 === s
     )
String == Net := (s,n) -> n == s

net String := x -> stack separate x			    -- was horizontalJoin
net RR := net Boolean := net File := net ZZ := net Database := toString
net000 := horizontalJoin ()
net Nothing := null -> net000

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

net Sequence := x -> horizontalJoin deepSplice (
     if #x === 0 then "()"
     else if #x === 1 then ("1 : (", net x#0, ")")
     else ("(", toSequence between(comma,apply(x,net)), ")"))

net List := x -> horizontalJoin deepSplice (
     "{",
     toSequence between(comma,apply(x,net)),
     "}")

VerticalList = new SelfInitializingType of VisibleList
net VerticalList := x -> "{" | stack toSequence apply(x,net) | "}"

net Array := x -> horizontalJoin deepSplice (
     "[",
     toSequence between(comma,apply(x,net)),
     "]")
net BasicList := x -> horizontalJoin deepSplice (
      net class x, 
      "{",
      toSequence between(comma,apply(toList x,net)),
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
 	  stack (horizontalJoin \ sort apply(pairs x,(k,v) -> (net k, " => ", net v))),
	  -- between(", ", apply(pairs x,(k,v) -> net k | "=>" | net v)), 
	  "}" 
     	  ))

net MutableHashTable := x -> (
     if PrintNames#?x then PrintNames#x
     else horizontalJoin ( net class x, if #x > 0 then ("{...", toString(#x), "...}") else "{}" ))
net Type := X -> (
     if PrintNames#?X then PrintNames#X
     else if ReverseDictionary#?X then return toString ReverseDictionary#X
     else horizontalJoin ( net class X, if #X > 0 then ("{...", toString(#X), "...}") else "{}" ))

texMath Net := n -> concatenate (
     ///{\arraycolsep=0pt
\begin{matrix}
///,
     between(///\\
///, unstack n / characters / (i -> apply(i,c -> (///\tt ///,c))) / (s -> between("&",s))),
     ///
\end{matrix}}
///)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
