--		Copyright 1996-2000 by Daniel R. Grayson

-- strings

separateRegexp = method()
separateRegexp(String,String) := (re,s) -> separateRegexp(re,0,s)
separateRegexp(String,ZZ,String) := (re,n,s) -> (
     m := matches(re,s);
     if m#?n then prepend(substring(s,0,m#n#0), separateRegexp(re,n,substring(m#n#0+m#n#1,s))) else {s})

selectRegexp = method()
selectRegexp(String,String) := (re,s) -> selectRegexp(re,0,s)
selectRegexp(String,ZZ,String) := (re,n,s) -> (
     m := matches(re,s);
     if m#?n then substring(m#n#0,m#n#1,s))

-- nets

Net.AfterPrint = identity

toString MutableHashTable := s -> (
     if s.?name and class s.name === String then return s.name;
     r := reverseDictionary s;
     if r =!= null then return toString r;
     concatenate ( toString class s, if parent s =!= Nothing then (" of ", toString parent s), "{...", toString(#s), "...}"))
toString HashTable := s -> (
     if s.?name and class s.name === String then return s.name;
     r := reverseDictionary s;
     if r =!= null then return toString r;
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
     if # s === 1 then concatenate("singleton ",toString s#0)
     else concatenate("(",between(",",toString \ s),")")
     )
net Command := toString Command := toExternalString Command := f -> (
     X := reverseDictionary f;
     if X =!= null then toString X else "--Command--"
     )

net Function := toString Function := f -> (
     X := reverseDictionary f;
     if X =!= null then toString X else (
	  t := locate f;
	  if t === null then "--Function--" 
	  else concatenate("--Function[", t#0, ":", toString t#1| ":", toString t#2, "-", toString t#3| ":", toString t#4, "]--")
	  )
     )

toExternalString Manipulator := toString Manipulator := f -> (
     X := reverseDictionary f;
     if X =!= null then toString X else "--Manipulator--"
     )
-----------------------------------------------------------------------------
toExternalString String := format

toString Net := x -> concatenate between("\n",unstack x)
toExternalString Net := x -> concatenate(format toString x, "^", toString(height x - 1))

toExternalString MutableHashTable := s -> (
     if s.?name and class s.name === String then return s.name;
     r := reverseDictionary s;
     if r =!= null then return toString r;
     error "anonymous mutable hash table cannot be converted to external string";
--     concatenate (
--	  toExternalString class s,
--	  if parent s =!= Nothing then (" of ", toExternalString parent s),
--	  "{...", toString(#s), "...}"
--	  )
     )
toExternalString HashTable := s -> (
     if s.?name and class s.name === String then return s.name;
     r := reverseDictionary s;
     if r =!= null then return toString r;
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
     (if class s === List then "{" else ("new ", toExternalString class s," from {")),
     between(", ",apply(toList s,toExternalString)),
     "}" )
toExternalString Array := s -> concatenate ( "[", between(", ",toExternalString \ toList s), "]" )
toExternalString Sequence := s -> (
     if # s === 1 then concatenate("singleton ",toExternalString s#0)
     else concatenate("(",between(",",toExternalString \ s),")")
     )
toExternalString Thing := toString
-----------------------------------------------------------------------------
describe = method()
describe Thing := net
describe MutableHashTable := x -> (
     if x.?name then (
	  n := x.name;
	  remove(x, symbol name);
	  d := net x;
	  x.name = n;
	  d)
     else net x
     )

net Manipulator := toString
net Thing := toString
-----------------------------------------------------------------------------
toExternalString Symbol := s -> (
     if not mutable s and value s === s then toString s
     else (
	  if s === symbol " "
	  then ///symbol " "///
	  else concatenate("symbol ",toString s)
	  )
     )

net Symbol := toString
File << Symbol := File => (o,s) -> o << toString s		    -- provisional
File << Thing  := File => (o,s) -> o << toString s		    -- provisional
-----------------------------------------------------------------------------
net Option := z -> net expression z

Net == Net := (x,y) -> x === y
Net == String := (n,s) -> (
     height n === 1 and depth n === 0 and width n === length s and n#0 === s
     )
String == Net := (s,n) -> n == s

net String := horizontalJoin
net RR := net Boolean := net File := net ZZ := net Database := toString
net000 := horizontalJoin ()
net Nothing := null -> net000

Net | Net := Net => horizontalJoin
Net || Net := Net => stack
String ^ ZZ := Net => (s,i) -> raise(horizontalJoin s,i)
Net ^ ZZ := Net => raise; erase symbol raise
String ^ Sequence := Net => (s,p) -> (
     (height,depth) -> (
	  tot := height + depth;
	  if tot <= 0 then (stack())^height
	  else (stack apply(tot,i->s))^(height-1)
	  )
     ) p

net Net := identity
---- this old routine draws a box around a net
--net Net := x -> ( 
--     s := concatenate("+", width x : "-", "+");
--     n := height x + depth x;
--     if n === 0 then (
--     	  (s || x || s) ^ (height x)
--	  )
--     else (
--     	  t := (stack (n : "|")) ^ (height x - 1);
--     	  (s || t | x | t || s) ^ (height x)
--	  )
--     )

comma := ", "

net Sequence := x -> horizontalJoin deepSplice (
     if #x === 0 then "()"
     else if #x === 1 then ("singleton ", net x#0)
     else ("(", toSequence between(comma,apply(x,net)), ")"))

net List := x -> horizontalJoin deepSplice (
     "{",
     toSequence between(comma,apply(x,net)),
     "}")

VerticalList = new Type of List
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
net MutableList := x -> horizontalJoin ( net class x, "{...", toString(#x), "...}" )
net HashTable := x -> (
     if x.?name and class x.name === String then return x.name;
     r := reverseDictionary x;
     if r =!= null then return toString r;
     horizontalJoin flatten ( 
     	  net class x,
	  "{", 
	  -- the first line prints the parts vertically, second: horizontally
 	  stack (horizontalJoin \ sort apply(pairs x,(k,v) -> (net k, " => ", net v))),
	  -- between(", ", apply(pairs x,(k,v) -> net k | "=>" | net v)), 
	  "}" 
     	  ))

net MutableHashTable := x -> (
     if x.?name and class x.name === String then return x.name;
     r := reverseDictionary x;
     if r =!= null then return toString r;
     horizontalJoin ( net class x, if #x > 0 then ("{...", toString(#x), "...}") else "{}" ))

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
