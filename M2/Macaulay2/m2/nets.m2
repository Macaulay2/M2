--		Copyright 1996-2000 by Daniel R. Grayson

Net.AfterPrint = identity

toString MutableHashTable := s -> if s.?name and class s.name === String then s.name else concatenate (
     toString class s, if parent s =!= Nothing then (" of ", toString parent s), 
     "{...", toString(#s), "...}"
     )
toString HashTable := s -> if s.?name and class s.name === String then s.name else concatenate (
     "new ", toString class s,
     if parent s =!= Nothing then (" of ", toString parent s),
     " from {",
     if # s > 0
     then demark(", ", apply(pairs s, (k,v) -> toString k | " => " | toString v) )
     else "",
     "}")
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
net Command := toString Command := toExternalString Command := f -> if Symbols#?f then string Symbols#f else "--Command--"

net Function := toString Function := f -> (
     if Symbols#?f then string Symbols#f 
     else (
	  l := locate f;
	  if l === null then "--Function--" 
	  else ((fn,li,co) -> concatenate("--Function[", fn, ":", toString li, ":", toString co, "]--")) l
	  )
     )

toExternalString Manipulator := toString Manipulator := f -> if Symbols#?f then string Symbols#f else "--Manipulator--"
toString Thing := string
-----------------------------------------------------------------------------
toExternalString String := format

toString Net := x -> concatenate between("\n",unstack x)
toExternalString Net := x -> concatenate(format toString x, "^", toString(height x - 1))

toExternalString MutableHashTable := s -> if s.?name and class s.name === String then s.name else concatenate (
     toExternalString class s,
     if parent s =!= Nothing then (" of ", toExternalString parent s),
     "{...", toString(#s), "...}"
     )
toExternalString HashTable := s -> if s.?name and class s.name === String then s.name else concatenate (
     "new ", toExternalString class s,
     if parent s =!= Nothing then (" of ", toExternalString parent s),
     " from {",
     if # s > 0
     then demark(", ", apply(pairs s, (k,v) -> toExternalString k | " => " | toExternalString v) )
     else "",
     "}")
toExternalString MutableList := s -> concatenate("new ",toExternalString class s, 
     " from {...", toString(#s), "...}" )
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
     if not mutable s and value s === s then string s
     else (
	  if s === symbol " "
	  then ///symbol " "///
	  else concatenate("symbol ",string s)
	  )
     )

net Symbol := string
File << Symbol := File => (o,s) -> o << string s		    -- provisional
File << Thing  := File => (o,s) -> o << toString s		    -- provisional
-----------------------------------------------------------------------------
net Option := z -> net expression z

Net == Net := (x,y) -> x === y
Net == String := (n,s) -> (
     height n === 1 and depth n === 0 and width n === length s and n#0 === s
     )
String == Net := (s,n) -> n == s

net String := horizontalJoin
net RR := net Boolean := net File := net ZZ := net Database := string
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
     if x.?name then x.name
     else horizontalJoin flatten ( 
     	  net class x,
	  "{", 
	  -- the first line prints the parts vertically, second: horizontally
 	  stack (horizontalJoin \ sort apply(pairs x,(k,v) -> (net k, " => ", net v))),
	  -- between(", ", apply(pairs x,(k,v) -> net k | "=>" | net v)), 
	  "}" 
     	  )
     )

net Dictionary := d -> if Symbols#?d then toString Symbols#d else "Dictionary{..." | toString length d | "...}"

net MutableHashTable := x -> (
     if x.?name then x.name 
     else horizontalJoin ( net class x, if #x > 0 then ("{...", toString(#x), "...}") else "{}" )
     )

texMath Net := n -> concatenate (
     ///{\arraycolsep=0pt
\begin{matrix}
///,
     between(///\\
///, unstack n / characters / (i -> apply(i,c -> (///\tt ///,c))) / (s -> between("&",s))),
     ///
\end{matrix}}
///)


erase symbol string
