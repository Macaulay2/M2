--		Copyright 1994 by Daniel R. Grayson

-- documentation is in doc.m2 because this file is loaded early

Function \ Sequence := Function \ List := (f,v) -> apply(v,f)
Sequence / Function := List / Function := (v,f) -> apply(v,f)

use = identity				  -- just temporary, until methods.m2

globalAssignFunction = (X,x) -> (
     if not x#?(quote name) then (
	  x.symbol = X;
	  x.name = string X;
	  );
     use x;
     )

globalReleaseFunction = (X,x) -> (
     if x.?symbol and X === x.symbol
     then (
	  remove(x,quote name);
	  remove(x,quote symbol);
	  )
     )

GlobalAssignHook Type := globalAssignFunction
GlobalReleaseHook Type := globalReleaseFunction

lookupi := x -> (
     r := lookup x;
     if r === null then error "encountered null or missing value";
     r)

name = s -> (
     if s.?name
     then (
	  if class s.name === String or class s.name === Net
	  then s.name
	  else error "expected value stored under 'name' to be a string or net"
	  )
     else (
	  f := lookup(name,class s);
	  if f =!= null 
	  then
	  f(s)
	  else concatenate("<<",name class s,":",name hash s,">>")))

name Thing := string
name String := format
name Net := x -> (
     s := concatenate( "stack(", between(",",apply(netRows x, format)), ")" );
     if height x === 1 then s
     else concatenate( "((", s, ")^", string(height x - 1), ")" )
     )

alphabet := new MutableHashTable
scan( characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", c -> alphabet#c = true)
operators = new MutableHashTable	  -- erased in methods.m2
scan( { quote or, quote do, quote else, quote then, quote of, quote shield,
	  quote from, quote and, quote not, quote if, quote try, 
	  quote new, quote while, quote quote, quote global, quote local,
	  quote timing, quote time
	  },
     i -> operators#i = concatenate("quote ", string i))
scan(pairs symbolTable(), (n,s) -> (
	  if not alphabet#?(n#0) then (
	       operators#s = concatenate("quote ",n);
	       )
	  )
     )
alphabet = null
operators#(quote " ") = ///quote " "///

name Symbol := s -> (
     if operators#?s then operators#s 
     -- else if value s =!= s then concatenate("quote ", string s)
     else string s
     )

name Function := f -> error "'name' applied to function too early in setup"

name MutableHashTable := s -> concatenate (
     name class s,
     if parent s =!= Nothing then (" of ", name parent s),
     "{...}"
     )
name HashTable := s -> concatenate (
     "new ", name class s,
     if parent s =!= Nothing then (" of ", name parent s),
     " from {",
     if # s > 0
     then demark(", ", apply(pairs s, (k,v) -> name k | " => " | name v) )
     else "",
     "}")
name MutableList := s -> concatenate("new ",name class s, " from {...}")
name BasicList := s -> concatenate(
     (if class s === List then "{" else ("new ", name class s," from {")),
     between(", ",apply(toList s,name)),
     "}" )
name Array := s -> concatenate ( "[", between(", ",name \ toList s), "]" )
name Sequence := s -> (
     if # s === 1 then concatenate("singleton ",name s#0)
     else concatenate("(",between(",",name \ s),")")
     )

describe = s -> (lookupi(name,class s)) s

File << Thing := (o,x) -> o << name x	  -- provisional
