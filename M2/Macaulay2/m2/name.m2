--		Copyright 1994 by Daniel R. Grayson

-- documentation is in doc.m2 because this file is loaded early

Function \ Sequence := Function \ List := (f,v) -> apply(v,f)
Sequence / Function := List / Function := (v,f) -> apply(v,f)

GlobalAssignHook Type := (X,x) -> (
     if not x#?(quote name) then x.name = X
     )

GlobalReleaseHook Type := (X,x) -> (
     if x#?(quote name) and X === x.name
     then remove(x,quote name)
     )

lookupi := x -> (
     r := lookup x;
     if r === null then error "encountered null or missing value";
     r)

name = s -> (
     if s.?name
     then string s.name
     else (
	  f := lookup(name,class s);
	  if f =!= null 
	  then try f(s)			  -- mask out errors here because
	       	    	      	   	  -- error printing routines often call us
	  else concatenate("<<",name class s,":",name hash s,">>")
	  else concatenate("<<",name class s,":",name hash s,">>")))

name Thing := string
name String := format;

name MutableHashTable := s -> concatenate (
     name class s,
     if parent s =!= Nothing then (" of ", name parent s),
     "{...}"
     )
name HashTable := s -> concatenate (
     name class s,
     if parent s =!= Nothing then (" of ", name parent s),
     "{",
     if # s > 0
     then demark(", ", apply(pairs s, (k,v) -> name k | " => " | name v) )
     else "",
     "}")
name MutableList := s -> concatenate(name class s, "{...}")
name BasicList := s -> concatenate(
     (if class s === List then "{" else (name class s,"{")),
     between(", ",apply(elements s,name)),
     "}" )
name Array := s -> concatenate ( "[", between(", ",name \ elements s), "]" )
name Sequence := s -> (
     if # s === 1 then concatenate("seq ",name s#0)
     else concatenate("(",between(",",name \ s),")")
     )

describe = s -> (
     << (lookupi(name,class s)) s << endl;
     )

File << Thing := (o,x) -> o << name x	  -- provisional
