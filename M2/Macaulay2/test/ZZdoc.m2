--		Copyright 1995 by Daniel R. Grayson

-- this file has an unusual name so it will be done last.

setrecursionlimit 1000

path = {"../g", "."}

haderror := false

warning := (sym,msg) -> (
     haderror = true;
     r := try locate sym else null;
     if r === null then (
	  stderr << "error: " << msg << endl;
	  )
     else ((filename,row1,row2) -> (
     	       stderr << filename << ":" << row1 << ": " << msg << endl;
	       )) r)

tab := symbolTable()
currentPage := null
verify := method(SingleArgumentDispatch=>true)
local Reach
reach := method(SingleArgumentDispatch=>true)
DocumentationMissing := new MutableHashTable
reachable := new MutableHashTable
verify Thing := x -> null
verify Sequence := verify BasicList := x -> scan(x,verify)
reach Thing := x -> null
reach Sequence := reach BasicList := x -> scan(x,reach)
DocumentationProvided := set topicList()
verify TO := x -> (
     s := x#0;
     if not DocumentationProvided#?s and not DocumentationMissing#?s 
     then DocumentationMissing#s = currentPage;
     )
reach TO := x -> (
     s := x#0;
     if not reachable#?s or not reachable#s
     then (
	  reachable#s = true;
	  reach doc s;
	  ))
scan(keys DocumentationProvided,
     s -> (
	  reachable#s = false;
	  d := doc s;
	  currentPage = s;
	  verify d;
	  ))
reachable#"Macaulay 2" = true
reach doc "Macaulay 2"
scan(sort pairs DocumentationMissing,
     (s,w) -> warning(
	  if tab#?s then tab#s,
	  "documentation for '"|s|"' missing, needed for '"|w|"'"))

unreachable := applyPairs(
     new HashTable from reachable,
     (k,v) -> if not v then (k,true))
scan(sort keys unreachable,
     s -> warning(
	  if tab#?s then tab#s,
	  "documentation for '"|s|"' not reachable"))

DocumentationNotNeeded = new MutableHashTable from apply(
     elements (tab#"a" .. tab#"Z"), s -> (s,true))
DocumentationNotNeeded#(quote
     ) = true
DocumentationNotNeeded#(quote[) = true
DocumentationNotNeeded#(quote{) = true
DocumentationNotNeeded#(quote() = true

scan(sort pairs tab, (n,s) -> 
     if not DocumentationProvided#?n and not DocumentationNotNeeded#?s
     then warning(s,"no documentation for symbol '"|n|"'"))

scan(tab#"a" .. tab#"Y", 
     i -> if lookupCount i != 1 
     then warning(i,
	  "indeterminate '"|name i|"' has been used "|name lookupCount i|" times"
	  ))

-- symbols which have been seen only once and are not protected
() -> (
     -- mention these symbols one more time so we don't report them.
     a b c d e f g h i j k l m n o p q r s t u v w x y z
     A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
     )
unset := sort select (values tab, s -> 
     class s === Symbol
     and lookupCount s === 1
     and mutable s
     and string s =!= "\n"
     )
scan(unset, s -> warning(s,"symbol '"|name s|"' seen only once"))

vals := select(tab#"a" .. tab#"Y", s -> value s =!= s)
scan(vals, s -> warning(s,"indeterminate '"|name s|"' has a value already"))

-- if haderror then exit 1
