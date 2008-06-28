newPackage(
	"SimpleDoc",
    	Version => "0.1", 
    	Date => "June 28, 2008",
    	Authors => {{Name => "Mike Stillman", 
		  Email => "math@math.cornell.edu", 
		  HomePage => "http://www.math.cornell.edu/~mike/"}},
    	Headline => "a simple documentation function",
    	DebuggingMode => true
    	)

export {doc}

doc = method()
doc String := (s) -> null

--document toDoc s

indentationLevel = (s) -> (
     lev := 0;
     for i from 0 to #s-1 do (
	  if s#i === " " then lev = lev+1
	  else if s#i === "\t" then lev = 8*((lev+8)//8)
	  else return (lev, replace("[[:space:]]+$","",substring(i, s)))
	  );
     (infinity, "")
     )

KeyFcns = new HashTable from {
     "Key" => (text, indents) -> Key => apply(text,value),
     "SeeAlso" => (text, indents) -> SeeAlso => apply(text,value),
     "Usage" => (text, indents) -> (
	  if #text =!= 1 then error "expected single line argument for Usage";
	  Usage => text#0)
     }
toDoc = (s) -> (
     -- perform translations to 'document' format
     -- s is a string
     s = lines s;
     t := apply(s, indentationLevel);
     s = apply(t, last);
     t = apply(t, first);
     outermost := min t;
     keypos := positions(t, x -> x === outermost);
     keypos = append(keypos,#t);
     for i from 0 to #keypos-2 list (
	  s'range := s_{keypos#i+1..keypos#(i+1)-1};
	  t'range := t_{keypos#i+1..keypos#(i+1)-1};
	  KeyFcns#(s#(keypos#i))(s'range, t'range)
	  )
     )

beginDocumentation()
document { 
	Key => SimpleDoc,
	Headline => "simpler documentation for functions and methods",
	EM "SimpleDoc", " contains a function to simplify writing documentation for functions "
	}
doc ///

Key
  (doc,String)
  doc
Headline
  a simple documentation function
Usage
  doc s
Inputs
  s:String
    See the example below for the format of {\tt s}
Consequences
  Documentation is created and stored so that after 
  @TO installPackage@ or @TO getPackage@ is used, 
  the corresponding documentation is
  available via @TO help@ and @TO viewHelp@
Description
  The sections of the documentation node must include
  Key and Headline, and may include Usage, Inputs, Outputs,
  Consequences, Description, SeeAlso.
  
  Indentation is used to determine structure: section headings
  must start in the first column. The text they refer to starts
  in the {\tt k}-th column, where {\tt k} is fixed in {\tt s}.

  In the Inputs, Outputs, and Description section, further
  indentation is used.
  
  Each section heading contains either: (1) lines of (Key),
  (2) a string (Headline, Usage), (3) marked up text (Consequences, Description).
  The sections Inputs, Outputs have slightly more structure.
  
  Each line in the section {\tt Key} is evaluated as a Macaulay2 expression
  and the various types are described in @TO Key@.  If a string is desired
  as a key, it must be surrounded by quotation marks.  The keys mentioned
  in the SeeAlso section, and also those appearing as hyperlinks in
  marked up text have the same format.

  The text in the sections {\tt Headline} and {\tt Usage}
  is a one line string.
  
  The text in Consequences and Description can be marked up as described
  below.

  The text in the sections Inputs and Outputs contains: the first line
  contains the name of the corresponding variable in the usage line, followed
  by a colon, followed by a type, for example: {\tt s:String}.  Subsequent
  further indented lines describe the variable.  These lines are strings and
  may include markup.
  
  Markups allowed include all tex-like commands as allowed in @TO TEX@.  Also
  allowed is a pair of \@ characters, enclosing Macaulay2 code, for example
  \@TO Key\@.

  EXAMPLE
    ourfcn = method()
    ourfcn ZZ := (n) -> n+1

  For a complete example and template, see the bottom of the SimpleDoc.m2 file.
SeeAlso
  document
  
///

end
restart
loadPackage "SimpleDoc"
debug SimpleDoc
indentationLevel "  	 Key"
flup = method()
flup ZZ := (n) -> -n

toDoc ///
Key
  flup
 (flup,ZZ)
Usage
  flup n
SeeAlso
   matrix
   "matrices"
///
document oo
help flup
