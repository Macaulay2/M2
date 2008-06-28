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
doc String := (s) -> document toDoc s

splitByIndent = (text, indents) -> (
     m := min indents;
     keypos := positions(indents, x -> x === m);
     keypos = append(keypos,#text);
     for i from 0 to #keypos-2 list (keypos#i,keypos#(i+1)-1)
     )

indentationLevel = (s) -> (
     lev := 0;
     for i from 0 to #s-1 do (
	  if s#i === " " then lev = lev+1
	  else if s#i === "\t" then lev = 8*((lev+8)//8)
	  else return (lev, replace("[[:space:]]+$","",substring(i, s)))
	  );
     (infinity, "")
     )

singleString = (key, text) -> (
     if #text =!= 1 then 
       error("expected single indented line after "|toString key);
     key => text#0)

markup = (text, indents) -> (
     s := concatenate between(" ",text);
     sp := separateRegexp(///(^|[^\])(@)///, 2, s);
     sp = apply(sp, s -> replace(///\\@///,"@",s));
     if not odd(#sp) then error "unmatched @";
     for i from 0 to #sp-1 list
	  if even i then TEX sp#i else value sp#i
     )

items = (text, indents) -> (
     apply(splitByIndent(text, indents), (i,j) -> (
	       s := text#i;
	       ps := separateRegexp("[[:space:]]*:[[:space:]]*", s);
	       if #ps =!= 2 then error("first line should contain a colon: ",s);
	       result := if i === j then "" else markup(text_{i+1..j}, indents_{i+1..j});
	       if ps#1 != "" then result = value ps#1 => result;
	       if ps#0 != "" then result = ps#0 => result;
	       result
	       ))
     )

DescriptionFcns = new HashTable from {
     "Example" => (text,indents) -> EXAMPLE text,
     "Text" => markup
     }

applySplit = (fcns, text, indents) ->
     apply(splitByIndent(text,indents), (i,j) -> (
	       key := text#i;
	       if not fcns#?key then error("unrecognized keyword: ",format key);
	       fcns#key(text_{i+1..j}, indents_{i+1..j})))

description = (text, indents) -> DIV applySplit(DescriptionFcns, text, indents)

KeyFcns = new HashTable from {
     "Key" => (text, indents) -> Key => apply(text,value),
     "SeeAlso" => (text, indents) -> SeeAlso => apply(text,value),
     "Usage" => (text, indents) -> singleString(Usage, text),
     "Headline" => (text, indents) -> singleString(Headline, text),
     "Description" => (text, indents) -> description(text,indents),
     "Caveat" => (text, indents) -> Caveat => {markup(text,indents)},
     "Consequences" => (text, indents) -> Consequences => {markup(text,indents)},
     "Inputs" => (text, indents) -> Inputs => items(text, indents),
     "Outputs" => (text, indents) -> Outputs => items(text, indents)
     }

toDoc = (text) -> (
     -- perform translations to 'document' format
     -- text is a string
     text = lines text;
     t := apply(text, indentationLevel);
     text = apply(t, last);
     indents := apply(t, first);
     applySplit(KeyFcns, text, indents)
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
 Text
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
  allowed is a pair of {\tt \@} characters, enclosing Macaulay2 code, for example
  {\tt \@TO Key\@}.

 Example
   ourfcn = method()
   ourfcn ZZ := (n) -> n+1
   ourfcn 3
 Text
  For a complete example and template, see the bottom of the SimpleDoc.m2 file.
SeeAlso
  document
  
///

end
restart
loadPackage "SimpleDoc"
installPackage SimpleDoc
viewHelp doc
debug SimpleDoc
flup = method()
flup ZZ := (n) -> -n
D = toDoc get "doc.eg"
document D
help flup
