-- -*- coding: utf-8 -*-
newPackage(
	"SimpleDoc",
    	Version => "1.1", 
    	Date => "December 1, 2009",
	AuxiliaryFiles=>true,
    	Authors => {
	     {Name => "Dan Grayson", 
		  Email => "dan@math.uiuc.edu", 
		  HomePage => "http://www.math.uiuc.edu/~grayson/"},
	     {Name => "Mike Stillman", 
		  Email => "mike@math.cornell.edu", 
		  HomePage => "http://www.math.cornell.edu/~mike/"}},
    	Headline => "a simple documentation function",
     	PackageImports => { "Text" },
    	DebuggingMode => false
    	)

export {"multidoc", "doc", "docTemplate", "docExample", "packageTemplate", "simpleDocFrob"}

simpleDocFrob = method()
simpleDocFrob(ZZ,Matrix) := (n,M) -> directSum(n:M)

-- We represent a line of text by a triple (text,indent,linenum) where 
--   text : String     	    the content of the line, with indentation removed, or null if the line was empty (?)
--   indent : ZZ     	    the number of spaces of indentation removed, or infinity if the line was empty
--   linenum : ZZ     	    the source line number
-- We use these access functions uniformly:
getText = textline -> textline#0
getIndent = textline -> textline#1
getLinenum = textline -> textline#2
-- We use this creation function:
makeTextline = (text,indent,linenum) -> (text,indent,linenum)

splitByIndent = (textlines,empties) -> (
     indents := for x in textlines list getIndent x;
     if empties then indents = apply(indents, n -> if n === infinity then -1 else n);
     m := infinity;
     indents = append(indents,infinity);
     r := for i from 0 to #indents-1 list if indents#i >= m+1 then continue else (m = indents#i; i);
     r = append(r, #indents-1);
     apply(#r - 1, i -> (r#i,r#(i+1)-1)))

indentationLevel = (s) -> (
     lev := 0;
     for i from 0 to #s-1 do (
	  c := s#i;
	  if c === " " then lev = lev+1
	  else if c === "\t" then lev = 8*((lev+8)//8)
	  else if c === "\r" then lev = 0
	  else return (lev, replace("[[:space:]]+$","",substring(i, s)))
	  );
     (infinity, "")
     )

singleString = (key, textlines, keylinenum) -> (
     if #textlines === 0 then
       error("line ",toString keylinenum," of string: expected single indented line after "|toString key)
     else if #textlines > 1 then 
       error("line ",toString getLinenum textlines#1," of string: expected single indented line after "|toString key);
     key => getText textlines#0)

multiString = (key, textlines, keylinenum) -> (				    -- written by Andrew Hoefel originally
     if #textlines === 0 then
       error("line ",toString keylinenum," of string: expected at least one indented line after "|toString key);
     key => concatenate between(newline, getText \ textlines)
     )

listOfStrings = (key, textlines, keylinenum) -> (
     key => getText \ textlines
     )

reassemble = (indent,textlines) -> concatenate between(newline,
     for x in textlines list ( if getIndent x =!= infinity then getIndent x - indent : " ", getText x )
     )

safevalue = t -> (
     try value t else (
	  stderr << "in the evaluation of: " << stack lines t << endl;
	  value t
	  )
     )

markup2 = (textlines, keylinenum) -> (
     if #textlines === 0 then return "";
     s := concatenate between(" ",getText \ textlines);
     sp := separateRegexp(///(^|[^\\])(@)///, 2, s);
     sp = apply(sp, s -> replace(///\\@///,"@",s));
     if not odd(#sp) then error "unmatched @";
     t := for i from 0 to #sp-1 list if even i then if sp#i != "" then TEX sp#i else "" else safevalue concatenate("(",sp#i,")");
     t = select(t, x -> x =!= "");
     if instance(t,List) and #t === 1 then t = first t;
     t
     )

markup = (textlines, keylinenum) -> (
     textlines = prepend(makeTextline("",infinity,if #textlines == 0 then "unknown" else getLinenum first textlines - 1), textlines);
     splits := splitByIndent(textlines,true);
     DIV apply(splits, (i,j) -> (
	       x := markup2(textlines_{i+1..j},getLinenum textlines#i);
	       if not (instance(x,HypertextContainer) or instance(x,HypertextParagraph)) then x = PARA x;
	       x)))
    
items = (textlines, keylinenum) -> (
     apply(splitByIndent(textlines, false), (i,j) -> (
	       s := getText textlines#i;
	       ps := separateRegexp("[[:space:]]*:[[:space:]]*", s);
	       if #ps =!= 2 then (
	       	    val := value;
	       	    ps = separateRegexp("[[:space:]]*=>[[:space:]]*", s);
	       	    if #ps =!= 2 then error("line ",toString getLinenum textlines#i," of string: expected line containing a colon or a double arrow");
		    )
	       else (
		    val = identity;
		    );
	       result := if i === j then "" else markup2(textlines_{i+1..j}, getLinenum textlines#i);
	       if ps#1 != "" then result = value ps#1 => result;
	       if ps#0 != "" then result = val ps#0 => result;
	       result
	       ))
     )

DescriptionFunctions = new HashTable from {
     "Example" => (textlines,keylinenum) -> EXAMPLE apply(
	  splitByIndent(textlines,false),
	  (i,j) -> reassemble(getIndent textlines#0, take(textlines, {i,j}))),
     "CannedExample" => (textlines,keylinenum) -> EXAMPLE { PRE reassemble(getIndent textlines#0, textlines) },
     "Text" => markup,
     "Pre" => (textlines, keylinenum) -> PRE reassemble(min\\getIndent\textlines, textlines),
     "Code" => (textlines, keylinenum) -> ( 
	  m := min\\getIndent\textlines; 
	  value concatenate ("(",apply(textlines, x -> (getIndent x - m, getText x, "\n")),")"))
     }

applySplit = (fcns, textlines) ->
     apply(splitByIndent(textlines,false), (i,j) -> (
	       key := getText textlines#i;
	       if not fcns#?key then error splice(
		    "unrecognized keyword, line ",toString getLinenum textlines#i,
		    " of string: ",format key,
		    "; expected: ", toSequence between(" ",sort keys fcns)
		    );
	       fcns#key(textlines_{i+1..j}, getLinenum textlines#i)))

description = (textlines, keylinenum) -> toSequence applySplit(DescriptionFunctions, textlines)
nonnull = x -> select(x, i -> i =!= null)

ConsequencesFuntions = new HashTable from {
     "Item" => (textlines, keylinenum) -> markup(textlines, keylinenum)
     }

KeyFunctions = new HashTable from {
     "Key" => (textlines, keylinenum) -> Key => (
	  r := nonnull apply(getText \ textlines,value);
	  if length r == 0 then error("Key (line ",toString keylinenum," of string): expected at least one key");
	  r),
     "SeeAlso" => (textlines, keylinenum) -> SeeAlso => apply(select(getText\textlines,p -> #p>0),value),
     "Subnodes" => (textlines, keylinenum) -> Subnodes => apply(getText\textlines, p -> if match("^:",p) then substring(1,p) else TO value p),
     "Usage" => (textlines, keylinenum) -> multiString(Usage, textlines, keylinenum),
     "Headline" => (textlines, keylinenum) -> singleString(Headline, textlines, keylinenum),
     "Description" => (textlines, keylinenum) -> description(textlines, keylinenum),
     "Caveat" => (textlines, keylinenum) -> Caveat => {markup(textlines, keylinenum)},
     "Consequences" => (textlines, keylinenum) -> Consequences => applySplit(ConsequencesFuntions, textlines),
     "Inputs" => (textlines, keylinenum) -> Inputs => items(textlines, keylinenum),
     "Outputs" => (textlines, keylinenum) -> Outputs => items(textlines, keylinenum),
     "ExampleFiles" => (textlines, keylinenum) -> listOfStrings(ExampleFiles,textlines, keylinenum)
     }

NodeFunctions = new HashTable from {
     "Node" => (textlines, keylinenum) -> (
	  r := deepSplice applySplit(KeyFunctions, textlines);
	  if any(r, i -> member(first i,{Inputs,Outputs}))
	  and not any(r, i -> first i === Usage)
	  then error("multidoc node, line ",toString keylinenum," of string: Inputs or Outputs specified, but Usage not provided");
	  r)
     }

toDoc = (funtab,text) -> (
     text = lines text;
     text = apply(text,1 .. #text,identity);		    -- append line numbers
     text = select(text, (l,n) -> not match("^[[:space:]]*--",l));
     linenums := apply(text,last);
     text = apply(text,first);
     t := apply(text, indentationLevel);
     text = apply(t, last);
     indents := apply(t, first);
     textlines := for i from 0 to #text-1 list makeTextline(text#i,indents#i,linenums#i);
     deepSplice applySplit(funtab, textlines))

doc = method()
doc String := (s) -> document toDoc(KeyFunctions,s)

multidoc = method()
multidoc String := (s) -> document \ toDoc(NodeFunctions,s)

docExample = "doc ///
  Key
    (simpleDocFrob,ZZ,Matrix)
  Headline
    a sample documentation node
  Usage
    x = simpleDocFrob(n,M)
  Inputs
    n:ZZ
      positive
    M:Matrix
      which is square
  Outputs
    x:Matrix
      A block diagonal matrix with {\\tt n} 
      copies of {\\tt M} along the diagonal
  Consequences
   Item
    The first side effect of the function, if any, is described here.
   Item
    The second side effect of the function is described here, and so on.
  Description
   Text
     Each paragraph of text begins with \"Text\".  The following 
     line starts a sequence of Macaulay2 example input lines.
     However, see @TO (matrix,List)@.
     
     The output in the following example was automatically generated at the time
     of package installation.
   Example
     M = matrix\"1,2;3,4\";
     simpleDocFrob(3,M)
   Text
     The following example was generated by the documentation author.
   CannedExample
      i1 : 4+
	   4

      o1 = 8

      i1000 = 2+2

      o1000 = 4
   Text
     See @ TO \"docExample\" @ for the code used to create this documentation.
  Caveat
    This is not a particularly useful function.
  SeeAlso
    matrix
    (directSum,List)
///"

docTemplate = "
doc ///
   Key
   Headline
   Usage
   Inputs
   Outputs
   Consequences
    Item
   Description
    Text
    Code
    Pre
    Example
    CannedExample
   Subnodes
   Caveat
   SeeAlso
///
"


packagetemplate = "
newPackage(
	\"%%NAME%%\",
    	Version => \"0.1\", 
    	Date => \"\",
    	Authors => {{Name => \"\", 
		  Email => \"\", 
		  HomePage => \"\"}},
    	Headline => \"\",
    	DebuggingMode => false
    	)

export {}

-- Code here

beginDocumentation()

doc ///
Key
  %%NAME%%
Headline
Description
  Text
  Example
Caveat
SeeAlso
///

doc ///
Key
Headline
Usage
Inputs
Outputs
Consequences
Description
  Text
  Example
  Code
  Pre
Caveat
SeeAlso
///

TEST ///
-- test code and assertions here
-- may have as many TEST sections as needed
///
"

packageTemplate = method()
packageTemplate String := (packagename) -> 
     replace("%%NAME%%", packagename, packagetemplate)

beginDocumentation()

multidoc get (currentFileDirectory | "SimpleDoc/doc.txt")

value docExample

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=SimpleDoc RemakePackages=true RemakeAllDocumentation=true IgnoreExampleErrors=false RerunExamples=true"
-- End:
