--		Copyright 1994 by Daniel R. Grayson

noapp := (f,x) -> error(
     "no method for applying item of class ", name class f, 
     " to item of class ", name class x
     )

Symbol Thing := (f,x) -> (
     m := lookup(f,class x);
     if m =!= null then (
	  if class m === Function then m x
	  else noapp(f,x)
	  )
     else noapp(f,x))

-----------------------------------------------------------------------------

protect Options
OptionsRegistry = new MutableHashTable

noMethod := args -> (
     if class args === Sequence 
     then if 0 < #args and #args <= 3 
     then error("no method found for items of classes ",name apply(args, class))
     else error("no method found for item of class Sequence and length ",string(#args))
     else error("no method found for item of class ", name class args)
     )

methodDefaults := new OptionTable from {
     SingleArgumentDispatch => false,
     -- ClassArgument => {},
     Associative => false,
     Options => null
     }

method = args -> processArgs(
  args,
  methodDefaults,
  (args,options) -> (
    () -> (
      if options.Options === null then (
	if options.Associative then (
	  methodFunction := newmethod1 noMethod;
	  sequenceMethod := methodFunction(Sequence) :=
	  args -> (
	    -- Common code for every associative method without options
	    if #args === 2 
	    then ((x,y) -> (
		f := lookup(methodFunction,class x,class y);
		if f === null then noMethod args
		else f(x,y))
	      ) args
	    else if #args >= 3 
	    then sequenceMethod prepend(sequenceMethod(args#0,args#1),drop(args,2))
	    else if #args === 1 then args#0
	    else if #args === 0 then noMethod args
	    else error "wrong number of arguments"
	    ))
	else if options.SingleArgumentDispatch
	then methodFunction = newmethod1 noMethod
	else (
	  if false -- options.FirstArgumentDispatch
	  then (
	    methodFunction = newmethod1 noMethod;
	    methodFunction(Sequence) :=
	    args -> (
	      -- Common code for methods that dispatch on first argument
	      -- and receive a sequence of arguments.
	      -- Using browse?  Try looking at the METHODS.
	      f := lookup(methodFunction, class args#0);
	      if f === null then noMethod args else f args
	      )
	    )
	  else (
	    methodFunction = newmethod123c(,noMethod,
		 {} -- options.ClassArgument
		 );
	    methodFunction(Sequence) := newmethod123c(
	      methodFunction, noMethod, 
	      {} -- options.ClassArgument
	      ))))
      else (
	opts := new OptionTable from options.Options;
	methodFunction = 
	args -> processArgs(args,opts,
	  -- Common code for every method with options.
	  -- Using browse?  Try looking at the METHODS.
	  (args,options) -> (
	    if #args === 1 then args = args#0;
	    f := lookup(methodFunction, class args);
	    if f === null then noMethod args
	    else f(args,options)));
	OptionsRegistry#methodFunction = opts;
	methodFunction(Sequence) := 
	(args,options) -> (
	  -- Common code for every method with options
	  if #args === 2 
	  then ((x,y) -> (
	      f := lookup(methodFunction,class x,class y);
	      if f === null then noMethod args
	      else f(x,y,options))
	    ) args
	  else if #args === 3 
	  then ((x,y,z) -> (
	      f := lookup(methodFunction,class x,class y,class z);
	      if f === null then noMethod args
	      else f(x,y,z,options))
	    ) args
	  else if #args === 1 
	  then ((x) -> (
	      f := lookup(methodFunction,class x);
	      if f === null then noMethod args
	      else f(x,options))
	    ) args
	  else if #args === 0
	  then noMethod args
	  else error "wrong number of arguments"
	  )
	);
      methodFunction
      )
    ) args
  )

OptionsRegistry#method = methodDefaults

-- document { quote ClassArgument,
--      TT "ClassArgument", " -- an option name for ", TO "method", " which
--      allows one argument to the method function to be interpreted as a
--      class in its own right.",
--      PARA,
--      NOINDENT,
--      TT "f = method(ClassArgument => {false,true})", " -- provides a list
--      of boolean values, the n-th of which specifies whether the n-th
--      argument presented to the the method function, rather than its class,
--      will participate in the search for a method function.",
--      PARA,
--      "The code above creates a function named 'f' which takes up to three 
--      arguments, looking up the appropriate method according to its arguments,
--      with inheritance.  To install a method for two arguments,
--      (x,Y), where x is of class X, use code like this:",
--      PRE "     f(X,Y) := (x,Y) -> ...",
--      "where '...' represents the body of the function you wish to install.
--      The syntax for one or three arguments is analogous.",
--      PARA,
--      "We give two examples for contrast.",
--      EXAMPLE "f = method ();",
--      EXAMPLE "f(ZZ,QQ) := print;",
--      EXAMPLE "f(3,3/2)",
--      "The method function used above was effectively found as the value of
--      ", TT "lookup(f,class 3, class 3/2)", ".",
--      EXAMPLE "g = method (ClassArgument => {false,true});",
--      EXAMPLE "g(ZZ,QQ) := print;",
--      EXAMPLE "g(3,QQ)",
--      "The method function used above was effectively found as the value of
--      ", TT "lookup(f,class 3, QQ)", ".  Notice that ", TT "QQ", ", the second
--      argument, participated directly, rather than its class.",
--      PARA,
--      "This option is incompatible with the ", TO "Associative", "option,
--      and hasn't yet been implemented in conjunction with the ", TO "Options", "
--      option.",
--      SEEALSO "method"
--      }


scan({ 
	  realPart, conjugate, imaginaryPart, borel, isBorel, codim, 
	  lcmDegree, gcdDegree, prune, euler, genera, gcdCoefficients,
	  isWellDefined, isInjective, isSurjective, singularLocus, isSubset,
	  dim, Hom, diff, contract, exteriorPower, subsets, partitions, member,
	  koszul, symmetricPower, basis, coefficientRing, trace, binomial, subquotient,
	  getchange, poincare, cover, super, poincareN, terms,
	  dual, cokernel, coimage, image, generators, someTerms, scanKeys, stats, 
	  substitute, rank, complete, ambient, top, transpose, length, baseName,
	  degree, degreeLength, coefficients, isHomogeneous, size,
	  isIsomorphism, exponents, 
	  height, depth, width, regularity, nullhomotopy,
	  hilbertFunction, content,
	  isPrime, leadTerm, leadCoefficient, leadMonomial, isField,
	  leadComponent, expand, degreesRing, degrees, annihilator,
	  chainComplex, assign, cohomology, homology, numgens,
	  autoload, ggPush, char, minprimes, relations, cone, pdim, random,
	  frac, betti, det, ring, presentation, quote use, degreesMonoid, submatrix,
	  truncate, fraction
	  },
     n -> (
	  f := method();
	  Documentation#f = n;
	  n <- f;
	  )
     )

use Thing := identity

use HashTable := x -> (
     if x.?use then x.use x; 
     x)

radical = method( Options=>{
	  Unmixed=>false,
	  CompleteIntersection => null
	  }
     )

sum = method()
product = method()
max = method(SingleArgumentDispatch=>true)
min = method(SingleArgumentDispatch=>true)
ideal = method(SingleArgumentDispatch=>true)
options = method(SingleArgumentDispatch=>true)
submodule = method(SingleArgumentDispatch=>true)
directSum = method(SingleArgumentDispatch=>true)
intersect = method(SingleArgumentDispatch=>true)
net = method(SingleArgumentDispatch=>true)
vars = method(SingleArgumentDispatch=>true)
expression = method(SingleArgumentDispatch=>true)
factor = method( Options => { } )
trim    = method ( Options => {
	  -- DegreeLimit => {}
	  } )
mingens = method ( Options => { 
	  -- DegreeLimit => {}
	  } )
hilbertPolynomial = method( Options => { Projective => true } )
width File := fileWidth; erase quote fileWidth
width Net := netWidth; erase quote netWidth
height Net := netHeight; erase quote netHeight
depth Net := netDepth; erase quote netDepth
width String := s -> #s
height String := s -> 1
depth String := s -> 0

-----------------------------------------------------------------------------

oldflatten := flatten
erase quote flatten
flatten = method(SingleArgumentDispatch=>true)
flatten List := flatten Sequence := oldflatten
coker = cokernel

source = (h) -> (
     if h#?(quote source) then h.source
     else if (class h)#?(quote source) then (class h)#?(quote source)
     else error ( name h, " of class ", name class h, " has no source" ))

target = (h) -> (
     if h.?target then h.target
     else if (class h)#?(quote target) then (class h)#?(quote target)
     else error (name h | " of class " | name class h | " has no target"))

gens = generators

-----------------------------------------------------------------------------

scanKeys(HashTable,Function) := (x,f) -> scanPairs(x, (k,v) -> f k)
scanKeys(Database,Function) := (x,f) -> (
     	  s := firstkey x;
     	  while s =!= null do (
	       f s;
	       s = nextkey x;
	       ))

oldnumerator := numerator
erase quote numerator
numerator = method()
numerator QQ := oldnumerator

olddenominator := denominator
erase quote denominator
denominator = method()
denominator QQ := olddenominator

erase quote newmethod1
erase quote newmethod123c

emptyOptionTable := new OptionTable
options Thing := X -> emptyOptionTable
options Function := function -> (
     if OptionsRegistry#?function then OptionsRegistry#function
     else emptyOptionTable
     )
options Symbol := s -> select(apply(pairs OptionsRegistry, (f,o) -> if o#?s then f), i -> i =!= null)

computeAndCache := (M,options,name,goodEnough,computeIt) -> (
     if not M#?name or not goodEnough(M#name#0,options) 
     then (
	  ret := computeIt(M,options);
	  M#name = {options,ret};
	  ret)
     else M#name#1
     )
-----------------------------------------------------------------------------

html = method(SingleArgumentDispatch=>true)
text = method(SingleArgumentDispatch=>true)
tex = method(SingleArgumentDispatch=>true)
texMath = method(SingleArgumentDispatch=>true)

-----------------------------------------------------------------------------

MarkUpList = new Type of BasicList

     MarkUpType = new Type of Type
EmptyMarkUpType = new Type of MarkUpType
     MarkUpType List := (h,y) -> new h from y
EmptyMarkUpType List := (h,y) -> if #y === 0 then new h from y else error "expected empty list"
     MarkUpType Thing := (h,y) -> new h from {y}
EmptyMarkUpType Thing := (h,y) -> error "expected empty list"

htmlMarkUpType := name -> (
     on := "<" | name | ">";
     off := "</" | name | ">";
     t -> concatenate(on, apply(t,html), off))

GlobalAssignHook MarkUpType := (X,x) -> (
     if not x.?name then (
	  x.symbol = X;
	  x.name = string X;
     	  html x := htmlMarkUpType string X;
	  );
     )

new MarkUpType := theMarkUpType -> new theMarkUpType of MarkUpList

BR         = new EmptyMarkUpType
NOINDENT   = new EmptyMarkUpType
HR         = new EmptyMarkUpType
PARA       = new MarkUpType
EXAMPLE    = new MarkUpType
TABLE      = new MarkUpType
ExampleTABLE = new MarkUpType
PRE        = new MarkUpType
TITLE      = new MarkUpType
HEAD       = new MarkUpType
BODY       = new MarkUpType
IMG	   = new MarkUpType
HTML       = new MarkUpType
CENTER     = new MarkUpType
H1         = new MarkUpType
H2         = new MarkUpType
H3         = new MarkUpType
H4         = new MarkUpType
H5         = new MarkUpType
H6         = new MarkUpType
LISTING    = new MarkUpType
XMP        = new MarkUpType
BLOCKQUOTE = new MarkUpType
VAR        = new MarkUpType
DFN        = new MarkUpType
STRONG     = new MarkUpType
BIG        = new MarkUpType
SMALL      = new MarkUpType
SAMP       = new MarkUpType
KBD        = new MarkUpType
SUB        = new MarkUpType
SUP        = new MarkUpType
ITALIC     = new MarkUpType; html ITALIC := htmlMarkUpType "I"
UNDERLINE  = new MarkUpType; html UNDERLINE := htmlMarkUpType "U"
TEX	   = new MarkUpType
SEQ	   = new MarkUpType
TT         = new MarkUpType
EM         = new MarkUpType
CITE       = new MarkUpType
BOLD       = new MarkUpType; html BOLD := htmlMarkUpType "B"
CODE       = new MarkUpType
HREF       = new MarkUpType
SHIELD     = new MarkUpType
MENU       = new MarkUpType
UL         = new MarkUpType
OL         = new MarkUpType
NL         = new MarkUpType
DL 	   = new MarkUpType
TO         = new MarkUpType

MarkUpList ^ MarkUpList := (x,y) -> SEQ{x,SUP y}
MarkUpList _ MarkUpList := (x,y) -> SEQ{x,SUB y}

-----------------------------------------------------------------------------
htmlLiteralTable := new MutableHashTable
scan(characters ascii(0 .. 255), c -> htmlLiteralTable#c = c)
htmlLiteralTable#"\"" = "&quot;"
htmlLiteralTable#"<" = "&lt;"
htmlLiteralTable#"&" = "&amp;"
htmlLiteralTable#">" = "&gt;"
htmlLiteral := s -> concatenate apply(characters s, c -> htmlLiteralTable#c)

htmlExtraLiteralTable := new MutableHashTable
scan(characters ascii(0 .. 255), c -> htmlExtraLiteralTable#c = c)
htmlExtraLiteralTable#"\"" = "&quot;"
htmlExtraLiteralTable#" " = "&nbsp;"
htmlExtraLiteralTable#"&" = "&amp;"
htmlExtraLiteralTable#"<" = "&lt;"
htmlExtraLiteralTable#">" = "&gt;"
htmlExtraLiteral := s -> concatenate apply(characters s, c -> htmlExtraLiteralTable#c)
-----------------------------------------------------------------------------
ttLiteralTable := new MutableHashTable
scan(0 .. 255, 
     c -> ttLiteralTable#(ascii{c}) = concatenate(///{\char ///, string c, "}"))
scan(characters ascii(32 .. 126), c -> ttLiteralTable#c = c)
scan(characters "\\{}$&#^_%~", 
     c -> ttLiteralTable#c = concatenate("{\\char ", string (ascii c)#0, "}"))
scan(characters "$%&#_", c -> ttLiteralTable#c = concatenate("\\",c))

cmrLiteralTable := copy ttLiteralTable

ttBreak :=
///
\leavevmode\hss\endgraf
///

(
if #newline === 1 
then ttLiteralTable#newline = ttBreak 
else if #newline === 2 then (
     ttLiteralTable#(newline#0) = "";
     ttLiteralTable#(newline#1) = ttBreak;
     )
)

ttLiteralTable#" " = ///\ ///
ttLiteralTable#"\t" = "\t"
ttLiteralTable#"`" = "{`}"     -- break ligatures ?` and !` in font \tt
                               -- see page 381 of TeX Book
ttLiteral := s -> concatenate apply(characters s, c -> ttLiteralTable#c)
-----------------------------------------------------------------------------
cmrLiteralTable#"\n" = "\n"
cmrLiteralTable#"\r" = "\r"
cmrLiteralTable#"\t" = "\t"
cmrLiteralTable#"\\" = "{\\tt \\char`\\\\}"
cmrLiteralTable# "<" = "{\\tt \\char`\\<}"
cmrLiteralTable# ">" = "{\\tt \\char`\\>}"
cmrLiteralTable# "|" = "{\\tt \\char`\\|}"
cmrLiteralTable# "{" = "{\\tt \\char`\\{}"
cmrLiteralTable# "}" = "{\\tt \\char`\\}}"
cmrLiteral := s -> concatenate apply(characters s, c -> cmrLiteralTable#c)
-----------------------------------------------------------------------------

html String := htmlLiteral
tex String := cmrLiteral
texMath String := cmrLiteral
text String := identity

texMath List := x -> concatenate("\\{", between(",", apply(x,texMath)), "\\}")
texMath Sequence := x -> concatenate("(", between(",", apply(x,texMath)), ")")

texMath Nothing := tex Nothing := html Nothing := text Nothing := x -> ""

texMath Boolean := texMath Symbol :=
tex Boolean := tex Symbol :=
text Symbol := text Boolean := 
html Symbol := html Boolean := string

net Symbol := s -> if operators#?s then operators#s else string s
File << Symbol := (o,s) -> o << net s	  -- replaces a method installed in setup.m2
erase quote operators			  -- created in name.m2

linkFilenameTable := new MutableHashTable
linkFilenameCounter := 0
linkFilenameKeys = () -> keys linkFilenameTable
linkFilename = s -> (
     if linkFilenameTable#?s 
     then linkFilenameTable#s
     else (
	  n := string linkFilenameCounter;
	  linkFilenameTable#s = n;
	  linkFilenameCounter = linkFilenameCounter + 1;
	  n)
     ) | ".html"

html MarkUpList := x -> concatenate apply(x,html)
text MarkUpList := x -> concatenate apply(x,text)
tex MarkUpList := x -> concatenate apply(x,tex)
net MarkUpList := x -> horizontalJoin apply(toList x,net)
texMath MarkUpList := x -> concatenate apply(x,tex)

--html MarkUpType := H -> html H{}
--text MarkUpType := H -> text H{}
--tex MarkUpType := H -> tex H{}
--net MarkUpType := H -> net H{}
--texMath MarkUpType := H -> tex H{}

html BR := x -> ///
<BR>
///
text BR := x -> ///
///
tex  BR := x -> ///
\hfil\break
///

html NOINDENT := x -> ""
net NOINDENT := x -> ""
text NOINDENT := x -> ""
tex  NOINDENT := x -> ///
\noindent\ignorespaces
///

html HR := x -> ///
<HR>
///
text HR := x -> ///
-----------------------------------------------------------------------------
///
tex  HR := x -> ///
\hfill\break
\line{\leaders\hrule\hfill}
///

html PARA := x -> concatenate(///
<P>
///,
     apply(x,html),
     ///
</P>
///)
tex PARA := x -> concatenate(newline, newline, apply(x,tex))
text PARA := x -> concatenate(newline, newline, apply(x,text))

text EXAMPLE := x -> concatenate apply(x,i -> text PRE i)
html EXAMPLE := x -> concatenate html ExampleTABLE apply(toList x, x -> CODE x)

text TABLE := x -> concatenate(newline, newline, apply(x, row -> (row/text, newline))) -- not good yet
text ExampleTABLE := x -> concatenate(newline, newline, apply(x, y -> (text y, newline)))

net TABLE := x -> net MatrixExpression toList x
tex TABLE := x -> concatenate applyTable(x,tex)
texMath TABLE := x -> concatenate (
     ///
\matrix{
///,
     apply(x,
	  row -> (
	       apply(row,item -> (texMath item, "&")),
	       ///\cr
///
	       )
	  ),
     ///}
///
     )

tex ExampleTABLE := x -> concatenate apply(x,tex)

html TABLE := x -> concatenate(
     newline,
     "<TABLE>",
     newline,
     apply(x, row -> ( 
	       "  <TR>",
	       newline,
	       apply(row, item -> ("    <TD ALIGN=CENTER>", html item, "</TD>",newline)),
	       "  </TR>",
	       newline)),
     "</TABLE>",
     newline
     )			 

html ExampleTABLE := x -> concatenate(
     newline,
     "<P>",
     "<CENTER>",
     "<TABLE cellspacing='0' cellpadding='12' border='4' bgcolor='#80ffff' width='100%'>",
     newline,
     apply(x, 
	  item -> (
	       "  <TR>", newline,
	       "    <TD NOWRAP>", html item, "</TD>", newline,
	       "  </TR>", newline
	       )
	  ),
     "</TABLE>",
     "</CENTER>",
     "</P>"
     )			 

text PRE   := x -> concatenate(
     newline,
     demark(newline,
	  apply(lines concatenate x, s -> concatenate("     ",s))),
     newline
     )
html PRE   := x -> concatenate( 
     "<PRE>", 
     html demark(newline,
	  apply(lines concatenate x, s -> concatenate("     ",s))),
     "</PRE>"
     )

shorten := s -> (
     while #s > 0 and s#-1 == "" do s = drop(s,-1);
     while #s > 0 and s#0 == "" do s = drop(s,1);
     s)
tex PRE := x -> concatenate (
     ///\par
\vskip 4 pt
{%
     \tt
     \baselineskip=9.5pt
///,
     between(newline, 
	  shorten lines concatenate x
	  / (line ->
	       if #line <= 81 then line
	       else concatenate(substring(line,0,71), " ..."))
	  / ttLiteral
	  / (line -> if line === "" then ///\penalty-500/// else line)
	  / (line -> (line,///\leavevmode\hss\endgraf///))
	  ),
     ///
     }
\par
\noindent
///
     )

html BODY := x -> concatenate(
     "<BODY BACKGROUND='recbg.jpg'>",
     -- "<BODY bgcolor='#e4e4ff'>",
     newline,
     apply(x, html),
     newline,
     "</BODY>",
     newline
     )

html IMG  := x -> "<IMG src=\"" | x#0 | "\">"
text IMG  := x -> ""
tex  IMG  := x -> ""

html LISTING := t -> "<LISTING>" | concatenate toSequence t | "</LISTING>";

texMath STRONG := tex STRONG := x -> concatenate("{\\bf ",apply(x,tex),"}")

texMath ITALIC := tex ITALIC := x -> concatenate("{\\sl ",apply(x,tex),"}")
html ITALIC := x -> concatenate("<I>",apply(x,html),"</I>")

texMath TEX := tex TEX := identity

texMath SEQ := tex SEQ := x -> concatenate(apply(x, tex))
text SEQ := x -> concatenate(apply(x, text))
html SEQ := x -> concatenate(apply(x, html))
net SEQ := x -> (
     x = toList x;
     p := join({-1},positions(x,i -> class i === PARA or class i === BR),{#x});
     verticalJoin apply(#p - 1, 
	  i -> horizontalJoin join(
	       if i > 0 then apply(toList x#(p#i), net) else {},
	       apply(take(x,{p#i+1, p#(i+1)-1}), net)
	       )
	  )
     )

tex Sequence := tex List := x -> concatenate("$",texMath x,"$")

text Sequence := x -> concatenate("(", between(",", apply(x,text)), ")")
text List := x -> concatenate("{", between(",", apply(x,text)), "}")

html Sequence := x -> concatenate("(", between(",", apply(x,html)), ")")
html List := x -> concatenate("{", between(",", apply(x,html)), "}")

texMath TT := tex TT := x -> concatenate(///{\tt {}///, ttLiteral concatenate x, "}")
text TT := net TT := x -> concatenate("'", x, "'")

net CODE := x -> verticalJoin lines concatenate x

html CODE   := x -> concatenate( 
     "<CODE>", 
     demark( ("<BR>",newline), apply(lines concatenate x, htmlExtraLiteral) ),
     "</CODE>"
     )

hypertex = true

html HREF := x -> (
     "<A HREF=\"" | x#0 | "\">" | x#-1 | "</A>"
     )
text HREF := x -> "\"" | x#-1 | "\""
tex HREF := x -> (
     if hypertex 
     then concatenate(
	  ///\special{html:<A href="///, ttLiteral x#0, ///">}///,
	  tex x#-1,
	  ///\special{html:</A>}///
	  )
     else (
	  if #x == 2
	  then concatenate(tex x#1, " (the URL is ", tex TT x#0, ")")
	  else tex TT x#0
	  )
     )

html SHIELD := x -> concatenate apply(x,html)
text SHIELD := x -> concatenate apply(x,text)

html TEX := x -> x#0

html MENU := x -> concatenate (
     "<MENU>", newline,
     apply(x, s -> if s =!= null then ("<LI>", html s, newline)),
     "</MENU>", newline, 
     "<P>", newline)

text MENU := x -> concatenate(
     newline,
     apply(x, s -> if s =!= null then ("    ", text s, newline))
     )

tex MENU := x -> concatenate(
     ///
\begingroup\parindent=40pt
///,
     apply(x, x -> if x =!= null then ( ///\item {$\bullet$}///, tex x, newline)),
     "\\endgroup", newline, newline)


html UL   := x -> concatenate(
     "<UL>", newline,
     apply(x, s -> ("<LI>", html s, newline)),
     "</UL>", newline, 
     "<P>", newline)

text UL   := x -> concatenate(
     newline,
     apply(x, s -> ("    ", text s, newline)))

html OL   := x -> concatenate(
     "<OL>", newline,
     apply(x,s -> ("<LI>", html s, newline)),
     "</OL>", newline, 
     "<P>", newline
     )
text OL   := x -> concatenate(
     newline,
     apply(x,s -> ("    ", text s, newline)))

html NL   := x -> concatenate(
     "<NL>", newline,
     apply(x, s -> ("<LI>", html s, newline)),
     "</NL>", newline, 
     "<P>", newline)
text NL   := x -> concatenate(
     newline,
     apply(x,s -> ("    ",text s, newline)))

html DL   := x -> (
     "<DL>" 
     | concatenate apply(x, p -> (
	       if class p === List or class p === Sequence then (
		    if # p === 2 then "<DT>" | html p#0 | "<DD>" | html p#1
		    else if # p === 1 then "<DT>" | html p#0
		    else error "expected a list of length 1 or 2"
		    )
	       else "<DT>" | html p
	       ))
     | "</DL>")	  
text DL   := x -> concatenate(
     newline, 
     newline,
     apply(x, p -> (
	       if class p === List or class p === Sequence then (
		    if # p === 2 
		    then (
			 "    ", text p#0, newline,
			 "    ", text p#1,
			 newline,
			 newline)
		    else if # p === 1 
		    then ("    ", 
			 text p#0, 
			 newline, 
			 newline)
		    else error "expected a list of length 1 or 2"
		    )
	       else ("    ", 
		    text p#0, 
		    newline, 
		    newline)
	       )),
     newline,
     newline)

ff := {"\"","\""}

text TO   := x -> concatenate ( 
     "'", formatDocumentTag x#0, "'",
     drop(toList x, 1)
     )

html TO   := x -> concatenate (
     "<A HREF=\"", linkFilename getDocumentationTag x#0, "\">", html formatDocumentTag x#0, "</A>", 
     drop(toList x,1)
     )

tex TO := x -> tex TT formatDocumentTag x#0

texMath SUP := x -> concatenate( "^{", apply(x, tex), "}" )
texMath SUB := x -> concatenate( "_{", apply(x, tex), "}" )

-----------------------------------------------------------------------------
-- start documentation here

document { quote document,
     TT "document {s, d}", " -- install documentation ", TT "d", " for 
     the topic ", TT "s", ".",
     PARA,
     "The documentation ", TT "d", " should be ", TO "hypertext", ".  The topic
     ", TT "s", " may be one of the special forms useable with ", TO "TO", ".  As
     a convenience, lists and sequences in ", TT "d", " are converted to ", TT "SEQ", "
     mark up items, and instances of ", TO "MarkUpType", ", such as ", TO "PARA", "
     are converted to empty instances of their type.",
     PARA,
     SEEALSO {"help functions"}
     }

document { quote TEST,
     TT "TEST s", " -- writes the string s to a new test file.  The
     commands in that file can be run later as a test.",
     PARA,
     "Intended for internal use only."
     }

document { quote between,
     TT "between(m,v)", " -- inserts ", TT "m", " between each pair of elements 
     of the list or sequence ", TT "v", ", returning a list.",
     }

document { quote SEEALSO,
     TT "SEEALSO {a, b, ...}", " -- inserts, into a documentation page, a sentence
     instructing the reader to see some other topics.",
     PARA,
     "The topics may have the special forms used with ", TO "TO", ".",
     SEEALSO "document"
     }

document { quote doc,
     TT "doc s", " -- provides the online documention for the topic s, in
     internal ", TO "hypertext", " form, suitable for conversion to
     text with ", TO "text", " or to html with ", TO "html", ".",
     PARA,
     EXAMPLE "doc partitions"
     }

document { quote help,
     -- no PARA in this documentation, so it all gets displayed.
     TT "help X", " -- displays the online documentation for ", TT "X", ".",
     BR, NOINDENT,
     TT "help \"Macaulay 2\"", " -- displays the base of the online documentation
     tree.",
     BR, NOINDENT,
     TT "help methods X", " -- displays help messages about the methods usable
     with things of type ", TT "X", ".",
     BR, NOINDENT,
     TT "help methods res", " -- displays help messages about the methods 
     usable with the function ", TT "res", ".",
     BR, NOINDENT,
     TT "help methods quote **", " -- displays help messages about the methods 
     usable with the operator ", TT "**", ".",
     BR, NOINDENT,
     TT "help methods (res, X)", " -- displays help messages about the 
     methods usable with the function ", TT "res", " and a thing of
     class ", TT "X", ".",
     BR, NOINDENT,
     TT "help methods (quote **, X)", " -- displays help messages about the 
     methods usable with the operator ", TT "**", " and a thing of
     class ", TT "X", ".",
     BR, NOINDENT,
     TT "help methods (X, Y)", " -- displays help messages about the 
     methods usable with a thing of class ", TT "X", " and a thing of class
     ", TT "Y", "."
     }

document { quote topicList,
     TT "topicList()", " -- provides a complete list of topics on which help 
     is available.",
     PARA,
     "Intended to be used in programs.  Users will prefer 
     to use ", TO "topics", ".",
     PARA,
     SEEALSO "help"
     }

document { quote topics,
     TT "topics  ", " -- displays a list of topics on which help is available.",
     PARA,
     "topics() -- Does the same in a function or file.",
     PARA,
     SEEALSO "help"
     }

document { quote apropos,
     TT "apropos s", " -- displays a list of global symbols which match
     the pattern specified by the string s.",
     PARA,
     "The pattern may contain '*'s as wild card characters.",
     EXAMPLE "apropos \"scan\""
     }

document { quote printExamples,
     TT "printExamples f", " -- prints out the examples of code using
     the function ", TT "f", " provided in the documentation for
     ", TT "f", ".",
     PARA,
     EXAMPLE "printExamples partition",
     SEEALSO {"examples", "document"}
     }

document { quote Documentation,
     TT "Documentation", " -- a hash table which is used to store
     pointers to documentation of functions, symbols, and methods.",
     PARA,
     "This hash table is used by the routines that display 
     documentation, is intended for internal use only, and its format may change.",
     PARA,
     "The documentation is stored both in a hash table in memory, and in a 
     database file.  Combined, the two look like a single hash table, but
     the ", TO "phase", " variable controls whether entries stored in it 
     persist to the next session.",
     PARA,
     "The key may be anything, and if the value is a string, then
     that string is taken to be the name of the thing, (which can be used for
     when printing the thing).  The search for documentation continues 
     with the name.",
     PARA,
     "The key may be anything, and if the value is a symbol, then
     the symbol is one whose value is the thing, (which can be used for
     when printing the thing), and the search for 
     documentation continues with the symbol.",
     PARA,
     "The key may be a string: if the value is a database, then the
     documentation is to be found there.  If the value is a list of
     type ", TO "SEQ", " then it's the documentation itself.",
     PARA,
     "The key may be a sequence such as ", TT "(quote +,X,Y)", "
     which is used to access the documentation installed when the method
     for adding an instance of class X to an instance of class Y was
     defined.  In this case the value is the list presented at that time,
     i.e., a list of the form ", TT "{Z, (x,y) -> ... , documentation ... }",
     ".",
     PARA,
     "The function ", TO "getDocumentationTag", " is used to chase through this
     hash table to the final key.",
     SEEALSO ":="
     }

document { quote getDocumentationTag,
     TT "getDocumentationTag s", " -- chase through the pointers in 
     the hash table ", TO "Documentation", " and return the final key, which
     is a string which can be used as a key into the documentation database.",
     PARA,
     "This function is intended for internal use only."
     }

document { quote formatDocumentTag,
     TT "formatDocumentTag x", " -- formats the tags used with ", TO "TO", " for
     display purposes in documents.",
     PARA,
     "This function is intended for internal use only."
     }

document { quote uniform,
     TT "uniform x", " -- whether all elements of the list x have the same class."
     }
document { quote newClass,
     TT "newClass(N,m)", " -- makes a copy of m with N as the new class", BR,
     TT "newClass(N,M,m)", " -- makes a copy of m with N as class and M as parent",
     PARA,
     "If m is a list, then BasicList should be an ancestor of N.  If m is 
     a hash table, then ", TT "HashTable", " should be an ancestor of N.",
     PARA,
     "If m is mutable, and instances of class N are also mutable, then
     copying is not required, and is not done.",
     PARA,
     SEEALSO { "copy", "toList" }
     }

document { quote MutableList,
     TT "MutableList", " -- the class of all mutable Lists.",
     PARA,
     "Normally the entries in a mutable list are not printed, to prevent
     infinite loops in the printing routines.  To print them out, use 
     ", TO "peek", ".",
     PARA,
     EXAMPLE {
	  "s = new MutableList from {a,b,c};",
      	  "s#2 = 1234;",
	  "s",
      	  "peek s",
	  },
     SEEALSO {"lists, arrays, and sequences", "BasicList"}
     }

document { quote lookup,
     TT "lookup", " -- a function for looking up methods.",
     PARA,
     NOINDENT,
     TT "lookup(M,A)", " -- provides the binary method named ", TT "M", " for class ", TT "A", ".
     The first place to look is ", TT "A#M", ".  The search proceeds with
     the parent of ", TT "A", ", and so on.",
     PARA,
     NOINDENT, TT "lookup(M,A,B)", " -- provides the binary method named ", TT "M", " for ", TT "(A,B)", ".
     The first place to look is ", TT "Y#(M,A,B)", " where ", TT "Y", " is the younger
     of ", TT "A", " and ", TT "B", ".  The search proceeds next with the parent of ", TT "B", ", 
     and so on.",
     PARA,
     NOINDENT, TT "lookup(M,A,B,C)", " -- provides the ternary method named ", TT "M", " for
     ", TT "(A,B,C)", ".  The first place to look is ", TT "Y#(M,A,B,C)", " where ", TT "Y", " 
     is the youngest of ", TT "A", ", ", TT "B", ", and ", TT "C", ".  The search proceeds with 
     the parent of ", TT "C", ", and so on.",
     PARA,
     "If no method is found, then ", TT "null", " is returned.",
     PARA,
     SEEALSO {"#", "classes", "installMethod"}
     }

document { quote installMethod,
     TT "installMethod", " -- a function for installing methods.",
     PARA,
     "Most users will use a different way of installing methods.",
     PARA,
     NOINDENT,
     TT "installMethod(M,A,f)", "     -- installs a function ", TT "f", " as a unary method for
     the class ", TT "A", " under the name ", TT "M", ".  This is the same as ", "M A := f", " 
     if ", TT "M", " is a function.  As currently implemented, this is also the same 
     as ", TT "A#M = f", ".",
     PARA,
     NOINDENT,
     TT "installMethod(M,A,B,f)", "   -- installs a function ", TT "f", " as a binary method for
     classes ", TT "A", " and ", TT "B", " under the name ", TT "M", ".  This is the same as 
     ", TT "M(A,B) := f", " if ", TT "M", " is a
     function, or the same as ", TT "A M B := f", " if ", TT "M", " is a binary operator. As currently
     implemented, this is also the same as ", TT "Y#(M,A,B) = f", ", where ", TT "Y", " is 
     the younger of ", TT "A", " and ", TT "B", ".",
     PARA,
     NOINDENT,
     TT "installMethod(M,A,B,C,f)", " -- installs a function ", TT "f", " as a ternary method 
     for classes ", TT "A", ", ", TT "B", ", and ", TT "C", " under the name ", TT "M", ".  
     This is the same as ", TT "M(A,B,C) := f", " if ", TT "f", "
     is a function.  As currently implemented, this is also the same as
     ", TT "Y#(M,A,B,C) = f", ", where ", TT "Y", " is the youngest of ", TT "A", ", ", TT "B", ", 
     and ", TT "C", ".",
     PARA,
     SEEALSO{"#", "lookup",  "new", "classes"}
     }
 
document { quote new,
     TT "new A of b from c", " -- make a hash table of class ", TT "A", " and 
     parent ", TT "b", " initialized from ", TT "c", ".", BR,
     NOINDENT,
     TT "new A of b", " -- make a hash table of class ", TT "A", " 
     and parent ", TT "b", ".", BR,
     NOINDENT,
     TT "new A from c", " -- make a new instance of class ", TT "A", " 
     initialized from ", TT "c", ".", BR,
     NOINDENT,
     TT "new A", " -- makes a new instance ", TT "n", " 
     of class ", TT "A", ".", BR,
     PARA,
     HR,
     NOINDENT,
     TT "new A of b from c", " -- make a hash table ", TT "n", " of
     class ", TT "AA", " and parent ", TT "b", " initialized from ", TT "c", ".",
     PARA,
     "One may use this to model the mathematical notion
     that ", TT "x", " is an element of ", TT "A", " and a subset of ", TT "b", ".
     Here ", TT "A", " and ", TT "b", " are hash tables, and ", TT "c", " is
     any expression.  Let ", TT "b", " be an instance of ", TT "B", ", ", TT "c", "
     be an instance of ", TT "C", ", and let ", TT "AA", " be an
     ancestor of ", TT "A", ".  Then use",
     PRE "          new AA of B from C := (A,b,c) -> ... ",
     "to install the corresponding optional creation routine -- the
     value it returns will be converted so its class is ", TT "A", " and its
     parent is ", TT "b", "; this will involve copying unless the returned value 
     is mutable and objects of class ", TT "A", " are mutable.",
     PARA,
     "If no installation routine has been installed, then ", TT "c", " should be
     a hash table or a list, and it will be converted directly.",
     HR,
     NOINDENT,
     TT "new A of b", " -- make a hash table of class ", TT "A", "
     and parent ", TT "b", ".",
     PARA,
     "Same as above, except ", TT "c", " is missing.  Use ",
     PRE "          new AA of B := (A,b) -> ... ",
     "to install the initialization routine.",
     HR,
     NOINDENT,
     TT "new A from c", " -- make a hash table or list ", TT "n", " of 
     class ", TT "A", " initialized from ", TT "c", ".",
     PARA,
     "The same as above except ", TT "b", " is missing.  Use ",
     PRE "          new AA from C := (A,c) -> ... ",
     "to install the corresponding initialization routine.",
     PARA,
     "Since no parent ", TT "b", " has been provided, the value returned by the
     initialization routine will not have its parent reset.  If there
     is no initialization routine the parent will be set to Nothing.",
     HR,
     NOINDENT,
     TT "new A", " -- make a new instance ", TT "n", " of 
     class ", TT "A", ".",
     PARA,
     "Same as above, except ", TT "b", " and ", TT "c", " are missing.
     Use ", TT "new AA := A -> ... ", " to install the initialization routine.",
     PARA,
     "Since no parent ", TT "b", " has been provided, the value returned by the
     initialization routine will not have its parent reset.  If there
     is no initialization routine the parent will be set to Nothing.",
     PARA,
     "Note that if the ", TT "of", " option is not used, then the class ", TT "A", "
     need not consist of hash tables or lists.  We are using this feature by
     installing a method so that ", TT "new ZZ", " returns an integer popped
     from the top of the engine's stack.",
     PARA,
     "The symbols ", TO "NewMethod", ", ", TO "NewOfMethod", ", ", 
     TO "NewFromMethod", ", and ", TO "NewOfFromMethod", " are used for 
     installation of the initialization routines.",
     SEEALSO {"classes", "of", "from"}
     }

document { quote "of",
     TT "of", " -- a keyword used with ", TO "new", "."
     }

document { quote "from",
     TT "from", " -- a keyword used with ", TO "new", "."
     }

document { quote NewMethod,
     TT "NewMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { quote NewOfMethod,
     TT "NewOfMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { quote NewFromMethod,
     TT "NewFromMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { quote NewOfFromMethod,
     TT "NewOfFromMethod", " -- a symbol used as a method name in conjuction with
     the ", TO "new", " operator."
     }

document { quote Thing,
     TT "Thing", " -- the class of all things.",
     PARA,
     "Everything in Macaulay 2 is a ", ITALIC "thing", ".  This 
     includes numbers, strings, and lists.  More complicated things such as 
     polynomials, groups, rings, and chain complexes are implemented
     as ", ITALIC "hash tables", ".  The class of all things is ", TO "Thing", ".",
     PARA,
     "The basic types of things are:", 
     MENU {
          TO "BasicList",
          TO "Boolean",
          SHIELD TO "Database",
          TO "File",
          TO "Function",
          SHIELD TO "Handle",
          TO "HashTable",
          TO "Net",
          TO "Nothing",
          TO "QQ",
          TO "RR",
          TO "Sequence",
          TO "String",
          TO "Symbol",
          TO "Thing",
          TO "ZZ"
	  },
     "Operations on things:",
     MENU {
	  TO "comparison",
	  TO "assignment"
	  }
     }

document { quote Nothing,
     TT "Nothing", " -- the empty class.",
     PARA,
     "This class is useful for representing the class of an argument
     which is missing.  It is also used as the parent for those things which
     are not themselves types, i.e., which do not have instances." 
     }

document { quote Option,
     TT "Option", " -- the class of all pairs x => y.",
     PARA,
     "Such pairs are used as optional arguments for functions.  There
     is also a way to make new hash tables with ", TO "new", " by
     providing a list of option pairs.",
     PARA,
     EXAMPLE {
	  "a => 5",
      	  "peek (a => 5)",
	  "new HashTable from {a => 5, b => 7}",
	  },
     PARA,
     "These pairs are implemented as lists, so that if ", TT "z", " is ", TT "x => y", ", then 
     ", TT "x", " is ", TT "z#0", " and ", TT "y", " is ", TT "z#1", ".",
     PARA,
     SEEALSO {"classes", "=>"}
     }

document { (NewFromMethod, HashTable, List),
     TT "new HashTable from x", " -- produce a new hash table from a
     list ", TT "x", ".",
     PARA,
     "Elements of ", TT "x", " which are options, ", TT "k => v", " cause
     the value ", TT "v", " to be stored in ", TT "x", " under the key ", TT "k", ".
     Other elements ", TT "s", " cause the value ", TT "true", " to be stored under 
     the key ", TT "s", "."
     }

document { quote OptionTable,
     TT "OptionTable", " -- the class of those hash tables which are used
     to store optional named parameters to functions.",
     SEEALSO "processArgs"
     }

document { quote processArgs,
     TT "processArgs(args,defaults,function)", " -- Here ", TT "args", " 
     is the sequence of arguments previously passed to some function 
     intended to accept optional arguments, ", TT "defaults", " is a
     hash table whose keys are the names of the optional arguments, and 
     whose values are the corresponding default values.
     The return value is obtained by evaluation of
     ", TT "function(newargs,options)", ",
     where newargs is obtained from args by removing the
     options of the form ", TT "X=>A", " (where ", TT "X", " is a
     name of an optional argument), and ", TT "options", " is a hash table
     of the same form as ", TT "defaults", " in which the default
     values have been replaced by the user-supplied values, e.g., the
     value stored under the key ", TT "X", " has been replaced by
     ", TT "A", ".  As shorthand for the option ", TT "X=>true", " the
     one may use ", TT "X", ".",
     PARA,
     EXAMPLE "processArgs((t,u,a=>4,c), new HashTable from {a=>1,b=>2,c=>false},identity)",
     SEEALSO {"OptionTable", "Option", "=>"}
     }

document { "using methods",
     "The method to be used for computing an expression such as ", TT "-x", " depends 
     on the type of Algorithm => x.  For example, the method for negating a polynomial
     differs from the method for negating an integer modulo 111.  Each
     method is a function of one variable, and is stored in the class 
     of ", TT "x", " under a key which is referred to as the name of the method.
     For some built-in methods the method name is a symbol, but for
     methods created with ", TO "method", ", the method name is the same
     as the function used for calling it up.",
     PARA,
     "Let's assume that ", TT "X", " is the class of ", TT "x", ".  The way to install a method
     for the negation of an instance ", TT "x", " of ", TT "X", " is with a statement of the 
     following form.",
     PRE "- X := x ->( ... )",
     "Here ", TT "( ... )", " represents the body of the function, consisting of
     suitable code for the operation at hand.",
     PARA,
     "The method installed by the code above is automatically inherited by
     ", TO "subclass", "es of X.  Here is a brief description of the way 
     this works.  Suppose ", TT "X", " is the ", TO "parent", " of ", TT "P", ".  When an expression
     ", TT "-p", " is to be evaluated, where the class of ", TT "p", " is ", TT "P", ", then the method for
     ", TT "-P", " is applied, unless there isn't one, in which case the method for
     ", TT "-X", " is applied, and so on, all the way up the chain of parents to the
     topmost ancestor of everything, which is called ", TO "Thing", ".",
     PARA,
     "As an extreme example of inheritance, code like", 
     PRE "- Thing := x -> ...",
     "will install a method for negating anything, which will take
     effect as a last resort whenever more a specifically defined method
     isn't found.",
     PARA,
     "The user may introduce new methods as well as new method names.  So it
     is important to understand how methods are installed and consulted.",
     PARA,
     "Applying a method named ", TT "C", " to a thing ", TT "x", " whose class is ", TT "X", " means that",
     PRE "(lookup(C,X)) x",
     "is evaluated.  In other words, ", TT "C", " is used as a key
     to obtain a function from ", TT "X", " (or its parent, grandparent,
     and so on), and the function is applied to ", TT "x", ".  See ", TO "lookup", ".",
     PARA,
     "Installing a method named ", TT "C", " for the class ", TT "X", " is done with code such
     as ",
     PRE "C X := (x) -> ( ... )",
     "where ", TT "( ... )", " represents suitable code for the operation at hand.",
     PARA,
     "Here is the routine for making new methods.",
     MENU {
	  TO "method"
	  },
     SEEALSO{"binary method", "classes", "lookup"}
     }

document { "binary method",
     "The method for computing a sum ", TT "x+y", " depends on the types of ", TT "x", " and ", TT "y", ".
     For example, the method for adding an integer ", TT "x", " and a polynomial 
     ", TT "y", " differs from the method for adding two integers modulo 111.  Because
     both the type of ", TT "x", " and the type of ", TT "y", " must enter into the selection of
     the method, we refer to these methods as binary methods.  Each binary
     method is a function of two variables, and is stored either in the class
     of ", TT "x", " or in the class of ", TT "y", ".  See also ", TO "lookup", ".",
     PARA,
     "Let's assume that ", TT "X", " is the class (or type) of ", TT "x", ", 
     and that ", TT "Y", " is the class of ", TT "y", ".  The way to install a 
     method for the addition of an instance ", TT "x", " of class ", TT "X", " to 
     an instance ", TT "y", " of class ", TT "Y", " is with a statement of the form ",
     PRE "X + Y := (x,y) -> ( ... )",
     "where ", TT "( ... )", " represents the body of the function, consisting of suitable
     code for the operation at hand.",
     PARA,
     "The method installed by the code above is automatically inherited by 
     ", TO "subclass", "es of ", TT "X", " and ", TT "Y", ".  Here is a brief
     description of the way this works.  Suppose ", TT "X", " is the 
     ", TO "parent", " of ", TT "P", " and ", TT "Y", " is the parent of X.  When 
     a sum ", TT "p+q", " is evaluated where the class of ", TT "p", " is 
     ", TT "P", " and the class of ", TT "q", " is ", TT "Q", ", then the binary
     method for ", TT "P+Q", " is applied, unless there isn't one, in which
     case the binary method for ", TT "P+Y", " is applied, unless there isn't
     one, in which case the binary method for ", TT "X+Q", " is applied,
     unless there isn't one, in which case the binary method for ", TT "P+Q", "
     is applied.  In general this search for a binary method continues all
     the way up the chain of parents to the topmost ancestor of everything,
     which is called ", TO "Thing", ".",
     PARA,
     "As an extreme example of inheritance, the code ", 
     PRE "Thing + Thing := (x,y) -> ( ... )",
     "will install a binary method for adding any two things, which will take
     effect as a last resort whenever more a specifically defined method
     isn't found.",
     PARA,
     "The ", TO "new", " function also uses a ternary lookup table to
     find the initialization function for the new thing, and should
     be thought of as a ternary operator.  The initialization function
     for a new expression created by",
     PRE "new Z of x from y",
     "is obtained as",
     PRE "lookup(NewMethod,Z,X,Y)",
     "Here ", TT "X", " is ", TT "class x", ", and ", TT "Y", " is
     ", TT "class y", ".  The initialization function can be installed 
     with",
     PRE "new Z of X from Y := (z,y) -> ...",
     "where ", TT "z", " denotes the new hash table of class ", TT "Z", " and parent
     ", TT "x", " provided to the routine by the system."
     }

document {quote OptionsRegistry,
     TT "OptionsRegistry", " -- a hash table used for recording the tables of
     option names and their default values for those functions which accpet
     optional arguments.",
     PARA,
     "If ", TT "f", " is a function which accepts optional arguments, then
     the ", TO "OptionTable", " for ", TT "f", " is stored as ", 
     TT "OptionsRegistry#f", ".",
     PARA,
     "The function ", TO "method", ", when given a table of options, will
     record them here."
     }

document { quote SingleArgumentDispatch,
     TT "SingleArgumentDispatch=>true", " -- an option to ", TO "method", "
     which specifies whether the method function should treat several
     arguments as a single argument, i.e., as a sequence.",
     PARA,
     "This allows the user to install a method for handling sequences, whereas
     normally, the types of up to the three arguments are considered.",
     EXAMPLE {
	  "f = method ( SingleArgumentDispatch => true )",
      	  "f Sequence := print",
	  "f (1,2,3)"
	  }
     }

document { quote method,
     TT "f = method()", " -- creates a method function",
     PARA,
     "Optional arguments:",
     MENU {
	  TO "Associative",
	  -- TO "ClassArgument",
	  TO "SingleArgumentDispatch",
	  TO "Options"
	  },
     PARA,
     "The code above creates a method function which takes up to three 
     arguments, looking up the appropriate method according to the classes of 
     the arguments, with inheritance.  To install a method for two arguments,
     (x,y), of classes X and Y, use code like this:",
     PRE "     f(X,Y) := (x,y) -> ...",
     "where '...' represents the body of the function you wish to install.
     The syntax for one or three arguments is analogous.  For a single
     argument x of class X, one could also write:",
     PRE "     f X := (x) -> ...",
     "the effect of which happens to be the same as that of",
     PRE "     X#f := (x) -> ...",
     PARA,
     SEEALSO {"Options", "methods", "OptionsRegistry"}
     }

document { quote Associative,
     TT "Associative", " -- an option name for ", TO "method", " which
     allows associative methods to be created.",
     PARA,
     NOINDENT,
     TT "f = method(Associative=>true)", " -- creates an associative
     method which will call upon the appropriate binary methods for its arguments 
     two at a time.",
     PARA,
     "In the following example we install a method which isn't associative
     to illustrate the order of evaluation.",
     EXAMPLE {
	  "f = method(Associative => true)",
	  ///f(String,String) := (i,j) -> "(" | i | ")," | j;///,
      	  ///f("a","b","c","d")///,
	  },
     SEEALSO "method"
     }

document { quote size,
     TT "size x", " -- returns the size of ", TT "x", " which usually gives
     a rough indication of memory space required to store the object ", TT "x", ".",
     PARA,
     "For a polynomial, the size is the number of terms."
     }
document { quote baseName,
     TT "baseName x", " -- returns the variable or symbol upon which a generator of a
     monoid or polynomial ring is based."
     }
document { quote degree,
     TT "degree X", " -- returns the degree of a polynomial, vector, 
     matrix, monomial, or module.",
     PARA,
     "The degree may be an integer, or a vector of integers.  The length
     of that vector is referred to as the 'number of degrees', and is
     provided by ", TO "degreeLength", ".",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "degree (x^2+y^2)^5",
      	  "F = R^{2,3,4}",
      	  "v = F_2",
      	  "degree v",
	  },
     "The degree of a module of finite length is the same as its length.",
     EXAMPLE "degree cokernel symmetricPower ( 2, vars R )",
     PARA,
     "Implemented with a method of the same name."
     }

document { quote degreeLength,
     TT "degreeLength x", " -- returns the number of degrees of x.",
     PARA,
     "Here x may be a ring, in which case it returns the number of degrees
     (the length of the degree vector) used in grading the ring.",
     SEEALSO "degree"
     }

document { quote coefficients,
     TT "coefficients({i,j,...},p)", " -- yields the coefficients and
     monomials of the polynomial or matrix p with respect to variables 
     numbered i, j, ... .  This has to
     be completely redone, so I don't document it further, but it is used in
     the factoring code.",
     BR,NOINDENT,
     TT "coefficients(p)", " -- yields the coefficients and monomials of
     the polynomial or matrix p with respect to all of the variables."
     }

document { quote isIsomorphism,
     TT "isIsomorphism f", " -- whether the map f of modules is an isomorphism."
     }
document { quote isHomogeneous,
     TT "isHomogeneous x", " -- whether the polynomial or ideal x is homogeneous."
     }
document { quote vars, 
     TT "vars R", " -- provides a 1 by n matrix whose entries are the
     variables of the polynomial ring R.",
     BR,
     NOINDENT,
     TT "vars(i .. j)", " -- provides a list of ", TO "indeterminates", ", the i-th
     one through the j-th one.",
     PARA,
     EXAMPLE {
	  "vars(3 .. 9)",
      	  "R = ZZ/101[vars(3 .. 5)]",
      	  "vars R",
      	  "symmetricPower(2,vars R)"
	  }
     }

document { quote leadCoefficient,
     TT "leadCoefficient f", " -- return the leading coefficient of the polynomial
     or vector f.",
     PARA,
     SEEALSO {"leadTerm", "leadMonomial", "leadComponent"}
     }

document { quote leadComponent,
     TT "leadComponent f", " -- return the leading component of the vector f,
     i.e., the integer i so that f_i is the first nonzero component of f.",
     PARA,
     SEEALSO {"leadTerm", "leadCoefficient", "leadMonomial"}
     }

document { quote leadMonomial,
     TT "leadMonomial f", " -- return the leading monomial of the polynomial
     or vector f.",
     PARA,
     SEEALSO {"leadTerm", "leadCoefficient", "leadCoefficient"}
     }

document { quote flatten,
     TT "flatten m", " -- produces a new list from m by effectively removing the braces
     surrounding the elements of any elements of m which happen to be
     lists.  Also works for matrices.",
     PARA,
     EXAMPLE "flatten {{2,3,4},{{5}},6}"
     }
document { quote coker,
     "An abbreviation for ", TO "cokernel", "."
     }
document { quote cokernel,
     TT "cokernel f", " -- produces the cokernel of the module homomorphism f",
     PARA,
     "The result will be a quotient module of the target of f.  If f is
     a ring element, it is interpreted as a one by one matrix.",
     PARA,
     "For an abbreviation, use ", TO "coker", "."
     }

document { quote image,
     TT "image h", " -- yields the image of the homomorphism h.",
     PARA,
     "The result will be a submodule of the target of h",
     PARA,
     "If h is a ring element, it is interpreted as a one by one matrix."
     }

document { quote source,
     TT "source h", " -- the source of a morphism h.",
     }

document { quote target,
     TT "target h", " -- the target of a morphism or Groebner basis.",
     }

document { quote ambient,
     TT "ambient M", " -- yields the ambient free module for the module M.",
     BR,
     NOINDENT,
     TT "ambient R", " -- yields the ambient ring of the quotient ring ", TT "R", ".
     For a Galois field it yields the ring it was constructed from.",
     PARA,
     EXAMPLE "ambient(ZZ/101[a,b]/b^3/a^3)",
     SEEALSO {"cover", "super"}
     }
     
document { quote Hom,
     TT "Hom(M,N)", " -- constructs the module of homomorphisms from M to N.",
     PARA,
     "Implemented with a method of the same name.",
     PARA,
     "Use ", TO "homomorphism", " to convert an element of the module of
     homomorphism to a matrix."
     }
document { quote gens,
     "See ", TO "generators", "."
     }
document { quote generators,
     TT "generators x", " -- produces the generators of x.",
     PARA,
     "For an abbreviation, use ", TO "gens", ".",
     PARA,
     "Produces the generators of a Groebner basis, a polynomial ring,
     a monoid ring, a free module, a free group, a submodule given by
     means of generators (or for which generators have been computed),
     or a free monoid.",
     PARA,
     "Usually the result is a list of generators, but the generators of
     a module or Groebner basis are provided as the columns in a matrix.  
     The matrix is stored in a module M under M.generators, unless the matrix
     is the identity matrix.",
     SEEALSO {"Monoid", "GroebnerBasis", "Module", "relations", "subquotient"}
     }

document { quote someTerms,
     TT "someTerms(f,i,n)", " -- selects n terms from the polynomial f, starting
     with the i-th one, and returns the resulting polynomial."
     }

document { quote scanKeys,
     TT "scanKeys(x,f)", " -- apply the function f to each key used in the
     hash table or database x.",
     PARA,
     "This function requires an immutable hash table.  To scan the pairs in
     a mutable hash table, use ", TT "scan(keys x, f)", "."
     }

document { quote GlobalAssignHook,
     TT "GlobalAssignHook", " -- a method name which is consulted when an
     assignment to a global variable occurs.",
     PARA,
     "The method should be a function of two variables: the symbol to which
     a value is being assigned, and the value being assigned.  The method
     should be stored under then name ", TT "GlobalAssignHook", " in the
     class of the value.  It will be executed just before the assignment
     occurs.",
     PARA,
     "This method is used for instances of ", TO "Type", " and ", TO "Ring", "
     to arrange for the name of the type or ring to be set to the name
     of the global variable to which it is first assigned.  The functions
     ", TO "globalAssignFunction", " and ", TO "globalReleaseFunction", " may installed
     as methods for this purpose.",
     PARA,
     EXAMPLE {
	  ///GlobalAssignHook RR := (sym,val) -> << concatenate (
     "assigning ", name val, " to ", name sym
     ) << endl///,
          "a=4.5",
      	  "a=5.4",
	  },
     SEEALSO "GlobalReleaseHook"
     }

document { quote GlobalReleaseHook,
     TT "GlobalReleaseHook", " -- a method name which is consulted when an
     assignment to a global variable is about to occur.",
     PARA,
     "The method should be a function of two variables: the symbol to which
     a value is being assigned, and the old value about to be overwritten.  
     The method should be stored under the name ", TT "GlobalReleaseHook", " in the
     class of the old value.  It is executed before the assignment occurs,
     and before the execution of ", TO "GlobalAssignHook", ".",
     PARA,
     EXAMPLE {
	  ///GlobalReleaseHook RR := (sym,val) -> << concatenate (
     "assigning ", name val, " to ", name sym
     ) << endl///,
          "a=4.5",
      	  "a=5.4",
	  },
     SEEALSO "GlobalAssignHook"
     }

document { quote stats,
     TT "stats g", " -- describe the status of a Groebner basis computation
     or of a resolution computation.",
     PARA,
     EXAMPLE {
	  "ZZ/101[a..f]",
      	  "stats gb matrix {{a*b, b*c},{a^3*f, b^3*e}}",
	  },
     SEEALSO { "GroebnerBasis", "Resolution" }
     }

document { quote complete,
     TT "complete C", " -- completely fills out the chain complex C by
     calling upon the engine to provide the maps and modules computed
     by ", TO "resolution", ".",
     PARA,
     "This is mainly intended for developers of new routines for chain
     complexes which have to make use of their internal structure.
     Before running this routine, it is not possible to determine which
     spots in a chain complex are actually occupied by modules or maps."
     }

document { quote drop,
     TT "drop(v,n)    ", " -- yields the list obtained from the list v by
     dropping the first n elements.  Also works for sequences.",
     PARA,
     "drop(v,-n)    -- yields the list obtained from the list v by dropping the 
     last n elements.",
     PARA,
     "drop(v,{m,n}) -- yields the list obtained from the list v by dropping the
     elements at positions m through n.",
     PARA,
     SEEALSO{ "take"}
     }

document { quote options,
     TT "options f", " -- returns the table of option names and default values
     provided for the function ", TT "f", ", if one has been registered.",
     BR,NOINDENT,
     TT "options X", " -- returns the options used when the monoid or polynomial
     ring X was created.",
     BR,NOINDENT,
     TT "options S", " -- returns a list of those functions which have an
     optional argument named ", TT "S", ".  Here ", TT "S", " is a symbol.",
     PARA,
     SEEALSO {"method", "OptionsRegistry"}
     }
document { (quote <<, Nothing, Thing),
     "null << x", " -- does nothing and returns ", TT "null", ".",
     PARA,
     "The intention here is that you can use ", TT "null", " as a dummy
     output file."
     }

document { (quote =>, Thing, Thing),
     TT "x => y", " -- an ", TO "Option", ", used as an optional argument with 
     some functions."
     }

