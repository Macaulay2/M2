--		Copyright 1994 by Daniel R. Grayson

noapp := (f,x) -> error(
     "no method for applying item of class ", name class f, 
     " to item of class ", name class x
     )

--Symbol Thing := (f,x) -> (
--     m := lookup(f,class x);
--     if m =!= null then (
--	  if class m === Function then m x
--	  else noapp(f,x)
--	  )
--     else noapp(f,x))

-----------------------------------------------------------------------------

protect Options

noMethod := args -> (
     if class args === Sequence 
     then if 0 < #args and #args <= 3 
     then error("no method found for items of classes ",name apply(args, class))
     else error("no method found for item of class Sequence and length ",string(#args))
     else error("no method found for item of class ", name class args)
     )

methodDefaults := new OptionTable from {
     SingleArgumentDispatch => false,
     Associative => false,
     Options => null
     }

method = args -> processArgs(
  args,
  methodDefaults,
  options -> () -> (
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
	    methodFunction = newmethod123c(,noMethod, {});
	    methodFunction(Sequence) := newmethod123c( methodFunction, noMethod, {} ))))
      else (
	opts := new OptionTable from options.Options;
	methodFunction = 
	args -> processArgs(args,opts,
	  -- Common code for every method with options.
	  -- Using browse?  Try looking at the METHODS.
	  options -> args -> (
	    f := lookup(methodFunction, class args);
	    if f === null then noMethod args
	    else (f options)(args)));
	OptionsRegistry#methodFunction = opts;
	methodFunction(Sequence) := options -> args -> (
	  -- Common code for every method with options
	  if #args === 2 
	  then ((x,y) -> (
	      f := lookup(methodFunction,class x,class y);
	      if f === null then noMethod args
	      else (f options)(x,y))
	    ) args
	  else if #args === 3 
	  then ((x,y,z) -> (
	      f := lookup(methodFunction,class x,class y,class z);
	      if f === null then noMethod args else (f options)(x,y,z))
	    ) args
	  else if #args === 1 
	  then ((x) -> (
	      f := lookup(methodFunction,class x);
	      if f === null then noMethod args else (f options)(x))
	    ) args
	  else if #args === 0
	  then noMethod args
	  else error "wrong number of arguments"
	  )
	);
      methodFunction
      )
  )

OptionsRegistry#method = methodDefaults

scan({ 
	  realPart, conjugate, imaginaryPart, borel, isBorel, codim, 
	  lcmDegree, gcdDegree, prune, euler, genera, gcdCoefficients,
	  isWellDefined, isInjective, isSurjective, singularLocus, isSubset,
	  dim, Hom, diff, contract, exteriorPower, subsets, partitions, member,
	  koszul, symmetricPower, basis, coefficientRing, trace, binomial, subquotient,
	  getchange, poincare, cover, super, poincareN, terms,
	  dual, cokernel, coimage, image, generators, someTerms, scanKeys, scanValues, stats, 
	  substitute, rank, complete, ambient, top, transpose, length, baseName,
	  degree, degreeLength, coefficients, isHomogeneous, size,
	  isIsomorphism, exponents, 
	  height, depth, width, regularity, nullhomotopy,
	  hilbertFunction, content, isUnit, monoid,
	  isPrime, leadTerm, leadCoefficient, leadMonomial, isField,
	  leadComponent, degreesRing, newDegreesRing, degrees, annihilator,
	  chainComplex, assign, numgens,
	  autoload, ggPush, char, minprimes, relations, cone, pdim, random,
	  frac, betti, det, ring, presentation, quote use, degreesMonoid, newDegreesMonoid, submatrix,
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

cohomology = method( Options => { 
	  Degree => 0		  -- for local cohomology and sheaf cohomology
	  } )
homology = method( Options => { } )

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

scanValues(HashTable,Function) := (x,f) -> scanPairs(x, (k,v) -> f v)

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
mathML = method(SingleArgumentDispatch=>true)

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

html EmptyMarkUpType := html MarkUpType := X -> html X{}

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
LITERAL    = new MarkUpType; html LITERAL := x -> x#0	    -- our own invention
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
TEX	   = new MarkUpType; html TEX := x -> x#0	    -- should do something else!

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
mathML String := htmlLiteral
tex String := cmrLiteral
texMath String := cmrLiteral
text String := identity

texMath List := x -> concatenate("\\{", between(",", apply(x,texMath)), "\\}")
texMath Sequence := x -> concatenate("(", between(",", apply(x,texMath)), ")")

mathML Nothing := texMath Nothing := tex Nothing := html Nothing := text Nothing := x -> ""

mathML Symbol := x -> concatenate("<ci>",string x,"</ci>")

texMath Boolean := texMath Symbol := 
tex Boolean := tex Symbol :=
text Symbol := text Boolean := 
html Symbol := html Boolean := string

net Symbol := s -> (
     if operators#?s then operators#s 
     -- else if s =!= value s then concatenate("quote ", string s)
     else string s
     )
File << Symbol := (o,s) -> o << net s	  -- replaces a method installed in setup.m2
erase quote operators			  -- created in name.m2

html MarkUpList := x -> concatenate apply(x,html)
text MarkUpList := x -> concatenate apply(x,text)
tex MarkUpList := x -> concatenate apply(x,tex)
net MarkUpList := x -> peek x
texMath MarkUpList := x -> concatenate apply(x,texMath)
mathML MarkUpList := x -> concatenate apply(x,mathML)

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

html PARA := x -> (
     if #x === 0 
     then ///
<P>
///
     else concatenate(///
<P>
///,
          apply(x,html),
          ///
</P>
///
          )
     )

tex PARA := x -> concatenate(///
\par
///,
     apply(x,tex))

text PARA := x -> concatenate(newline, newline, apply(x,text))

text EXAMPLE := x -> concatenate apply(x,i -> text PRE i)
html EXAMPLE := x -> concatenate html ExampleTABLE apply(toList x, x -> {x, CODE concatenate("in = ",x)})

text TABLE := x -> concatenate(newline, newline, apply(x, row -> (row/text, newline))) -- not good yet
text ExampleTABLE := x -> concatenate(newline, newline, apply(x, y -> (text y#1, newline)))
net ExampleTABLE := x -> stack between("",apply(toList x#1, net))

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

tex ExampleTABLE := x -> concatenate apply(x,y -> tex y#1)

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
	       "    <TD NOWRAP>", html item#1, "</TD>", newline,
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
     "<BODY bgcolor='#e4e4ff'>",
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
     stack apply(#p - 1, 
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

net CODE := x -> stack lines concatenate x

html CODE   := x -> concatenate( 
     "<CODE>", 
     demark( ("<BR>",newline), apply(lines concatenate x, htmlExtraLiteral) ),
     "</CODE>"
     )

html HREF := x -> (
     "<A HREF=\"" | x#0 | "\">" | html x#-1 | "</A>"
     )
text HREF := x -> "\"" | x#-1 | "\""
tex HREF := x -> (
--   if hypertex then 
     concatenate(
	  ///\special{html:<A href="///, ttLiteral x#0, ///">}///,
	  tex x#-1,
	  ///\special{html:</A>}///
	  )
--     else (
--	  if #x == 2
--	  then concatenate(tex x#1, " (the URL is ", tex TT x#0, ")")
--	  else tex TT x#0
--	  )
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
     "<A HREF=\"", 
     -- linkFilename getDocumentationTag x#0,
     "\">", html formatDocumentTag x#0, "</A>", 
     drop(toList x,1)
     )

tex TO := x -> tex TT formatDocumentTag x#0

texMath SUP := x -> concatenate( "^{", apply(x, tex), "}" )
texMath SUB := x -> concatenate( "_{", apply(x, tex), "}" )

oldexit := exit
erase quote exit
exit = method(SingleArgumentDispatch => true)
exit ZZ := i -> oldexit i
exit Sequence := () -> oldexit 0
exit = new Command from exit
quit = new Command from (() -> oldexit 0)
