--		Copyright 2007 by Daniel R. Grayson

-- Default rendering is by concatenation of rendered inputs
setupRenderer(mathML, concatenate, Hypertext)

moen := name -> concatenate("<mo>&",name,";</mo>")
nest := (tag,s) -> concatenate("<",tag,">",s,"</",tag,">")
mo   := s -> nest("mo",s)
mrow := s -> nest("mrow",s)
mtable := x -> concatenate(
     "<mtable align=\"baseline1\">", newline,
     apply(x, row -> ( "<mtr>", apply(row, e -> ("<mtd>",e,"</mtd>",newline)), "</mtr>", newline ) ),
     "</mtable>", newline )
mtableML := x -> mtable applyTable(x,mathML)
mtableMLcol := x -> mtableML apply(x, row -> {row})
-- see texmacs-1.0.6.12/share/Macaulay2/Core/m2/texhtml.m2
-- NN.mathmL = "<mi>\u2115</mi>";
-- PP.mathML = "<mi>\u2119</mi>";
ZZ.mathML = "<mi>\u2124</mi>";
RR.mathML = "<mi>\u211d</mi>";
QQ.mathML = "<mi>\u211a</mi>";
CC.mathML = "<mi>\u2102</mi>";

mathML Net := x -> mtableMLcol unstack x
mathML String := x -> (
     if match("\n",x) 
     then mathML net x
     else concatenate("<mtext>",replace(" ", "&nbsp;", htmlLiteral x),"</mtext>")
     )
mathML Nothing := texMath Nothing := tex Nothing := html Nothing := x -> ""
mathML Symbol := x -> concatenate("<mi>",toString x,"</mi>")
mathML InfiniteNumber := i -> if i === infinity then "<mrow><mi>&infin;</mi></mrow>" else "<mo>-</mo><mi>&infin;</mi>"
mathML Array := s -> concatenate ( "<mrow><mo>[</mo><mrow>", between("<mo>,</mo>",mathML \ toList s), "</mrow><mo>]</mo></mrow>" )
mathML Set := x -> mrow (mathML "set", mathML keys x)
mathML Sequence := s -> concatenate ( "<mrow><mo>(</mo><mrow>", between("<mo>,</mo>",mathML \ toList s), "</mrow><mo>)</mo></mrow>" )
mathML List := s -> concatenate ( "<mrow><mo>{</mo><mrow>", between("<mo>,</mo>",mathML \ toList s), "</mrow><mo>}</mo></mrow>" )
mathML VerticalList := s -> concatenate( "<mrow><mo>{</mo>", mtableMLcol s, "<mo>}</mo></mrow>", newline )
mathML Holder := v -> mathML v#0
-- mathML Holder2 := v -> mathML v#0
mathML Adjacent := x -> concatenate("<mrow>",mathML x#0,  mathML x#1, "</mrow>")
mathML Adjacent :=
mathML FunctionApplication := m -> (
     p := precedence m;
     fun := m#0;
     mfun := mathML fun;
     args := m#1;
     margs := mathML args;
     if precedence args >= p
     then if precedence fun > p
     then (
	  if class mfun === Net and mfun#?0 and width mfun > width mfun#0
	  then concatenate (mfun, margs)
	  else concatenate (mfun, " ", margs)
	  )
     else concatenate (bigParenthesize mfun, margs)
     else if precedence fun > p
     then concatenate (mfun, bigParenthesize margs)
     else concatenate (bigParenthesize mfun, bigParenthesize margs)
     )
mathML MatrixExpression := x -> concatenate( "<mrow><mo>(</mo>", mtableML x#0, "<mo>)</mo></mrow>", newline )
mathML Minus := v -> concatenate( "<mo>-</mo>", mathML v#0)
mathML Divide := x -> concatenate("<mfrac>", mathML x#0, mathML x#1, "</mfrac>")
mathML OneExpression := x -> "<mn>1</mn>"
mathML ZeroExpression := x -> "<mn>0</mn>"
mathML Sum := v -> (
     n := # v;
     if n === 0 then "<mn>0</mn>"
     else if n === 1 then mathML v#0
     else (
	  p := precedence v;
	  seps := newClass(MutableList, apply(n+1, i->"<mo>+</mo>"));
	  seps#0 = "";
	  seps#n = "";
	  v = apply(n, i -> (
		    if class v#i === Minus 
		    then (
			 seps#i = "<mo>-</mo>"; 
			 v#i#0)
		    else v#i));
	  concatenate (
	       "<mrow>", 
	       mingle(seps, 
	       	    apply(n, i -> 
		    	 if precedence v#i <= p 
		    	 then ("<mrow><mo>(</mo>", mathML v#i, "<mo>)</mo></mrow>")
		    	 else mathML v#i),
		    n:newline),
	       "</mrow>",newline)))
mathML Product := v -> (
     n := # v;
     if n === 0 then "<mn>1</mn>"
     else if n === 1 then mathML v#0
     else (
     	  p := precedence v;
	  seps := newClass(MutableList, (n+1) : "<mo>*</mo>");
	  if n>1 and isNumber v#0 and startsWithSymbol v#1 then seps#1 = "<mo></mo>";
     	  boxes := apply(#v,
	       i -> (
		    term := v#i;
		    nterm := mathML term;
	       	    if precedence term <= p then (
			 seps#i = seps#(i+1) = "<mo></mo>";
			 nterm = ("<mrow><mo>(</mo>", nterm, "<mo>)</mo></mrow>");
			 );
		    if class term === Power
		    and not (term#1 === 1 or term#1 === ONE)
		    or class term === Subscript then (
			 seps#(i+1) = "<mo></mo>";
			 );
	       	    nterm));
     	  seps#0 = seps#n = "";
	  concatenate ("<mrow>", mingle (seps, boxes), "</mrow>")
	  )
     )
mathMLparen = s -> concatenate("<mrow><mo>(</mo>",s,"<mo>)</mo></mrow>")
mathML Boolean := i -> if i then "<mi>true</mi>" else "<mi>false</mi>"
mathML Subscript := v -> concatenate("<msub>",mathML v#0,mathML v#1,"</msub>")
mathML Superscript := 
mathML Power := v -> (
     if v#1 === 1 then mathML v#0
     else (
	  p := precedence v;
	  x := mathML v#0;
	  y := mathML v#1;
	  if precedence v#0 <  p then x = mathMLparen x;
	  concatenate("<msup>",x,y,"</msup>")))
mathML ZZ := i -> concatenate("<mn>",toString i, "</mn>")
mathML RR := i -> concatenate("<mn>",toString i, "</mn>")
mathML QQ := i -> concatenate( "<mfrac>",mathML numerator i, mathML denominator i, "</mfrac>" )
mathML Expression := x -> error("mathML conversion for expression class ", toString class x, " not implemented yet")

mathML LITERAL := x -> concatenate x

-- see texmacs source file HTMLsymbol.scm for these names:
leftarrow := moen "larr"
doublerightarrow := moen "rArr"
leftbrace := mo "{"
rightbrace := mo "}"

mathML ChainComplex := C -> (
     complete C;
     s := sort spots C;
     if #s === 0 then mathML "0"
     else mtable transpose between({leftarrow,"",""}, toList apply(s#0 .. s#-1,i -> {mathML C_i,"",mathML i})))
mathML Option := s -> concatenate("<mrow>",mathML s#0, doublerightarrow, mathML s#1, "</mrow>")
mathML Type :=
mathML ImmutableType := R -> if R.?mathML then R.mathML else mathML expression R
mathML VirtualTally :=
mathML HashTable := s -> if s.?mathML then s.mathML else concatenate( "<mrow>",mathML class s,leftbrace, mtable sort apply(pairs s, (k,v) -> {mathML k, doublerightarrow, mathML v}), rightbrace,"</mrow>", newline )
mathML MutableHashTable := x -> if x.?mathML then x.mathML else (
     if hasAttribute(x,ReverseDictionary) then mathML getAttribute(x,ReverseDictionary)
     else mrow ( mathML class x, nest("mtext",if #x > 0 then ("{...", toString(#x), "...}") else "{}" )))
mathML BettiTally := v -> mtableML rawBettiTally v
mathML CacheFunction := f -> mathML "-*a cache function*-"
-- these are all provisional:
mathML CoherentSheaf :=
mathML Ideal :=
mathML ImmutableType :=
mathML Matrix :=
mathML MonoidElement :=
mathML MutableMatrix :=
mathML OptionTable :=
mathML ProjectiveHilbertPolynomial :=
mathML RingMap :=
mathML SheafOfRings :=
mathML ChainComplexMap :=
mathML GradedModule :=
mathML GradedModuleMap :=
mathML GroebnerBasis :=
mathML IndexedVariableTable :=
mathML Package :=
mathML Resolution :=
mathML ScriptedFunctor :=
mathML Monoid :=
mathML Variety :=
mathML Thing := x -> (
     -- maybe "expression" should not just put unknown things in a holder ...
     -- anyway, we have to break the loop somehow here
     if hasAttribute(x,ReverseDictionary) then mathML return mathML getAttribute(x,ReverseDictionary);
     y := expression x;
     if instance(y,Holder) and y#0 === x then mathML toString x else mathML y)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
