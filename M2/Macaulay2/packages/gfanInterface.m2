
newPackage(
	"gfanInterface",
    	Version => "0.2.1", 
    	Date => "Feb 15, 2010",
    	Authors => {
	     {Name => "Mike Stillman", Email => "mike@math.cornell.edu", HomePage => ""},
	     {Name => "Andrew Hoefel", Email => "handrew@mathstat.dal.ca", HomePage => ""}
	     },
    	Headline => "Interface to A. Jensen's gfan package",
			Configuration => { 
			 "path" => "",
			 "fig2devpath" => "",
	     "keep files" => true
	      },
    	DebuggingMode => false,
	CacheExampleOutput => true,
	AuxiliaryFiles => true,
	PackageExports => {"Polymake"}
    	)

export {
     "gfan", "weightVector", "initialIdeal", "groebnerCone", "groebnerFan", "universalGroebnerBasis", 
     "renderStaircase", "render", "Symmetries"
     }

gfan'path = gfanInterface#Options#Configuration#"path"
if gfan'path == "" then gfan'path = prefixDirectory | currentLayout#"programs"
if instance(gfan'path, String) then gfan'path = {gfan'path}
if not instance(gfan'path,List) then error "expected configuration option gfan'path to be a list of strings or a string"
gfan'path = join(
     gfan'path,
     separate(":",getenv "PATH"),
     {
	  "/usr/libexec/gfan/" -- Fedora has gfan as a separate package; the programs gfan_* are here, but gfan is in /usr/bin
	  }
     )
findProgram = pgmname -> (
     r := searchPath(gfan'path, pgmname);
     if length r == 0 then error("program ",pgmname," not found on path ",gfan'path);
     if debugLevel == 101 then stderr << "--" << pgmname << " found in: " << stack r << endl;
     r#0 | "/" | pgmname)

fig2dev'path = gfanInterface#Options#Configuration#"fig2devpath"

needsPackage "FourierMotzkin"

writePolynomial = f -> (
	E := exponents f;
	C := flatten entries (coefficients f)_1;
	concatenate apply(#E, i -> writeMonomial(C_i, E_i))
)

writeMonomial = (coeff, expList) -> (
	coeffStr := toExternalString coeff;
	if coeff > 0 then (
		coeffStr = "+" | coeffStr
	);
	coeffStr | concatenate apply(#expList, i-> "*" | writeVariable(i, #expList) | "^" |expList_i)
)

writeVariable = (i,n) -> (
	numDigits := # toExternalString n;
	numString := toExternalString i;
	paddingZeros := concatenate apply(numDigits - #numString, j -> "0");
	"g_" | paddingZeros | numString
)

wtvec = (inL,L) -> (
  W := flatten apply(#inL, i -> (
    f := inL#i;
    g := L#i;
    m := first exponents(f);
    apply(exponents(g-f), e -> m-e)));
  W = transpose matrix W;
  F := fourierMotzkin W;
  W' := - F_0;
  w := sum entries transpose W';
  (w, W',F_1,W))

weightVector = method(TypicalValue=>List)
weightVector(List,List) := (inL,L) -> (
	(w,W',H,W) := wtvec(inL,L);
	--(W',H) is the "cone": W' are the positive edges, and H is contained inside.
	-- we need to find a positive element in here
	if min w > 0 then w else
	if numgens source H > 0 then (
		h := sum entries transpose H;
		if min h <= 0 then error "need to try harder to find a weight vector";
		while min w <= 0 do w = w + h;
		w
	)
	-- other cases should return null
)

runchk = cmd -> if 0 != run cmd then error("command failed: ", cmd)

render = method(Options=>{Symmetries=>{}})
render(Ideal) := opts->(I) -> (
    F := temporaryFileName();
		render(F,I);		
		ex := fig2dev'path |"fig2dev -Lpng "| F | " " | F |".png";
		runchk ex;
		show URL("file://"|F|".png");
)

render(String, Ideal) := opts-> (F, I) -> (
	R := ring I;
	p := char R;
	if p === 0 and coefficientRing(R) =!= QQ then 
	error "expected prime field or QQ";
	-- Create the input file
	f := temporaryFileName();
	-- << "using temporary file " << f << endl;
	ex := "";
	if opts.Symmetries =!= {}
	then (
	if not instance(opts.Symmetries, VisibleList)
	then error "Symmetries value should be a list of permutations (list of lists of integers)";
	ex = ex|" --symmetry";
	);
	ex = findProgram "gfan" | " " | ex | "  <" | f | " | "|findProgram "gfan_render"|" > " | F;
	writeGfanIdeal(f, I, opts.Symmetries);
	runchk ex;
	removeFile f;
)


renderStaircase = method()
renderStaircase(Ideal) := (I) -> (
	d := 2 +max( flatten entries gens I / exponents /flatten /max );
	renderStaircase(d, I);
)
renderStaircase(String, Ideal) := (F, I) -> (
	d := 2 +max( flatten entries gens I / exponents /flatten /max );
	renderStaircase(F, d, I);
)

renderStaircase(ZZ,Ideal) := (d,I) -> renderStaircase(d,1,{I})
renderStaircase(String,ZZ,Ideal) := (F,d,I) -> renderStaircase(F,d,1,{I})
renderStaircase(ZZ,ZZ,List) := (d,n,L) -> (
	F := temporaryFileName();
	renderStaircase(F,d,n,L);		
	ex := fig2dev'path |"fig2dev -Lpng "| F | " " | F |".png";
	runchk ex;
	show URL("file://"|F|".png");
)

renderStaircase(String,ZZ,ZZ,List) := (F,d,w,L) -> (
	if class L#0 === List then L = apply(L, ideal);
	R := ring L#0;
	p := char R;
	if p === 0 and coefficientRing(R) =!= QQ then 
		error "expected prime field or QQ";
	if any(L, J-> ring J =!= R) then
		error "all ideals should have the same ring";

	ex := "";
	f := temporaryFileName();
	-- << "using temporary file " << f << endl;

	ex = findProgram "gfan_renderstaircase"| " -m -d "| d | " -w " | w ;
	ex = ex | " < " | f |" >" | F;

	writeGfanIdealList(f, L);
	runchk ex;
	removeFile f;
)

groebnerCone = method()
groebnerCone(List,List) := (inL,L) -> (
	(w,W',H,W) := wtvec(inL,L);
	(W',H)
)

initialIdeal = method(TypicalValue=>Ideal)
initialIdeal Ideal  := (I) -> ideal leadTerm I
initialIdeal(List,Ideal) := (w,I) -> (
	R := ring I;
	R1 := (coefficientRing R)[gens R, 
	Degrees=>(monoid R).Options.Degrees, 
	MonomialOrder=>Weights=>w];
	I' := substitute(I,R1);
	ideal substitute(leadTerm(1, gens gb I'), R)
)

writeGfanRing = (R) -> (
	p := char R;
	outStr := if p === 0 then "Q" else "Z/"|p|"Z";
	outStr = outStr | "[";
	for i from 0 to numgens R - 1 do (
		outStr = outStr | writeVariable(i, numgens R);
		if i =!= numgens R -1 then outStr = outStr | ", ";
	);
	outStr | "]"
)

writeGfanIdeal = method()
writeGfanIdeal(String,Ideal,List) := (filename,I,symms) -> (
	F := openOut filename;
	R := ring I;
	F << writeGfanRing R << endl;

	-- Now make the list of the gens of I
	F << "{";
	n := numgens I - 1;
	for i from 0 to n do (
	F << writePolynomial(I_i);
	if i < n then F << "," else F << "}";
		F << endl;
	);
	symms = apply(symms, x -> apply(x,index));
	-- If any symmetries, write them here
	if #symms > 0 then (
		F << toString symms << endl;
	);
	F << close;
)

writeGfanIdealList = method()
writeGfanIdealList(String,List) := (filename,L) -> (
	F := openOut filename;
	R := ring L#0;
	F << writeGfanRing R << endl;

	F << "{" << endl;
	for j from 0 to #L-1 do (
		-- Now make the list of the gens of I
		I := initialIdeal L#j;
		F << "{" << endl;
		n := numgens I - 1;
		for i from 0 to n do (
			F << writePolynomial(I_i);
			if i < n then F << "," else (
				F << endl << "}";
				if j =!= #L-1 then F << ",";
			);
			F << endl;
		 )
	);
	F << "}" << endl;
	F << close;
)

readGfanIdeals = method()
readGfanIdeals (String, PolynomialRing) := (f, R) -> (
	-- Reads using the current ring
	s := get f;
	G := separate("\n,",s);

	firstLine := G#0;
	firstLine = separate("\n", firstLine);
	firstLine = drop(firstLine, 1);  -- remove the ring from the first line
	tempStr  := "";
	apply(firstLine, t -> tempStr = concatenate(tempStr, "\n", t)); -- build the firstline

	G = drop(G,1);  -- drop the old first entry
	G = prepend(tempStr, G); -- and then add the first entry minus the ring
	H := apply(G, t -> replace(///[\{\}]*///,"",t));
	g := getSymbol "g";
	S := (coefficientRing R)[g_0..g_(numgens R - 1)];
	rsMap := map(R,S, gens R);
	apply(H, s -> apply(value("{"|s|"}"), p -> rsMap(p)))
)

-- The following routines are lifted from Polymake.m2, but should be taken out, once that package
-- is ready for prime time.

------------------------------------------------------------------------------

readGroebnerfan= method()
readGroebnerfan String := (f) -> (
		 f = lines get f;
		 return new PolymakeObject from {
				"AMBIENT_DIM" => value first removeComments getProperty(f,"AMBIENT_DIM"),
				"DIM" => value first removeComments getProperty(f,"DIM"),
				"LINEALITY_DIM" => value first removeComments getProperty(f,"LINEALITY_DIM"),
				"RAYS" => getMatrixProperty(f,"RAYS"),
				"N_RAYS" => value first removeComments getProperty(f,"N_RAYS"),
				"LINEALITY_SPACE" => getMatrixProperty(f,"LINEALITY_SPACE"),
				"ORTH_LINEALITY_SPACE" => getMatrixProperty(f,"ORTH_LINEALITY_SPACE"),
				"F_VECTOR" => getVectorProperty(f,"F_VECTOR"),
				"CONES" => getListProperty(f,"CONES"),
				"MAXIMAL_CONES" => getListProperty(f,"CONES"),
				"PURE" => value first removeComments getProperty(f,"PURE")}
		)
			


gfan = method(Options=>{Symmetries=>{}})
gfan Ideal := opts -> (I) -> (
     R := ring I;
     p := char R;
     if p === 0 and coefficientRing(R) =!= QQ then 
     error "expected prime field or QQ";
     -- Create the input file
     f := temporaryFileName();
     -- << "using temporary file " << f << endl;
     ex := "";
     if opts.Symmetries =!= {}
     then (
	  if not instance(opts.Symmetries, VisibleList)
	  then error "Symmetries value should be a list of permutations (list of lists of integers)";
	  ex = ex|" --symmetry";
	  );
     ex = findProgram "gfan"| " " | ex | "  <" | f | " >" | f | ".out";
     writeGfanIdeal(f, I, opts.Symmetries);
     runchk ex;
     ex2 := findProgram "gfan_leadingterms"| " -m <" | f | ".out >" | f | ".lt";
     runchk ex2;
     L := readGfanIdeals(f | ".out", R);
     M := readGfanIdeals(f | ".lt", R);
     removeFile f;
     removeFile(f | ".out");
     removeFile(f | ".lt");
     (M,L)
     )

universalGroebnerBasis = method(Options=>{Symmetries=>{}})
universalGroebnerBasis Ideal := opts -> (I) -> (
	 toList sum(apply(last gfan(I,opts) , l -> set apply(l,p-> p/leadCoefficient(p))))
	 --- gfan returns lists of monomials, and lists of reduced groebner bases.
	 --- To compute the universal groebner basis we take the union of the reduced gbs.
	)

groebnerFan = method(Options=>{Symmetries=>{}})
groebnerFan Ideal := opts -> (I) -> (
     R := ring I;
     p := char R;
     if p === 0 and coefficientRing(R) =!= QQ then 
     error "expected prime field or QQ";
     -- Create the input file
     f := temporaryFileName();
     -- << "using temporary file " << f << endl;
     ex := "";
     if opts.Symmetries =!= {}
     then (
	  if not instance(opts.Symmetries, VisibleList)
	  then error "Symmetries value should be a list of permutations (list of lists of integers)";
	  ex = ex|" --symmetry";
	  );
     ex = findProgram "gfan"| " " | ex | "  <" | f | "| " | findProgram "gfan_topolyhedralfan" | " " | ex | " >" | f | ".out";
     writeGfanIdeal(f, I, opts.Symmetries);
     runchk ex;
     ret := readGroebnerfan(f | ".out");
     removeFile f;
     removeFile(f | ".out");
     ret
     )

beginDocumentation()

document { 
	Key => "gfanInterface",,
	Headline => "a Macaulay2 interface to gfan",
	EM "gfanInterface", " is an interface to Anders Jenssen's gfan package, which is a C++
	program to compute the Groebner fan (i.e. all the initial ideals) of an ideal.",
	PARA{
	"The main function in this package is ", TO gfan, ", which computes all of the Groebner bases and initial ideals of a given ideal.  A useful feature of this function is that it can handle symmetries in the ideal. If you want the geometric information of this list of Groebner basis, see ", TO groebnerFan, "."},
	PARA{"There are other functions in the gfan package.  If you wish to use
	one whose interface is not included here, you have two options: either write the interface yourself, and then send it
	to the package author, so it can be included for others, or ask the package author to write it."},
	}

doc ///
	Key 
		gfan
		(gfan, Ideal)
		[gfan, Symmetries]
	Headline
		all initial ideals of an ideal
	Usage 
		(M,L) = gfan I
	Inputs
		I:Ideal
		Symmetries:List
			of permutations of the variables leaving the ideal invariant
	Outputs
		M:List
			of lists of monomials
		L:List
			of all of the initial ideals of I
	Description
		Text
			This method compute the Groebner fan of an ideal, which is a list of all reduced 
			Groebner bases of an ideal. A particular Grobner basis may occur twice if it
			is a Groebner basis under multiple term orderings. Rather than marking the each Groebner
			basis to distinguish the leading terms, a second parallel list is given which contains
			the lead terms of each Groebner basis.
		Example
			R = ZZ/32003[symbol a..symbol d];
			I = monomialCurveIdeal(R,{1,3,4})
			time (M,L) = gfan I;
	 		M/toString/print;
			L/toString/print;
		Text
			We can see that the leading terms of
			{\tt -c^3+b*d^2, -b*c+a*d, a*c^2-b^2*d, -b^3+a^2*c} (which is the first Groebner basis
			listed in {\tt L}) are {\tt b*d^2, a*d, a*c^2, a^2*c}.

			If the ideal is invariant under some permutation of the variables, then gfan can compute
			all initial ideals up to this equivalence, which can change an intractible problem to a doable one.
			The cyclic group of order 4 a --> b --> c --> d --> a leaves the following ideal fixed.
		Example
			S = ZZ/32003[a..e];
			I = ideal"a+b+c+d,ab+bc+cd+da,abc+bcd+cda+dab,abcd-e4"
			(inL,L) = gfan I;
			#inL
		Text
			There are 96 initial ideals of this ideal. We can use the symmetry on the above:
		Example
			(inL1, L1) = gfan(I, Symmetries=>{(b,c,d,a,e)});
			#inL1
		Text
			Internally gfanInterface uses a polynomial ring in variables g_i. If you are using the variable g in your ring, you may need to reasign the old ring to a variable in order to recover the use of g as a variable.
		Example
			QQ[f,g,h]
			I = ideal"fg + gh";
			gfan I;
			R = ring I;
	SeeAlso
		weightVector
		initialIdeal
		groebnerCone
		Symmetries
///

document {
	Key => {weightVector, (weightVector, List,List)},
	Headline => "weight vector of a marked set of polynomials",
	Usage => "weightVector(inL,L)",
	Inputs => { "inL" => List => {"of monomials which are to be the lead terms of the elements of ", TT "L"},
	     "L" => List => "of polynomials"
	     },
	Outputs => {
	     {"of positive integers giving a weight vector under which ", TT "inL", " are the lead terms of ", TT "L"}
	     },
	"If there is no weight vector, then ", TT "null", " is returned.",
	EXAMPLE lines ///
	  R = ZZ/32003[symbol a..symbol d]
	  inL = {c^4, b*d^2, b*c, b^2*d, b^3}
	  L = {c^4-a*d^3, -c^3+b*d^2, b*c-a*d, -a*c^2+b^2*d, b^3-a^2*c}
	  weightVector(inL,L)
	  groebnerCone(inL,L)
	  ///,
        PARA{"Now we construct all of the initial ideals of the rational quartic curve in P^3,
	     then compute weight vectors for each, and then verify that the initial ideals that
	     gfan returned are the initial ideals using these weight vectors."},
	EXAMPLE lines ///
	  I = monomialCurveIdeal(R,{1,3,4})
	  time (inLs,Ls) = gfan I
     	  wtvecs = apply(#inLs, i -> weightVector(inLs#i, Ls#i));
	  wtvecs/print;
     	  inL1 = wtvecs/(w -> initialIdeal(w,I));
	  inL1/toString/print;
	  assert(inL1 == inLs/ideal)
     	///,
	Caveat => {"In the current implementation, it might be possible that a positive vector exists, but the algorithm fails to find it.  In this 
	     case, use groebnerCone and find one by hand.  You might want to email the package author to complain too!"},
	SeeAlso => {"gfan", "initialIdeal", "groebnerCone"}
	}


doc ///
	Key 
		renderStaircase
		(renderStaircase, Ideal)
		(renderStaircase, ZZ, Ideal)
		(renderStaircase, String, Ideal)
		(renderStaircase, String, ZZ, Ideal)
		(renderStaircase, ZZ, ZZ, List)
		(renderStaircase, String, ZZ, ZZ, List)
	Headline
		draws staircases of an initial ideal.
	Usage 
		renderStaircase(I) \n renderStaircase(d,I) \n renderStaircase(F,I) \n renderStaircase(F,d,I) \n renderStaircase(d,n,L) \n renderStaircase(F,d,n,L)
	Inputs 
		F:String 
			filename of the xfig file that is produced 
		I:Ideal 
			of which to draw the staircase.
		L:List 
			of ideals of which to draw their staircases.
		d:ZZ 
			number of boxes to be drawn along each axis
		n:ZZ 
			number of diagrams per line
	Description
		Text
			Renders the staircase of a monomial ideal (or a list of monomial ideals) in three variables.
			The output is put into an xfig file if a filename is provided. If no filename is provided then
			macaulay uses fig2dev to make a png which is displayed by the operating system (see @TO show@).
		Example
			QQ[x,y,z]
			I = ideal "x4,y4,z4,xy2z3,x3yz2,x2y3z"
			J = ideal "x4,y4,z4,x2y2z2"
			null -- renderStaircase(I)
			null -- renderStaircase("twostairs.fig", 6,2,{I,J})
		Text
			To run the examples above, ommit the {\tt null -- }. 
			The first command will display the staircase of {\tt I} while the
			second writes the image of both {\tt I} and {\tt J} to file.
///

doc ///
	Key 
		render
		(render, Ideal)
		(render, String, Ideal)
		[render, Symmetries]
	Headline
		draws the Groebner fan an ideal.
	Usage 
		renderStaircase(I) \n renderStaircase(d,I) \n renderStaircase(F,I) \n renderStaircase(F,d,I) \n renderStaircase(d,n,L) \n renderStaircase(F,d,n,L)
	Inputs 
		F:String 
			filename of the xfig file that is produced 
		I:Ideal 
			of which to draw the Groebner fan.
		Symmetries:List
			a list of symmetries of the ideal.
	Description
		Text
			Renders the Groebner fan of an ideal.
			The output is put into an xfig file if a filename is provided. If no filename is provided then
			macaulay uses fig2dev to make a png which is displayed by the operating system (see @TO show@).
		Example
	 		R = QQ[symbol a..symbol f];
			I = pfaffians(4, genericSkewMatrix(R,4))
			null -- render(I)
			null -- render("pfaffgfan.fig", I)
		Text
			To run the examples above, ommit the {\tt null -- }. 
			The first command will display the Groebner fan of {\tt I} while the
			second writes the image to file.
///

doc ///
	Key 
		groebnerFan
		(groebnerFan, Ideal)
		[groebnerFan, Symmetries] 
	Headline
		the fan of all groebner bases of an ideal
	Usage 
		P = groebnerFan(I)
	Inputs 
		I:Ideal 
			of which to compute the fan
		Symmetries:List 
			of permutations of the variables leaving the ideal invariant
	Outputs
		P:HashTable
			containing all the data of the polyhedral fan of {\tt I}.
	Caveat
		Requires loading of the Polymake package to make the PolymakeObject type available.
	Description
		Example	
	 		R = QQ[symbol a..symbol f];
			I = pfaffians(4, genericSkewMatrix(R,4))
			P = groebnerFan(I)
			peek P
	SeeAlso 
		gfan
		initialIdeal
		groebnerCone
		Symmetries
///

doc ///
	Key 
		Symmetries	
	Headline
		permutations leaving an ideal invariant to speed up gfan computations.
	Usage 
		Symmetries => L
	Inputs 
		L:List
			of permutations of the variables leaving an ideal invariant.
	Description
		Text
			Many gfan functions can be sped up and give smaller output when symmetries of
			the ideal are given. Permuations are specified as sequences of variables.	
			Not all permuations need to be listed; only permutations that generate
			all of the symmetries.
			
			A possible caveat is that the permuatations must be appled to the output to get the entire result.

		Example	
	 		R = QQ[a,b,c];
			I = ideal(a+b,b+c);
			gfan(I)
			gfan(I, Symmetries => {(c,b,a)})
		
		Text
			Note that the use of symmetries above reduces the amount of output.
			The permutations must be appled to the output to get the complete result.

		Example
	 		R = QQ[a,b,c,d,e];
			I = ideal"a+b+c+d,ab+bc+cd+da,abc+bcd+cda+dab,abcd-e4"
			#universalGroebnerBasis(I)
			#universalGroebnerBasis(I, Symmetries => {(b,c,d,a,e)})
///

doc ///
	Key 
		universalGroebnerBasis
		(universalGroebnerBasis, Ideal)
		[universalGroebnerBasis, Symmetries] 
	Headline
		the union of all reduced Groebner bases of an ideal.
	Usage 
		B = universalGroebnerBasis(I)
	Inputs 
		I:Ideal 
			of which to compute the universal Groebner basis
		Symmetries:List 
			of permutations of the variables leaving the ideal invariant. See @TO gfan@.
	Outputs
		B:List
			containing the polynomials that form the universal Groebner basis of {\tt I}.
	Description
		Example 
	 		R = QQ[symbol x, symbol y, symbol z]
			I = ideal(x+y, y+z)
			universalGroebnerBasis(I)
	SeeAlso 
		gfan
///

doc ///
	Key
		groebnerCone
		(groebnerCone, List,List)
	Headline
		the cone whose interior weight vectors give the given initial ideal
	Usage 
		(C,H) = groebnerCone(inL,L)
	Inputs 
		inL:List 
			of monomials which are to be the lead terms of the elements of {\tt L}
		L:List 
			of polynomials
	Outputs 
		C:Matrix
			the columns are the extremal rays of the Groebner cone
	  H:Matrix
			the columns generate the largest linear space contained in the cone
	Description
		Example
			R = ZZ/32003[symbol a..symbol d]
			inL = {c^4, b*d^2, b*c, b^2*d, b^3}
			L = {c^4-a*d^3, -c^3+b*d^2, b*c-a*d, -a*c^2+b^2*d, b^3-a^2*c}
			weightVector(inL,L)
			groebnerCone(inL,L)
			I = monomialCurveIdeal(R,{1,3,4})
			time (inLs,Ls) = gfan I
			weightVector(inLs#0, Ls#0)
			scan(#inLs, i -> print weightVector(inLs#i, Ls#i));
			scan(#inLs, i -> print groebnerCone(inLs#i, Ls#i));
	SeeAlso 
		gfan
		initialIdeal	
		weightVector
///

doc ///
	Key
		initialIdeal
		(initialIdeal, List,Ideal)
		(initialIdeal, Ideal)
	Headline 
		initial ideal with respect to a weight vector
	Usage 
		M = initialIdeal(w,I) or M = initialIdeal(I)
	Inputs 
		w:List
			a positive weight vector
		I:Ideal
			in a polynomial ring (not a quotient ring)
	Outputs
		M:Ideal
			the ideal of lead polynomials under this weight vector
        
	Description
		Text
			The weight vector should be totally positive, even in the homogeneous case.  
			The result may or may not be a monomial ideal. When a weight vector is not specified, this
			simply uses the current term order.
		Example
			R = ZZ/32003[symbol a..symbol d]
			inL = {c^4, b*d^2, b*c, b^2*d, b^3}
			L = {c^4-a*d^3, -c^3+b*d^2, b*c-a*d, -a*c^2+b^2*d, b^3-a^2*c}
			weightVector(inL,L)
			groebnerCone(inL,L)
			initialIdeal({8,8,3,1},ideal L)
			initialIdeal({5,5,2,1},ideal L)
	SeeAlso 
		gfan
		weightVector
		groebnerCone
///

---Basic explicit test of gfan
TEST ///
QQ[x,y];
(M,L) = gfan(ideal(x+y));
assert(#M === 2);
assert(set M === set{{x},{y}});
assert(#L === 2);
assert(L === {{ x+y} ,{x+y}});
///

----Check that using symmetries gives the
----same output as without.
TEST ///
QQ[x,y,z]
(M,L) = gfan(ideal(x+y,y+z));
(N,T) = gfan(ideal(x+y,y+z), Symmetries=>{(z,y,x)});
assert(#M === 3);
assert(#N === 2);
Nset = set( apply(N, set) | apply(N, L -> set apply(L, p -> sub(p, {x=>z, z=>x}))))
Tset = set( apply(T, set) | apply(T, L -> set apply(L, p -> sub(p, {x=>z, z=>x}))))
Mset = set apply(M, set);
Lset = set apply(L, set);
assert( Nset === Mset);
assert( Tset === Lset);
///

end
restart
loadPackage "gfanInterface"
installPackage "gfanInterface"
R = ZZ/3[a..d]
I = ideal(a^3-a,b^3-b,a+2*b+c+d-1,a^2*b+b^2*a,c^3-c)
time (M,L) = gfan I;
time apply(#M, i -> (
  weightVector(L_i, M_i)))

wtvecs = apply(oo, x -> x#0)
Mset1 = apply(wtvecs, w -> set flatten entries initialIdeal(w,I))
Mset = M/set
Mset === Mset1

R = ZZ/32003[symbol a..symbol d]
I = monomialCurveIdeal(R,{1,3,4})
time (M,L) = gfan I
time apply(#M, i -> (
  wtvec(ideal(L_i), ideal(M_i))))

R = ZZ/32003[symbol a..symbol f]
m = genericSymmetricMatrix(R,a,3)
I = minors(2,m)
time (M,L) = gfan I;
L1 = L/sort;
L2 = L1/(I -> apply(I, leadMonomial))
unique L2
L3 = L2/(I -> flatten entries gens gb ideal I)
unique L3
#oo

-- an example with symmetries
R = ZZ/32003[symbol a..symbol d,symbol e]
I = ideal"a+b+c+d,ab+bc+cd+da,abc+bcd+cda+dab,abcd-e4"
(M1,L1) = gfan I;
(M2,L2) = gfan(I, Symmetries=>{(b,c,d,a,e)});

-- veronese in general coordinates
R = ZZ/32003[symbol a..symbol f]
m = genericSymmetricMatrix(R,a,3)
I = minors(2,m)
F = map(R,R,random(R^1, R^{6:-1}))
I = F I
time (M,L) = gfan I;
M/toString/print

R = ZZ/32003[x,y,z]
I = ideal(x^3-y*z, x*y*z-y^4)
gfan I


gfan  # implement the symmetry
gfan_groebnercone # can replace Fourier-Motzkin
gfan_topolyhedralfan
gfan_render  # for 3 vars
gfan_renderstaircase # for 3 vars

gfan_tropicalbasis		
gfan_tropicalintersection	
gfan_tropicalstartingcone	
  #gfan_tropicaltraverse		



F = get "/tmp/M2-2675-1.out"
G = separate("\n,",F);
H = for i from 0 to #G-1 list if i === 0 then substring(1,G#0) else if i === #G-1 then substring(0,(#G#-1)-1,G#-1) else G#i
apply(H, value)

class output
split
separate("\n,",output)
replace
viewHelp replace
oo/print;


/Users/mike/local/software/sage/sage-1.4.1.2/local/bin/gfan_
