-- THIS FILE IS UNDER CONSTRUCTION!!  It will replace gfanInterface in 1.3?

--needsPackage "Polymake"

newPackage(
	"gfanInterface2",
	Version => "0.3", 
	Date => "July 15, 2009",
	Authors => {
		{Name => "Mike Stillman", Email => "mike@math.cornell.edu", HomePage => ""},
		{Name => "Andrew Hoefel", Email => "andrew.hoefel@mathstat.dal.ca", HomePage => ""}
	},
	Headline => "Interface to A. Jensen's gfan package",
	Configuration => { 
		"path" => "",
		"fig2devpath" => "",
		--"keep files" => true,
		"verbose" => false
	},
	DebuggingMode => false
)

export {
        MarkedPolynomialList,
	gfan, 
	gfanBuchberger,
	gfanDoesIdealContain,
	gfanGroebnerCone,
	gfanHomogeneitySpace,
	gfanHomogenize,
	gfanInitialForms,
	gfanInteractive,
	gfanIsMarkedGroebnerBasis,
	gfanKrullDimension,
	gfanLeadingTerms,
	gfanMarkPolynomialSet,
	gfanPolynomialSetUnion,
	gfanRender, 
	gfanRenderStaircase, 
	gfanSaturation,
	gfanStats,
	gfanSubstitute,
	gfanToLatex,
	gfanToPolyhedralFan,
	gfanTropicalBasis,
	gfanTropicalBruteForce,
	gfanTropicalIntersection,
	gfanTropicalLifting,
	gfanTropicalMultiplicity,
	gfanTropicalStartingCone,
	gfanTropicalTraverse
}

gfanPath = gfanInterface2#Options#Configuration#"path"
fig2devPath = gfanInterface2#Options#Configuration#"fig2devpath"
gfanVerbose = gfanInterface2#Options#Configuration#"verbose"
--gfanKeepFiles = gfanInterface2#Options#Configuration#"keep files"

--needsPackage "Polymake"

MarkedPolynomialList = new Type of List
  -- Currently: this is a list {inL,L}, where 
  --   inL is a list of monomials (with coefficient, often 1)
  --   L is a list of polynomials
  -- and L and inL have the same length, and the
  -- the monomial inL#i is the marked monomial, which
  -- should occur with the same coefficient in L#i.

--------------------------------------------------------
--------------------------------------------------------
-- PARSING FUNCTIONS
--------------------------------------------------------
--------------------------------------------------------

gfanParseList = method();
gfanParseList String := (S) -> (
	S = replace("\n", "", S);
        stack := {};
        r := regex(///[\{,\}]///, S);
        popstate := false;
        while #r == 1 do (
                startpos := first first r;
                character := substring(startpos,1,S);
                prestring := substring(0, startpos, S);
                S = substring(startpos+1, #S,S);
                if character == "," then (
                        if not popstate then (
                           stack = append(drop(stack,-1), append(last stack, prestring));
                        );
                        popstate = false;
                ) else if character == "{" then (
                        stack = append(stack, {});
                ) else if #stack == 0 then (
                        error "Parsing error";
                ) else if #stack == 1 then (
                        if not popstate then (
                                stack = append(drop(stack,-1), append(last stack, prestring));
                        );
                        return first stack;
                ) else (
                        if not popstate then (
                        stack = append(drop(stack,-1), append(last stack, prestring));
                        );
                        stack = append(drop(stack,-2), append(stack#(#stack -2), last stack));
                        popstate = true;
                );
                r = regex(///[\{,\}]///, S);
        );
        error "Parsing error";
)

gfanParseMarkedPoly = method();
gfanParseMarkedPoly String := (S) -> (
	indices := first regex(///\`[:space:]*.[^+-]*///,S);
	return { value substring(first indices, last indices,S), value S};
	--match from the start of the string all whitespace, one
	--non-white space character and then everything until the +/-
)

gfanParseIdeals = method()
gfanParseIdeals String := (s) -> (
	--needs to be simplified
	G := separate("\n,",s);
	firstLine := G#0;
	firstLine = separate("\n", firstLine);
	firstLine = drop(firstLine, 1);  -- remove the ring from the first line
	tempStr  := "";
	scan(firstLine, t -> tempStr = concatenate(tempStr, "\n", t)); -- build the firstline

	G = drop(G,1);  -- drop the old first entry
	G = prepend(tempStr, G); -- and then add the first entry minus the ring
	H := apply(G, t -> replace(///[\{\}]*///,"",t));
	apply(H, s -> value("{"|s|"}"))
)

gfanParseIdeal = method()
gfanParseIdeal String := (s) -> (
	G := separate("]",s);
	G = drop(G,1);
	value concatenate G 
)

gfanParseMarkedIdeal = method()
gfanParseMarkedIdeal String := (s) -> (
	G := separate("]",s);
	G = drop(G,1);
	transpose apply(gfanParseList(concatenate G), p -> gfanParseMarkedPoly(p))
)

gfanParseMarkedIdeals = method()
gfanParseMarkedIdeals String := (s) -> (
	G := separate("]",s);
	G = drop(G,1);
	apply(gfanParseList(concatenate G), L -> transpose apply(L, p -> gfanParseMarkedPoly(p)))
)

gfanParseIdealPair = method()
gfanParseIdealPair String := (s) -> (
	G := separate("\n",s);
	G = drop(G,1);
	drop(value ("{"| replace(///\{///, ",{", concatenate G ) | "}"), 1)
)

gfanParseMarkedIdealPair = method()
gfanParseMarkedIdealPair String := (s) -> (
	G := separate("\n",s);
	G = concatenate drop(G,1);
	apply(drop(regex(///(\{[^\}]*\})(\{[^\}]*\})///, G),1), i-> transpose apply(gfanParseList(substring(first i, last i, G)), p-> gfanParseMarkedPoly(p)))
)

gfanParseInteger = method()
gfanParseInteger String := (s) -> value s

gfanParseBool = method()
gfanParseBool String := (s) -> s == "true\n"

gfanParseBoolInteger = method()
gfanParseBoolInteger String := (s) -> s == "1\n"


gfanSymbolToString = method()
gfanSymbolToString Symbol := (X) -> (
	toExternalString(X) | "\n"
)

gfanIdealToString = method()
gfanIdealToString Ideal := (I) -> (
	out := "{";
	n := numgens I - 1;
	for i from 0 to n do (
		out = out | toExternalString(I_i);
		if i < n then out = out | "," else out = out | "}";
	out = out | newline;
	);
	return out;
)


gfanPolynomialListToString = method()
gfanPolynomialListToString List := (L) -> (
	out := "{";
	n := #L - 1;
	for i from 0 to n do (
		out = out | toExternalString(L#i);
		if i < n then out = out | "," else out = out | "}";
		out = out | newline;
	);
	return out;
)

gfanMPLToString = method()
gfanMPLToString List := (L) -> (
	L = transpose L;
	out := "{";
	n := #L - 1;
	for i from 0 to n do (
		out = out | toExternalString(first L#i);
		if (last L#i) - (first L#i) != 0 then 
			out = out | " + " | toExternalString((last L#i) - (first L#i));
		if i < n then out = out | "," else out = out | "}";
		out = out | newline;
	);
	return out;
)

gfanPairOfPolynomialListsToString = method()
gfanPairOfPolynomialListsToString (List, List) := (L,K) -> (
	return "{" | gfanPolynomialListToString(L)
		| gfanPolynomialListToString(K)
		| "}\n"; -- note there is no comma. Is this a bug in gfan?
)

gfanListOfPolynomialListsToString = method()
gfanListOfPolynomialListsToString := (L) -> (
	out := "{";
	n := #L - 1;
	for i from 0 to n do (
		out = out | gfanPolynomialListToString(L#i);
		if i < n then out = out | "," else out = out | "}";
		out = out | newline;
	);
	return out;
)

--list of marked polynomial lists to string
gfanLMPLToString = method()
gfanLMPLToString := (L) -> (
	out := "{";
	n := #L - 1;
	for i from 0 to n do (
		out = out | gfanMPLToString(L#i);
		if i < n then out = out | "," else out = out | "}";
		out = out | newline;
	);
	return out;
)

gfanSymmetriesToString = method()
gfanSymmetriesToString := (L) -> (
	out := "{";
	n := #L - 1;
	for i from 0 to n do (
		out = out | gfanSymmetryToString(L#i);
		if i < n then out = out | "," else out = out | "}";
		out = out | newline;
	);
	return out;
)

gfanSymmetryToString = method()
gfanSymmetryToString List := (L) -> (
	out := "(";
	n := #L - 1;
	for i from 0 to n do (
		out = out | toExternalString(L#i);
		if i < n then out = out | "," else out = out | ")";
		out = out | newline;
	);
	return out;
)

gfanIntegerListToString = method()
gfanIntegerListToString List := (L) -> toString L

gfanRingToString = method()
gfanRingToString PolynomialRing := (R) -> (
	p := char R;
	out := if p === 0 then "Q" else "Z/"|p|"Z";
	out = out | toExternalString(new Array from gens R) | newline;
	return out;
)

gfanMPLToRingToString = method()
gfanMPLToRingToString List := (L) -> (
	R := ring first last L; 
	p := char R;
	out := if p === 0 then "Q" else "Z/"|p|"Z";
	out = out | toExternalString(new Array from gens R) | newline;
	return out;
)


--------------------------------------------------------
-- runGfanCommand
--------------------------------------------------------

runGfanCommand = (cmd, argStrs, args, data) -> (
	tmpName := temporaryFileName();
	if gfanVerbose then << "using temporary file " << tmpName << endl;
	args = concatenate toList apply(#argStrs, i-> if args#i then " " | argStrs#i else "");
	ex = gfanPath | cmd | args | " < " | tmpName | " > " | tmpName | ".out";
	tmpFile := openOut tmpName;
	tmpFile << data << close;
	if gfanVerbose then << ex << endl;
	run ex;
	get(tmpName | ".out")
)

runGfanCommandCaptureError = (cmd, argStrs, args, data) -> (
	tmpName := temporaryFileName();
	if gfanVerbose then << "using temporary file " << tmpName << endl;
	args = concatenate toList apply(#argStrs, i-> if args#i then " " | argStrs#i else "");
	ex = gfanPath | cmd | args | " < " | tmpName | " 2> " | tmpName | ".out";
	tmpFile := openOut tmpName;
	tmpFile << data << close;
	if gfanVerbose then << ex << endl;
	run ex;
	get(tmpName | ".out")
)


--------------------------------------------------------
--------------------------------------------------------
-- GFAN HOOKS START HERE 
--------------------------------------------------------
--------------------------------------------------------


--------------------------------------------------------
-- gfan
--------------------------------------------------------

-- NOT COMPLETE!!
argStrs = hashTable{
     "g" => "-g",
     "symmetry" => "--symmetry"
     }

gfan = method(Options=>{
	  "g"=>false, 
	  "symmetry"=>false, 
	  "e"=>false, 
	  "subspace"=>false, 
	  "disableSymmetryTest"=>false, 
	  "help"=>false})
gfan Ideal := opts -> (I) -> (
    argStrs := {"-g", "--symmetry", "-e", "--subspace", "--disableSymmetryTest", "--help"};
    args := { opts#"g", opts#"symmetry", opts#"e", opts#"subspace", opts#"disableSymmetryTest", opts#"help"};
    out := runGfanCommand("gfan",
                          argStrs, args, 
                          gfanRingToString(ring I) | gfanIdealToString(I));
    if opts#"help" then return out;
    gfanParseMarkedIdeals(out)
    --gfanParseIdeals(out)
)

gfan (Ideal, List) := opts -> (I,S) -> (
    argStrs := {"-g", "--symmetry", "-e", "--subspace", "--disableSymmetryTest", "--help"};
    args := { opts#"g", opts#"symmetry", opts#"e", opts#"subspace", opts#"disableSymmetryTest", opts#"help"};
    out := runGfanCommand("gfan",
                          argStrs, args, 
                          gfanRingToString(ring I) | gfanIdealToString(I) | gfanSymmetriesToString(S));
    if opts#"help" then return out;
    gfanParseMarkedIdeals(out)
    --gfanParseIdeals(out)
)

--L: Marked Poly  (available for the -g option)
gfan List := opts -> (L) -> (
    argStrs := {"-g", "--symmetry", "-e", "--subspace", "--disableSymmetryTest", "--help"};
    args := { opts#"g", opts#"symmetry", opts#"e", opts#"subspace", opts#"disableSymmetryTest", opts#"help"};
    out := runGfanCommand("gfan",
                          argStrs, args, 
                          gfanMPLToRingToString(L) | gfanMPLToString(L));
    if opts#"help" then return out;
    gfanParseMarkedIdeals(out)
)

--L: Marked Poly  (available for the -g option)
gfan (List, List) := opts -> (L,S) -> (
    argStrs := {"-g", "--symmetry", "-e", "--subspace", "--disableSymmetryTest", "--help"};
    args := { opts#"g", opts#"symmetry", opts#"e", opts#"subspace", opts#"disableSymmetryTest", opts#"help"};
    out := runGfanCommand("gfan",
                          argStrs, args, 
                          gfanMPLToRingToString(L) | gfanMPLToString(L) | gfanSymmetriesToString(S));
    if opts#"help" then return out;
    gfanParseMarkedIdeals(out)
)

--gfan Ideal := opts -> (I) -> gfan(markedPolynomialList I,opts)
gfan MarkedPolynomialList := opts -> (L) -> (
     -- NOT COMPLETE!!
     inputString := gfanMPLToRingToString(L) | gfanMPLToString(L) | gfanSymmetriesToString(opts.Symmetries);
     )
--------------------------------------------------------
-- gfan_buchberger
--------------------------------------------------------

gfanBuchberger = method(Options=>{"w"=>false, "r"=>false, "W"=>false, "g"=>false, "help"=>false})
gfanBuchberger Ideal := opts -> (I) -> (
    argStrs := {"-w", "-r", "-W", "-g", "--help"};
    args := { opts#"w", opts#"r", opts#"W", opts#"g", opts#"help"};
    out := runGfanCommand("gfan_buchberger",
                          argStrs, args, 
                          gfanRingToString(ring I) | gfanIdealToString(I));
    if opts#"help" then return out;
    gfanParseMarkedIdeal(out)
)

gfanBuchberger (Ideal,List) := opts -> (I,L) -> (
    argStrs := {"-w", "-r", "-W", "-g", "--help"};
    args := { opts#"w", opts#"r", opts#"W", opts#"g", opts#"help"};
    out := runGfanCommand("gfan_buchberger",
                          argStrs, args,
                          gfanRingToString(ring I) | gfanIdealToString(I) | gfanIntegerListToString(L));
    if opts#"help" then return out;
    gfanParseMarkedIdeal(out)
)

--------------------------------------------------------
-- gfan_doesidealcontain
--------------------------------------------------------

gfanDoesIdealContain = method(Options=>{"help"=>false})
gfanDoesIdealContain (List,List) := opts -> (I,J) -> (
    argStrs := {"--help"};
    args := {opts#"help"};
    out := runGfanCommand("gfan_doesidealcontain",
                          argStrs, args,
                          gfanMPLToRingToString(I) | gfanMPLToString(I) | gfanPolynomialListToString(J));
    if opts#"help" then return out;
    gfanParseBoolInteger out 
)

--------------------------------------------------------
-- gfan_groebnercone
--------------------------------------------------------

gfanGroebnerCone = method(Options=>{"restrict"=>false, "pair"=>false, "asfan"=>false, "help"=>false})
gfanGroebnerCone List := opts -> (L) -> (
    argStrs := {"--restrict", "--pair", "--asfan", "--help"};
    args := {opts#"restrict", opts#"pair", opts#"asfan", opts#"help"};
    out := if opts#"pair" then
		runGfanCommand("gfan_groebnercone",
                          argStrs, args,
                          gfanMPLToRingToString(first L) | gfanMPLToString(first L) | gfanMPLToString(last L))
	   else runGfanCommand("gfan_groebnercone",
                          argStrs, args,
                          gfanMPLToRingToString(L) | gfanMPLToString(L));
    if opts#"help" then return out;
    out  --- PARSE AS POLYHEDRAL DATA
)

--------------------------------------------------------
-- gfan_homogeneityspace
--------------------------------------------------------

gfanHomogeneitySpace = method(Options=>{"help"=>false})
gfanHomogeneitySpace (List) := opts -> (L) -> (
    argStrs := {"--help"};
    args := {opts#"help"};
    out := runGfanCommand("gfan_homogeneityspace",
                          argStrs, args,
                          gfanRingToString(ring first L) | gfanPolynomialListToString(L));
    if opts#"help" then return out;
    out --- should be parsed
)

--------------------------------------------------------
-- gfan_homogenize
--------------------------------------------------------

---Not marked -- does it need to be?
gfanHomogenize = method(Options=>{"i"=>false, "w"=>false, "help"=>false})
gfanHomogenize (List, Symbol) := opts -> (L,X) -> (
    argStrs := {"-i", "-w", "--help"};
    args := {opts#"i", opts#"w", opts#"help"};
    out := runGfanCommand("gfan_homogenize",
                          argStrs, args,
                          gfanRingToString(ring first L) | gfanPolynomialListToString(L) | gfanSymbolToString(X));
    if opts#"help" then return out;
    R := ring first L;
    S := R[X];
    gfanParseIdeal(out)
)

gfanHomogenize (List, List, Symbol) := opts -> (L,W,X) -> (
    argStrs := {"-i", "-w", "--help"};
    args := {opts#"i", opts#"w", opts#"help"};
    out := runGfanCommand("gfan_homogenize",
                          argStrs, args,
                          gfanRingToString(ring first L) | gfanPolynomialListToString(L) | gfanSymbolToString(X) | gfanIntegerListToString(W) );
    if opts#"help" then return out;
    R := ring first L;
    S := R[X];
    gfanParseIdeal(out)
)


--------------------------------------------------------
-- gfan_initialforms
--------------------------------------------------------

-- not marked -- should the output be?
gfanInitialForms = method(Options=>{"ideal"=>false, "pair"=>false,"help"=>false})
gfanInitialForms (List, List) := opts -> (L,W) -> (
    argStrs := {"--ideal", "--pair", "--help"};
    args := {opts#"ideal", opts#"pair", opts#"help"};
    out := runGfanCommand("gfan_initialforms",
                          argStrs, args,
                          gfanRingToString(ring first L) | gfanPolynomialListToString(L) | gfanIntegerListToString(W));
    if opts#"help" then return out;
    if opts#"pair" then return gfanParseIdealPair(out);
    gfanParseIdeal(out)
)

--------------------------------------------------------
-- gfan_interactive
--------------------------------------------------------

gfanInteractive = method(Options=>{"help"=>false})
gfanInteractive := opts -> () -> (
	argStrs := {"--help"};
	args := {opts#"help"};
	if opts@"help" then
		runGfanCommand("gfan_interactive", argStrs, args, "")
	else
		error "Not implemented";
)

--------------------------------------------------------
-- gfan_ismarkedgroebnerbasis
--------------------------------------------------------

gfanIsMarkedGroebnerBasis = method(Options=>{"help"=>false})
gfanIsMarkedGroebnerBasis (List) := opts -> (L) -> (
    argStrs := {"--help"};
    args := {opts#"help"};
    out := runGfanCommand("gfan_ismarkedgroebnerbasis",
                          argStrs, args,
                          gfanMPLToRingToString(L) | gfanMPLToString(L));
    if opts#"help" then return out;
    gfanParseBool out 
)


--------------------------------------------------------
-- gfan_krulldimension
--------------------------------------------------------

gfanKrullDimension = method(Options=>{"help"=>false})
gfanKrullDimension (List) := opts -> (L) -> (
    argStrs := {"--help"};
    args := {opts#"help"};
    out := runGfanCommand("gfan_krulldimension",
                          argStrs, args,
                          gfanMPLToRingToString(L) | gfanMPLToString(L));
    if opts#"help" then return out;
    gfanParseInteger out 
)


--------------------------------------------------------
-- gfan_leadingterms
--------------------------------------------------------

gfanLeadingTerms = method(Options=>{"m"=>false, "help"=>false})
gfanLeadingTerms (List) := opts -> (L) -> (
    argStrs := {"-m", "--help"};
    args := {opts#"m", opts#"help"};
    polys := if opts#"m" then 
	gfanMPLToRingToString(first L) | gfanLMPLToString(L) 
    else 
	gfanMPLToRingToString(L) | gfanMPLToString(L);
    out := runGfanCommand("gfan_leadingterms",
                          argStrs, args, polys );
    if opts#"help" then return out;
    if opts#"m" then return gfanParseIdeals out;
    gfanParseIdeal out 
)


--------------------------------------------------------
-- gfan_markpolynomialset
--------------------------------------------------------

gfanMarkPolynomialSet = method(Options=>{"help"=>false})
gfanMarkPolynomialSet (List, List) := opts -> (L,W) -> (
    argStrs := {"--help"};
    args := {opts#"help"};
    out := runGfanCommand("gfan_markpolynomialset",
                          argStrs, args,
                          gfanRingToString(ring first L) | gfanPolynomialListToString(L) | gfanIntegerListToString(W));
    if opts#"help" then return out;
    gfanParseMarkedIdeal out  
)


--------------------------------------------------------
-- gfan_polynomialsetunion
--------------------------------------------------------

--Should this be marked?
gfanPolynomialSetUnion = method(Options=>{"s"=>false,"help"=>false})
gfanPolynomialSetUnion (List,List) := opts -> (L,K) -> (
    argStrs := {"--help"};
    args := {opts#"help"};
    out := runGfanCommand("gfan_polynomialsetunion",
                          argStrs, args,
                          gfanRingToString(ring first L) | gfanPairOfPolynomialListsToString(L,K));
    if opts#"help" then return out;
    gfanParseIdeal out 
)


--------------------------------------------------------
-- gfan_render
--------------------------------------------------------

gfanRender = method(Options=>{"L"=>false, "shiftVariables"=>0, "help"=>false})
gfanRender (List) := opts -> (L) -> (
	argStrs := {"-L", "--shiftVariables " | opts#"shiftVariables", "--help"};
	args := {opts#"L", opts#"shiftVariables" != 0, opts#"help"};
	out := runGfanCommand("gfan_render",
		argStrs, args,
		gfanMPLToRingToString(first L) | gfanLMPLToString(L));
	if opts#"help" then return out;
	fileName = temporaryFileName();
	figure = openOut(fileName | ".fig");
	figure << out << close;
	<< "Figure rendered to " << fileName << ".fig" << endl;
	if fig2devPath != "" then	(
	run fig2devPath | "fig2dev -Lpng " | fileName  | ".fig " | fileName |".png";
	<< "Figure converted to png: " << fileName << ".png" << endl;
	show URL("file://" | fileName | ".png");
	) else << "fig2dev path not set." << endl ;
)


--------------------------------------------------------
-- gfan_renderstaircase
--------------------------------------------------------

gfanRenderStaircase = method(Options=>{"m"=>false, "d"=>8, "w"=>5, "help"=>false})
gfanRenderStaircase (List) := opts -> (L) -> (
	argStrs := {"-m", "-d " | opts#"d", "-w " | opts#"w", "--help"};
	args := {opts#"m", opts#"d" != 8, opts#"w" != 5, opts#"help"};
	out := if opts#"m" then
		runGfanCommand("gfan_renderstaircase",
			argStrs, args,
			gfanMPLToRingToString(first L) | gfanLMPLToString(L))
	else
		runGfanCommand("gfan_renderstaircase",
			argStrs, args,
			gfanMPLToRingToString(L) | gfanMPLToString(L));
	if opts#"help" then return out;
	fileName = temporaryFileName();
	figure = openOut(fileName | ".fig");
	figure << out << close;
	<< "Figure rendered to " << fileName << ".fig" << endl;
	if fig2devPath != "" then	(
		run fig2devPath | "fig2dev -Lpng " | fileName  | ".fig " | fileName |".png";
		<< "Figure converted to png: " << fileName << ".png" << endl;
		show URL("file://" | fileName | ".png");
	) else << "fig2dev path not set." << endl ;
)


--------------------------------------------------------
-- gfan_stats
--------------------------------------------------------

gfanStats = method(Options=>{"help"=>false})
gfanStats (List) := opts -> (L) -> (
    argStrs := {"--help"};
    args := {opts#"help"};
    out := runGfanCommand("gfan_stats",
                          argStrs, args,
                          gfanMPLToRingToString(first L) | gfanLMPLToString(L));
    if opts#"help" then return out;
    out  -- Parse this?
)

--------------------------------------------------------
-- gfan_substitute
--------------------------------------------------------

gfanSubstitute = method(Options=>{"help"=>false})
gfanSubstitute (List, PolynomialRing) := opts -> (L,R) -> (
    argStrs := {"--help"};
    args := {opts#"help"};
    out := runGfanCommand("gfan_substitute",
                          argStrs, args,
                          gfanMPLToRingToString(L) | gfanMPLToString(L) | gfanRingToString(R));
    if opts#"help" then return out;
    gfanParseMarkedIdeal out 
)


--------------------------------------------------------
-- gfan_tolatex
--------------------------------------------------------

gfanToLatex = method(Options=>{"h"=>false, "polynomialset"=>false, "polynomialsetlist"=>false, "help"=>false})
gfanToLatex (List) := opts -> (L) -> (
    argStrs := {"-h", "--polynomialset_", "--polynomialsetlist_", "--help"};
    args := {opts#"h", opts#"polynomialset", opts#"polynomialsetlist", opts#"help"};
    
    out := if opts#"polynomialsetlist" then
		runGfanCommand("gfan_tolatex",
                          argStrs, args,
                          gfanLMPLToString(L))
		else
		runGfanCommand("gfan_tolatex",
                          argStrs, args,
                          gfanMPLToString(L));
    if opts#"help" then return out;
    out 
)

--------------------------------------------------------
-- gfan_tropicalbasis
--------------------------------------------------------

gfanTropicalBasis = method(Options=>{"h"=>false, "help"=>false})
gfanTropicalBasis (Ideal) := opts -> (I) -> (
    argStrs := {"-h", "--help"};
    args := {opts#"h", opts#"help"};
    out := runGfanCommand("gfan_tropicalbasis",
                          argStrs, args,
                          gfanRingToString(ring I) | gfanIdealToString(I));
    if opts#"help" then return out;
    gfanParseIdeal out 
)

--------------------------------------------------------
-- gfan_tropicalintersection
--------------------------------------------------------

gfanTropicalIntersection = method(Options=>{"t"=>false, "tplane"=>false, "help"=>false})
gfanTropicalIntersection (List) := opts -> (L) -> (
    argStrs := {"-t", "--tplane", "--help"};
    args := {opts#"t", opts#"tplane", opts#"help"};
    out := runGfanCommand("gfan_tropicalintersection",
                          argStrs, args,
                          gfanRingToString(ring first L) | gfanPolynomialListToString(L));
    if opts#"help" then return out;
    out 
)

--------------------------------------------------------
-- gfan_tropicalstartingcone
--------------------------------------------------------

gfanTropicalStartingCone = method(Options=>{"g"=>false, "d"=>false, "help"=>false})
gfanTropicalStartingCone (List) := opts -> (L) -> (
    argStrs := {"-g", "-d", "--help"};
    args := {opts#"g", opts#"d", opts#"help"};
    out := runGfanCommand("gfan_tropicalstartingcone",
                          argStrs, args,
                          gfanRingToString(ring first L) | gfanPolynomialListToString(L));
    if opts#"help" then return out;
    gfanParseMarkedIdealPair out 
)


--------------------------------------------------------
-- gfan_tropicaltraverse
--------------------------------------------------------

gfanTropicalTraverse = method(Options=>{"symmetry"=>false, "noincidence"=>false, "help"=>false})
gfanTropicalTraverse (List) := opts -> (L) -> (
    argStrs := {"--symmetry", "--noincidence", "--help"};
    args := {opts#"symmetry", opts#"noincidence", opts#"help"};
    out := runGfanCommand("gfan_tropicaltraverse",
                          argStrs, args,
                          gfanMPLToRingToString(first L) | gfanMPLToString(first L) | gfanMPLToString(last L));
    if opts#"help" then return out;
    out  --- PARSE POLYHEDRAL DATA
)

gfanTropicalTraverse (List, List) := opts -> (L, S) -> (
    argStrs := {"--symmetry", "--noincidence", "--help"};
    args := {opts#"symmetry", opts#"noincidence", opts#"help"};
    out := runGfanCommand("gfan_tropicaltraverse",
                          argStrs, args,
                          gfanMPLToRingToString(first L) | gfanMPLToString(first L) | gfanMPLToString(last L) | gfanSymmetriesToString(S));
    if opts#"help" then return out;
    out  --- PARSE POLYHEDRAL DATA
)


--------------------------------------------------------
-- gfan_tropicalmultiplicity
--------------------------------------------------------

gfanTropicalMultiplicity = method(Options=>{"help"=>false})
gfanTropicalMultiplicity (List) := opts -> (L) -> (
    argStrs := {"--help"};
    args := {opts#"help"};
    out := runGfanCommand("gfan_tropicalmultiplicity",
                          argStrs, args,
                          gfanMPLToRingToString(L) | gfanMPLToString(L));
    if opts#"help" then return out;
    gfanParseInteger out 
)


--------------------------------------------------------
-- gfan_saturation
--------------------------------------------------------

gfanSaturation = method(Options=>{"help"=>false})
gfanSaturation (Ideal) := opts -> (I) -> (
    argStrs := {"--help"};
    args := {opts#"help"};
    out := runGfanCommand("gfan_saturation",
                          argStrs, args,
                          gfanRingToString(ring I) | gfanIdealToString(I));
    if opts#"help" then return out;
    gfanParseIdeal out 
)

--------------------------------------------------------
-- gfan_tropiciallifting
--------------------------------------------------------

gfanTropicalLifting = method(Options=>{"help"=>false})
gfanTropicalLifting := opts -> () -> (
	argStrs := {"--help"};
	args := {opts#"help"};
	if opts@"help" then
		runGfanCommand("gfan_tropicallifting", argStrs, args, "")
	else
		error "Not implemented";
)

--------------------------------------------------------
-- gfan_topolyhedralfan
--------------------------------------------------------

gfanToPolyhedralFan = method(Options=>{"restrict"=>false, "symmetry"=>false, "help"=>false})
gfanToPolyhedralFan List := opts -> (L) -> (
    argStrs := {"--restrict", "--symmetry", "--help"};
    args := {opts#"restrict", opts#"symmetry", opts#"help"};
    out := runGfanCommand("gfan_topolyhedralfan",
                          argStrs, args,
                          gfanMPLToRingToString(first L) | gfanLMPLToString(L));
    if opts#"help" then return out;
    out   ---- PARSE AS POLYHEDRAL DATA
)

gfanToPolyhedralFan (List, List) := opts -> (L, S) -> (
    argStrs := {"--restrict", "--symmetry", "--help"};
    args := {opts#"restrict", opts#"symmetry", opts#"help"};
    out := runGfanCommand("gfan_topolyhedralfan",
                          argStrs, args,
                          gfanMPLToRingToString(first L) |  gfanSymmetriesToString(S) | gfanLMPLToString(L));
    if opts#"help" then return out;
    out   ---- PARSE AS POLYHEDRAL DATA
)

--------------------------------------------------------
-- gfan_tropicalbruteforce
--------------------------------------------------------

gfanTropicalBruteForce = method(Options=>{"help"=>false})
gfanTropicalBruteForce List := opts -> (L) -> (
    argStrs := {"--help"};
    args := {opts#"help"};
    out := runGfanCommand("gfan_tropicalbruteforce",
                          argStrs, args,
                          gfanMPLToRingToString(L) | gfanMPLToString(L));
    if opts#"help" then return out;
    out   ---- PARSE AS POLYHEDRAL DATA
)

beginDocumentation()


gfanFunctions = {
	gfan, 
	gfanBuchberger,
	gfanDoesIdealContain,
	gfanGroebnerCone,
	gfanHomogeneitySpace,
	gfanHomogenize,
	gfanInitialForms,
	gfanInteractive,
	gfanIsMarkedGroebnerBasis,
	gfanKrullDimension,
	gfanLeadingTerms,
	gfanMarkPolynomialSet,
	gfanPolynomialSetUnion,
	gfanRender, 
	gfanRenderStaircase, 
	gfanStats,
	gfanSubstitute,
	gfanToLatex,
	gfanTropicalBasis,
	gfanTropicalIntersection,
	gfanTropicalLifting,
	gfanTropicalStartingCone,
	gfanTropicalTraverse,
	gfanTropicalMultiplicity,
	gfanSaturation,
	gfanToPolyhedralFan,
	gfanTropicalBruteForce
}

gfanFunctionNames = {
	"gfan", 
	"gfan_buchberger",
	"gfan_doesidealcontain",
	"gfan_groebnercone",
	"gfan_homogeneityspace",
	"gfan_homogenize",
	"gfan_initialforms",
	"gfan_interactive",
	"gfan_ismarkedgroebnerbasis",
	"gfan_krulldimension",
	"gfan_leadingterms",
	"gfan_markpolynomialset",
	"gfan_polynomialsetunion",
	"gfan_render", 
	"gfan_renderstaircase", 
	"gfan_stats",
	"gfan_substitute",
	"gfan_tolatex",
	"gfan_tropicalbasis",
	"gfan_tropicalintersection",
	"gfan_tropicallifting",
	"gfan_tropicalstartingcone",
	"gfan_tropicaltraverse",
	"gfan_tropicalmultiplicity",
	"gfan_saturation",
	"gfan_topolyhedralfan",
	"gfan_tropicalbruteforce"
}


--scan(4..(#gfanFunctions)-1, i -> 
 --   document(
--	{ Key => {gfanFunctions#i} | toList methods gfanFunctions#i } 
--	| apply( lines runGfanCommandCaptureError(gfanFunctionNames#i, {"--help"}, {true}, "") , l->PARA {l}) 
   --)
--)

gfanHelp = hashTable apply(#gfanFunctions, i -> 
	gfanFunctionNames#i => apply( lines runGfanCommandCaptureError(gfanFunctionNames#i, {"--help"}, {true}, "") , l->PARA {l}) 
)

doc ///
	Key
		"gfanInterface2"
	Headline
		a Macaulay2 interface to gfan
	Description
		Text
			@EM "gfanInterface2"@ is an interface to Anders Jenssen's gfan package, which is a C++
			program to compute the Groebner fan (i.e. all the initial ideals) of an ideal.

			The main function in this package is @TO gfan@ which computes all of the Groebner 
			bases and initial ideals of a given ideal.  A useful feature of this function is 
			that it can handle symmetries in the ideal. If you want the geometric information 
			of this list of Groebner basis, see @TO gfanGroebnerCone@.

			Most of the functions in gfanInterface2 required marked Groebner bases as input. 
			We represent marked Groebner bases as pairs of lists, where the first list 
			contains the monomial leading terms of the Groebner basis in the second list. 
			See @TO "Marked Groebner Basis Example"@ for more details.

			New users should read the following guides:

			@TO "Installation and Configuration of gfanInterface2"@

			@TO "Conventions for calling methods with options"@

			Most of functions in the gfan package are accessible through this interface.
			If you wish to use one whose interface is not included here send a message to 
			the package author. Also, please feel free to suggest changes to the
			parameter types and return types of each method. 
///

doc ///
	Key
		"Marked Groebner Basis Example"
	Description
		Text
			A marked Groebner basis is a set of polynomials which forms a 
			Groebner basis in which the leading terms of each polynomial have been
			distinguished.

			In gfan, the leading terms of polynomials are marked by writing them first. 
			For example, the leading term of @TEX "$y^2 + x^2 + z^2$"@ is @TEX "$y^2$"@ as 
			it appears first. In Macaulay 2, polynomials are sorted based on the term 
			order of the ring and so distinguished terms are lost if they do not correspond 
			to the current ring's term order.

		Example
			QQ[x,y,z];
			y^2 + x^2 + z^2

		Text
			In {\tt gfanInterface2}, we represent marked Groebner bases using a list 
			of leading terms and a second list of the Groebner basis itself. 
			For example, we take a list of polynomials and call @TO gfanMarkPolynomialSet@ 
			which takes a list of polynomials and a weight vector and returns a list of 
			marked polynomials. In this case, the leading term is first computed using 
			the weight vector and then lexicographic order to break ties.

		Example
			gfanMarkPolynomialSet({x*y^3+z^4, x^2*z^2 + y^3*z}, {-1,2,5})
///

doc ///
	Key
		"Installation and Configuration of gfanInterface2"
	Description
		Text
			In order to use the {\tt gfanInterface2} in Macaulay 2, {\tt gfan} by Anders Jenssen
			must already be installed. In addition, {\tt gfanInterface2} must know the path
			of the {\tt gfan} executables. This can be specified when loading the package:

		Example
			loadPackage("gfanInterface2", Configuration => { "path" => "/directory/to/gfan/"})

		Text
			The path to the executables should end in a slash. 
			To set the path permanently, one needs to change 
			{\tt gfanInterface2.m2} either before installing or in the installed copy.
			You will find the path configuration near the top of the file.

			If {\tt gfanInterface2} is already installed and loaded, you can find the path
			of the source file by the following command:

		Example
			gfanInterface2#"source file"

		Text
			Finally, if you want to be able to render Groebner fans and monomial staircases
			to {\tt .png} files, you should install {\tt fig2dev} and specify its path
			as follows:

		Example
			loadPackage("gfanInterface2", Configuration => { "fig2devpath" => "/directory/to/fig2dev/"})

		Text
			Again, the path should end in a slash.
///


doc ///
	Key
		"Conventions for calling methods with options"
	Description
		Text
			In creating {\tt gfanInterface2} the objective has been to mirror
			the {\tt gfan} commands as closely as possible in Macaulay 2. 
			Many commands in {\tt gfan} allow command line options and these
			reproduced in {\tt gfanInterface2} as optional arguments. 

			For example, say we want to find the Groebner bases of 
			an ideal with symmetry. From the command line, one would
			type @TT "gfan --symmetry"@ and then give the ring, ideal and 
			symmetries of the ideal as input.

			In {\tt gfanInterface2} we pass the optional argument {\tt "symmetry"} the
			value {\tt true} and provide the ideal and symmetries as arguments.

		Example
			QQ[x,y,z]; 
			gfan(ideal(x^2*y -z, y^2*z - x, z^2*x - y), {{0,1,2}, {1,2,0}}, "symmetry"=>true)

		Text
			If the symmetries are given as an argument, but the {\tt "symmetry"}
			option is not set, the symmetries will not be used.

			For each optional {\tt gfan} argument, the corresponding {\tt gfanInterface2}
			argument is obtained by simply removing the dashes.

			Here's another example. If we run {\tt gfanBuchberger} without a weight vector,
			it will use the lexicographic order.

		Example
			QQ[x,y,z];
			gfanBuchberger(ideal(x,y+z))

		Text
			If we want to use a different order, the {\tt gfan} documentation tells us to
			use the {\tt -w} argument. So, in Macaulay 2, we set the {\tt w} argument to be true.

		Example
			QQ[x,y,z];
			gfanBuchberger(ideal(x,y+z),{1,2,3}, "w"=>true)
///

doc ///
	Key
		gfan
		(gfan, Ideal)
		(gfan, Ideal, List)
		(gfan, List)
		(gfan, List, List)
	Headline
		all reduced Groebner bases of a polynomial ideal
	Usage
		G = gfan(I)
		G = gfan(I,S)
		G = gfan(L)
		G = gfan(L,S)
	Inputs
		I:Ideal
			contained in a polynomial ring
		S:List
			symmetries of {\tt I}
		L:List
			a @TO2 {"Marked Groebner Basis Example", "marked Groebner basis"}@ (for use with option g)
	Outputs 
		G:List
			all @TO2 {"Marked Groebner Basis Example", "marked reduced Groebner bases"}@ of {\tt I} or {\tt L}
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan"@
///

doc ///
	Key
		gfanBuchberger
		(gfanBuchberger, Ideal)
		(gfanBuchberger, Ideal, List)
	Headline
		reduced Groebner basis with respect to some monomial order
	Usage
		G = gfanBuchberger(I)
		G = gfanBuchberger(I,W)
	Inputs
		I:Ideal
			contained in a polynomial ring
		W:List
			a weight vector
	Outputs
		G:List
			a @TO2 {"Marked Groebner Basis Example", "marked reduced Groebner basis"}@ of {\tt I}
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_buchberger"@
///

doc ///
	Key
		gfanDoesIdealContain
		(gfanDoesIdealContain, List, List)
	Headline
		check ideal membership by the division algorithm
	Usage
		B = gfanDoesIdealContain(L,K)
	Inputs
		L:List
			a @TO2 {"Marked Groebner Basis Example", "marked Groebner basis"}@
		K:List
			a list of polynomials
	Outputs
		B:Boolean
			true if every polynomial in {\tt K} belongs to the ideal generated by {\tt L}
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_doesidealcontain"@
///

doc ///
	Key
		gfanGroebnerCone
		(gfanGroebnerCone, List)
	Headline
		polyhedral information about a Groebner cone
	Usage
		S = gfanGroebnerCone(L)
	Inputs
		L:List
			of @TO2 {"Marked Groebner Basis Example", "marked polynomials"}@
	Outputs
		S:String
			a description of the Groebner cone of {\tt L}
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_groebnercone"@
///

doc ///
	Key
		gfanHomogeneitySpace
		(gfanHomogeneitySpace, List)
	Headline
		homogeneity space of a list of polynomials
	Usage
		gfanHomogeneitySpace(L)
	Inputs
		L:List
			of polynomials
	Outputs
		S:String
			polymake data with a lineality space of all weight vectors for which {\tt L} is homogeneous.
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_homogeneityspace"@
///

doc ///
	Key
		gfanHomogenize
		(gfanHomogenize, List, Symbol)
		(gfanHomogenize, List, List, Symbol)
	Headline
		homogenize a list of polynomials with respect to a weight vector
	Usage
		H = gfanHomogenize(L,X)
		H = gfanHomogenize(L,W,X)
	Inputs
		L:List
			of polynomials
		X:Symbol
			the homogenizing variable
		W:List
			a weight vector
	Outputs
		H:List
			polynomials from {\tt L} homogenized with variable {\tt X} (according to weights {\tt W})
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_homogenize"@
///

doc ///
	Key
		gfanInitialForms
		(gfanInitialForms, List, List)
	Headline
		initial forms of polynomials with respect to a weight vector
	Usage
		F = gfanInitialForms(L,W)
	Inputs
		L:List
			of polynomials
		W:List
			a weight vector
	Outputs
		F:List
			initial forms of the polynomials in {\tt L} with respect to weight {\tt W}
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_initialforms"@
///

doc ///
	Key
		gfanInteractive
	Headline
		not implemented
	Description
		Text
			This method is not implemented.

			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_interactive"@
///

doc ///
	Key
		gfanIsMarkedGroebnerBasis
		(gfanIsMarkedGroebnerBasis, List)
	Headline
		check if a list of marked polynomials are a Groebner basis
	Usage
		B = gfanIsMarkedGroebnerBasis(L)
	Inputs
		L:List
			of @TO2 {"Marked Groebner Basis Example", "marked polynomials"}@
	Outputs
		B:Boolean
			true if {\tt L} forms a Groebner basis
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_ismarkedgroebnerbasis"@
///

doc ///
	Key
		gfanKrullDimension
		(gfanKrullDimension, List)
	Headline
		krull dimension
	Usage
		D = gfanKrullDimension(L)
	Inputs
		L:List
			a @TO2 {"Marked Groebner Basis Example", "marked Groebner basis"}@
	Outputs
		D:ZZ
			the Krull dimension of the polynomial ring modulo the ideal generated by {\tt L}
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_krulldimension"@
///

doc ///
	Key
		gfanLeadingTerms
		(gfanLeadingTerms, List)
	Headline
		leading terms of a list (or list of lists) of marked polynomials
	Usage
		T = gfanLeadingTerms(L)
	Inputs
		L:List
			of @TO2 {"Marked Groebner Basis Example", "marked polynomials"}@ (or, for the {\tt m} option, a lists of marked polynomials 
	Outputs
		T:List
			the leading terms of {\tt L} (or lists of the leading terms of each list in {\tt L})
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_leadingterms"@
///

doc ///
	Key
		gfanMarkPolynomialSet
		(gfanMarkPolynomialSet, List, List)
	Headline
		mark the initial terms of a list of polynomials with respect to a weight vector
	Usage
		M = gfanMarkPolynomialSet(L,W)
	Inputs
		L:List
			of polynomials
		W:List
			a weight vector
	Outputs
		M:List
			the polynomials in {\tt L} @TO2 {"Marked Groebner Basis Example", "marked"}@ with respect to {\tt W}
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_markpolynomialset"@
///

doc ///
	Key
		gfanPolynomialSetUnion
		(gfanPolynomialSetUnion, List, List)
	Headline
		union of two lists of polynomials
	Usage
		U = gfanPolynomialSetUnion(L,K)
	Inputs
		L:List
			of polynomials
		K:List
			of polynomials
	Outputs
		U:List
			the union of lists {\tt L} and {\tt K}
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_polynomialsetunion"@
///

doc ///
	Key
		gfanRender
		(gfanRender, List)
	Headline
		render an image of a Grobener fan
	Usage
		gfanRender(L)
	Inputs
		L:List
			all @TO2 {"Marked Groebner Basis Example", "marked reduced Groebner bases"}@ of {\tt I} or an ideal 
	Description
		Text
			This method renders the Groebner fan of an ideal and writes it to a {\tt .fig} file.
			If {\tt fig2dev} is installed then the {\tt .fig} file will be converted to {\tt .png}
			and displayed. The names of the {\tt .fig} and {\tt .png} files are printed on 
			standard out.

			This method may support more options in the future to control rendering, conversion,
			and display. It may also eventually output file names in a list.

			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_render"@
///

doc ///
	Key
		gfanRenderStaircase
		(gfanRenderStaircase, List)
	Headline
		render the staircase of a monomial initial ideal
	Usage
		gfanRenderStaircase(L)
	Inputs
		L:List
			a @TO2 {"Marked Groebner Basis Example", "marked Groebner basis"}@ }
			or lists of @TO2 {"Marked Groebner Basis Example", "marked Groebner bases"}@ }
			(for use with the {\tt m} option)
	Description
		Text
			This method renders the staircase of the monomial initial ideal of the given
			Groebner basis (or bases in the case of the {\tt m} option).
			The rendered image is written to a {\tt .fig} file.
			If {\tt fig2dev} is installed then the {\tt .fig} file will be converted to {\tt .png}
			and displayed. The names of the {\tt .fig} and {\tt .png} files are printed on 
			standard out.

			This method may support more options in the future to control rendering, conversion,
			and display. It may also eventually output file names in a list.

			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_renderstaircase"@
///

doc ///
	Key
		gfanSaturation
		(gfanSaturation, Ideal)
	Headline
		saturation of a ideal
	Usage
		gfanSaturation(I)
	Inputs
		I:Ideal
	Outputs
		L:List
			polynomials generating the saturation of {\tt I} with the product of the 
			variables of the ring of {\tt I}
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_saturation"@
///

doc ///
	Key
		gfanStats
		(gfanStats, List)
	Headline
		display certain properties of a list of polynomials
	Usage
		gfanStats(L)
	Inputs
		L:List
			containing lists of @TO2 {"Marked Groebner Basis Example", "marked Groebner bases"}@
	Outputs
		S:String
			describing the bases in {\tt L}
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_stats"@
///

doc ///
	Key
		gfanSubstitute
		(gfanSubstitute, List, PolynomialRing)
	Headline
		rename the variables of a list of polynomials
	Usage
		gfanSubstitute(L,R)
	Inputs
		L:List
			of polynomials
		R:PolynomialRing
			with the name number of variables as the ring of the polynomials in {\tt L}
	Outputs
		L:List
			of polynomials from {\tt L} with variables replaced by those in {\tt R}
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_substitute"@
///

doc ///
	Key
		gfanToLatex
		(gfanToLatex, List)
	Headline
		convert a list of polynomials to LaTeX
	Usage
		gfanToLatex(L)
	Inputs
		L:List
			of polynomials or lists of polynomials (for the {\tt polynomialsetlist} option)
	Outputs
		S:String
			LaTeX math for {\tt L}
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_tolatex"@
///

doc ///
	Key
		gfanToPolyhedralFan
		(gfanToPolyhedralFan, List)
		(gfanToPolyhedralFan, List, List)
	Headline
		polyhedral data about the fan of a list of Groebner bases
	Usage
		gfanToPolyhedralFan(L)
		gfanToPolyhedralFan(L,S)
	Inputs
		L:List
			containing lists of @TO2 {"Marked Groebner Basis Example", "marked reduced Groebner bases"}@
		S:List
			symmetries of {\tt L}
	Outputs
		P:String
			polymake data describing the fan of the Groebner basis in {\tt L}
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_topolyhedralfan"@
///

doc ///
	Key
		gfanTropicalBasis
		(gfanTropicalBasis, Ideal)
	Headline
		tropical basis of an ideal
	Usage
		gfanTropicalBasis(I)
	Inputs
		I:Ideal
	Outputs
		L:List
			of polynomials that give a tropical basis of {\tt I}
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_tropicalbasis"@
///

doc ///
	Key
		gfanTropicalBruteForce
		(gfanTropicalBruteForce, List)
	Headline
		polyhedral data describing the tropical variety of an ideal
	Usage
		gfanTropicalBruteForce(L)
	Inputs
		L:List
			a @TO2 {"Marked Groebner Basis Example", "marked reduced Groebner basis"}@ for a homogeneous ideal
	Outputs
		P:String
			polymake data describing the tropical variety of the ideal of {\tt L}
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_tropicalbruteforce"@
///

doc ///
	Key
		gfanTropicalIntersection
		(gfanTropicalIntersection, List)
	Headline
		polyhedral data describing intersection of tropical hypersurfaces
	Usage
		gfanTropicalIntersection(L)
	Inputs
		L:List
			of polynomials
	Outputs
		P:String
			polymake data describing the intersection of the tropical hypersurfaces of polynomials in {\tt L}
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_tropicalintersection"@
///

doc ///
	Key
		gfanTropicalLifting
	Headline
		not implemented
	Description
		Text
			This method is not implemented.

			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_tropicallifting"@
///

doc ///
	Key
		gfanTropicalMultiplicity
		(gfanTropicalMultiplicity, List)
	Headline
		multiplicity of a tropical cone
	Usage
		gfanTropicalMultiplicity(L)
	Inputs
		L:List
			a @TO2 {"Marked Groebner Basis Example", "marked reduced Groebner basis"}@
	Outputs
		M:ZZ
			the multiplicity of the tropical cone of {\tt L}
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_tropicalmultiplicity"@
///

doc ///
	Key
		gfanTropicalStartingCone
		(gfanTropicalStartingCone, List)
	Headline
		a pair of Groebner bases for use with gfanTropicalTraverse
	Usage
		gfanTropicalStartingCone(L)
	Inputs
		L:List
			of homogeneous polynomials
	Outputs
		P:List
			a pair of @TO2 {"Marked Groebner Basis Example", "marked reduced Groebner bases"}@
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_tropicalstartingcone"@
	SeeAlso
		gfanTropicalTraverse
///

doc ///
	Key
		gfanTropicalTraverse
		(gfanTropicalTraverse, List)
		(gfanTropicalTraverse, List, List)
	Headline
		polyhedral data describing a tropical variety
	Usage
		gfanTropicalTraverse(L)
		gfanTropicalTraverse(L,S)
	Inputs
		L:List
			a pair of @TO2 {"Marked Groebner Basis Example", "marked reduced Groebner bases"}@ 
		S:List
			symmetries of the second Groebner basis in {\tt L}
	Outputs
		P:String
			polymake data describing the tropical variety of the given ideal
	Description
		Text
			@STRONG "gfan Documentation"@
			@gfanHelp#"gfan_tropicaltraverse"@
///

end

-------------------------------------------------------------
-------------------------------------------------------------
-------------------------------------------------------------

restart
uninstallPackage "gfanInterface2"
installPackage("gfanInterface2", UserMode=>true, DebuggingMode=>true)
loadPackage "gfanInterface2"
viewHelp

R = QQ[x,y,z]; 
I = ideal(x,y+z); 
L = transpose {{x,x},{z,y+z}};
gfan(I)
gfan(ideal(x^2*y -z, y^2*z - x, z^2*x - y), {{0,1,2}, {1,2,0}}, "symmetry"=>true)
gfan(transpose{{x,x},{z,y+z}})
gfanBuchberger(I)
gfanBuchberger(I,{3,2,1}, "w"=>true)
gfanDoesIdealContain(L, {x})
gfanDoesIdealContain(L, {y})
gfanGroebnerCone(transpose{{x,x},{y,y+z}})
gfanGroebnerCone(transpose{{x,x},{y,y+z}}, "restrict"=>true)
gfanGroebnerCone(transpose{{x,x},{y,y+z}}, "asfan"=>true)
gfanGroebnerCone({transpose{{x,x},{y,y+z}},transpose{{y,y+z},{x,x}}}, "pair"=>true)
gfanHomogeneitySpace(transpose{{x,x},{y,y+z}})
gfanHomogenize({z+1},a)
gfanInitialForms({z+x+y},{2,1,2})
gfanInitialForms({z+x+y},{2,1,2}, "pair"=>true)
gfanInitialForms({z+x+y},{2,1,2}, "ideal"=>true)
gfanIsMarkedGroebnerBasis(transpose{{y,y+z},{x, x+1}})
gfanKrullDimension(transpose{{x,x+1}})
gfanLeadingTerms(transpose{{x,x+y},{z,z+x+y}})
gfanLeadingTerms({transpose{{x,x+y},{z,z+x+y}},transpose{{x*y,x*y + y^2}}}, "m"=>true)
gfanMarkPolynomialSet({x+y+z}, {1,1,2})
gfanPolynomialSetUnion({x},{x+y})
gfanRender(gfan(ideal(x*y+z, y^3+x*z)))
gfanRenderStaircase(transpose{{x,x},{y^3,y^3}, {z^2,z^2}})
gfanRenderStaircase({transpose{{x,x},{y^3,y^3}, {z^2,z^2}},transpose{{x,x},{y^3,y^3}, {z^2,z^2}}}, "m"=>true)
gfanStats({transpose{{x,x+y},{z,z}},transpose{{y,y+x},{z,z}}})
gfanSubstitute(transpose{{y,y+x},{z,z}}, QQ[a,b,c])
gfanToLatex(transpose{{y,y+x},{z,z}}, "polynomialset"=>true)
gfanToLatex({transpose{{y,y+x}},transpose{{z,z}}}, "polynomialsetlist"=>true)
gfanToLatex({transpose{{y,y+x}},transpose{{z,z}}}, "polynomialsetlist"=>true, "h"=>true)
gfanTropicalBasis(ideal{x^2+x*y, z^2})
gfanTropicalBasis(ideal{x^2+y, z^2}, "h"=>true)
gfanTropicalIntersection({x^2+y, z^2})
gfanTropicalIntersection({x^2+y, z^2}, "t"=>true)
gfanTropicalStartingCone({x^2+y*x, x*y+z^2})
gfanTropicalStartingCone({x^2+y*x, x*y+z^2}, "d"=>true)
gfanTropicalTraverse(gfanTropicalStartingCone({x^2+y*x, x*y+z^2}))
gfanTropicalMultiplicity(first gfanTropicalStartingCone({x^2+y*x, x*y+z^2}))
gfanSaturation(ideal(z*y-z*x))
gfanToPolyhedralFan({transpose{{x*y, x*y+ z}, {z,z}}, transpose{{x*y, x*y},{z,z}}})
gfanToPolyhedralFan({transpose{{x*y, x*y+ z}, {z,z}}, transpose{{x*y, x*y},{z,z}}}, "restrict"=>true)
gfanToPolyhedralFan(gfan(ideal(x^2*y -z, y^2*z - x, z^2*x - y), {{0,1,2}, {1,2,0}}, "symmetry"=>true), {{0,1,2}, {1,2,0}}, "symmetry"=>true)
gfanTropicalBruteForce(transpose{{x*y,x*y + z}, {z,z}})

