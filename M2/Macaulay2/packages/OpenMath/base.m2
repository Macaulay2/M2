------------------------
-------- Status --------
------------------------
-- From To --
-- OK  OK  OMA       (apply)
-- OK      OMATP     (attribute pair)
-- OK      OMATTR    (attributes)
--         OMB       (Binary data)
-- OK      OMBIND    (Binding)
-- OK      OMBVAR    (Binding variables)
--     1/2 OME       (Error)
-- OK  OK  OMF       (Float)
--         OMFOREIGN (Foreign XML)
-- OK  OK  OMI       (Integers)
-- OK      OMOBJ     ("Object": the root node)
--         OMR       (References)
-- OK  OK  OMSTR     (String)
-- OK  OK  OMS       (Symbol)
-- OK  OK  OMV       (Variable)

------------------------
-------- To OM ---------
------------------------
recentOMProblem = null

toOpenMath = method()
toOpenMath String := x -> OMSTR(x)
toOpenMath ZZ     := x -> OMI(x)

toOpenMath RR     := idCheck(x -> (
	sx := toExternalString x;
	m := regex("^([0-9\\.\\-]+)p([0-9]+)e([0-9\\-]+)$", sx);
	if m === null then (theOMerror = "RR elt does not match regex"; error("whoops"));
	
	OMA("bigfloat1", "bigfloatprec", {
		OMA("bigfloat1", "bigfloat", 
			{ 	OMF(substring(m#1, sx)), 
				OMI(10), 
				OMI(substring(m#3, sx)) 
			}),
		OMI(2),
		OMI(substring(m#2, sx))
	})
))

toOpenMath RingElement := p -> (
	R := class p;
	if isPolynomialRing(R) then (
		toOpenMathPoly p
	) else if (class R === QuotientRing) and (ambient R === ZZ) then (
		OMI(toString p)
	) else if (class R === QuotientRing) then (
		toOpenMathFFelt p
	) else if (class R === GaloisField) then (
		toOpenMathFFelt p
	) else (
		(theOMerror = concatenate("Cannot handle ring element '", toString(p), " ' in '", toString R, "'"); print theOMerror; error("whoops"));
	)
)


-- toOpenMath of an XMLnode will be just toOpenMath of that XMLnode,
-- unless it has attributes, because then it will be wrapped in an OMATTR
toOpenMath XMLnode := x -> (
	if x.?OMattributes and #(x.OMattributes) > 0 then (
		attrs := x.OMattributes;
		clearOMAttr(x);
		OMATTR(x, attrs)
	) else 
		x
)

-- Symbols will be mapped to variables
toOpenMath Symbol := x->OMV(toString x);

-- Holders are silly
toOpenMath Holder := x -> toOpenMath x#0


------------------------
-------- From OM -------
------------------------
OMSEvaluators = new MutableHashTable;

OMDeclareUnhandledSymbol = method()
OMDeclareUnhandledSymbol (String, String) := (cd,nm) -> (
	OMSEvaluators#cd#nm = (args, attrs) -> (
		(theOMerror = OME(OMS("error", "unhandled_symbol"), {OMS(cd, nm)}); error("whoops"));
	);
)


fromOpenMathOMI = x->(
	s := (x.children)#0;
	if class(s) =!= String then (
		theOMerror = concatenate("Illegal OMI: Children has type ", toString(class(s)), "."); 
		error("whoops")
	);
	m := regex("^(\\-)?[0-9]+$", s);
	if m === null then (theOMerror = concatenate("Illegal OMI: '", s, "'"); error("whoops"));
	value(s)
)
fromOpenMathOMF = x->(
	if not x#?"dec" then (theOMerror = "Illegal OMF: no \"dec\""; error("whoops"));
	s := x#"dec";
	if class(s) =!= String then (
		theOMerror = concatenate("Illegal OMF: \"dec\" has type ", toString(class(s)), "."); 
		error("whoops")
	);
	m := regex("^(\\-)?[0-9\\.]+$", s);
	if m === null then (theOMerror = concatenate("Illegal OMF: '", s, "'"); error("whoops"));
	value(s)
)
fromOpenMathOMSTR = x->(
	s := (x.children)#0;
	if class(s) =!= String then (
		theOMerror = concatenate("Illegal OMSTR: children has type ", toString(class(s)), "."); 
		error("whoops");
	);
	s
)
fromOpenMathOMOBJ = x->(
	s := (x.children)#0;
	if class(s) =!= XMLnode then (
		theOMerror = concatenate("Illegal OMOBJ: children has type ", toString(class(s)), "."); 
		error("whoops");
	);
	fromOpenMath(s)
)
fromOpenMathOMS = x->(
	if not x#?"cd" then (theOMerror = "OMS has no cd"; error("whoops"));
	if not x#?"name" then (theOMerror = "OMS has no name"; error("whoops"));
	
	if OMSEvaluators#?(x#"cd") and OMSEvaluators#(x#"cd")#?(x#"name") then
		-- We can parse it!
		OMSEvaluators#(x#"cd")#(x#"name")
	else (
		-- We cannot parse it -- leave as is.
		recentOMProblem = concatenate("Could not parse application of ", x#"cd", ".", x#"name");
		print concatenate("WARNING -- ", recentOMProblem);		
		x
	)
)

fromOpenMathOMA = x->(
	c := x.children;
	if class(c) =!= List then (
		theOMerror = concatenate("Illegal OMA: children has type ", toString(class(c)), "."); 
		error("whoops");
	);
	if #c === 0 then (
		theOMerror = "Illegal OMA: has no children."; 
		error("whoops");
	);
	hd := fromOpenMath(c#0);
	attrs := if x.?OMattributes then x.OMattributes else null;

	if class(hd) === XMLnode then (
		-- We cannot parse it -- leave as is.
		if hd.tag =!= "OMS" then (
			recentOMProblem = concatenate("Could not parse application of this ", hd.tag);
			print concatenate("WARNING -- ", recentOMProblem)
		);
		x
	) else (
		-- We can parse it!
		hd(take(c, {1,#c-1}), attrs)
	)
)

--The implementation of OMBind is sort of hacky and bad.
storedOMVvals = new MutableHashTable;
fromOpenMathOMV = x->(
	if not x#?"name" then 
		(theOMerror = "Illegal OMV: no \"name\""; error("whoops"));
	
	vname := x#"name";
	
	--If we stored the value of the variable in storedOMVvals
	-- (i.e. if we're evaluating an OMBIND) then we return that
	if storedOMVvals#?vname then (
		return storedOMVvals#vname;
	);
		
	--Otherwise we turn it into a symbol.
	vname = replace("_", "$", toString(vname));
	if regex("^[a-zA-Z][a-zA-Z0-9\\$]*$", vname) === null then 
		(theOMerror = concatenate("Illegal variable name: '", x#"name", "'"); error("whoops"));

	getSymbol(vname)
)

evalBind = (hd, ombvars, expr, vals, attrs) -> (
	-- Check that we can evaluate the OMBIND
	fhd := null;
	try 
		fhd = fromOpenMath(hd)
	else
		(theOMerror = concatenate("Cannot evaluate OMBIND with head '", toString hd , "'"); error("whoops"));
	if class(fhd) === XMLnode then
		(theOMerror = concatenate("Cannot evaluate OMBIND with head '", toString hd , "'"); error("whoops"));

	-- We assume vars is an OMBVAR
	-- Store the values (as a stack)
	if class(ombvars) =!= XMLnode or ombvars.tag =!= "OMBVAR" then
		(theOMerror = "evalBind: ombvars should be an XMLnode of type OMBVAR"; error("whoops"));
	vars := ombvars.children;
	for i in 0..(#vars-1) do (
		v := vars#i;
		if class(v) =!= XMLnode or v.tag =!= "OMV" then 
			(theOMerror = "evalBind: vars are not all OMVs"; error("whoops"));
		
		--"Push"
		vname := v#"name";
		if storedOMVvals#?vname then
			storedOMVvals#vname = (fromOpenMath(vals#i), storedOMVvals#vname)
		else
			storedOMVvals#vname = fromOpenMath(vals#i);
	);
	
	-- Evaluate the bind
	r := fhd(expr, attrs);
		
	-- Delete the values again.
	for v in vars do (
		--"Pop"
		vname := v#"name";
		if class(storedOMVvals#vname) === Sequence then
			storedOMVvals#vname = (storedOMVvals#vname)#1
		else
			remove(storedOMVvals, vname);
	);

	-- Done.
	r
)
fromOpenMathOMBIND := x -> (
	if #(x.children) < 3 then
		(theOMerror = "Cannot evaluate an OMBIND with less than 3 children"; error("whoops"));
		
	hd := (x.children)#0;
	vars := (x.children)#1;
	expr := take(x.children, {2,(#(x.children)-1)});
	
	--We return a function that can be applied to things.
	(vals, attrs) -> evalBind(hd, vars, expr, vals, attrs)
)


fromOpenMathOMATTR := x -> (
	--we simply put the attributes as a hashtable into the child node, 
	--possibly to be looked at later.
	if #(x.children) =!= 2 then
		(theOMerror = "OMATTR should have exactly two children"; error("whoops"));

	--The child.
	child := (x.children)#1;
	
	--OMATP
	omatp := (x.children)#0;
	if omatp.tag =!= "OMATP" or (#(omatp.children) % 2 =!= 0) then
		(theOMerror = "1st argument of OMATTR should be an OMATP with an even number of children"; error("whoops"));
	chd := omatp.children;
	for i in 0..((#chd // 2)-1) do setOMAttr(child, chd#(2*i), chd#(2*i + 1));
	
	--Done! recurse into the child.
	fromOpenMath(child)
)

fromOpenMathOMR := x -> (
	r := null;
	if      x#?"href" then r = x#"href"
	else if x#?"xref" then r = x#"xref"
	else error "OMR without href, without xref";
		
	if existsOMref(r) then
		(getOMref(r))#0
	else (
		--(theOMerror = concatenate("Could not resolve reference '", r, "'"); error("whoops"));
		--Keep the reference around!
		x
	)
)



fromOpenMath = method()

fromOpenMath XMLnode := x->(
	r := null;
	
	--If we know what it is (by the id) then when just use that!
	if x#?"id" and (class(x#"id") === String) and (existsOMref("#"|x#"id")) then (
		r = (getOMref("#"|x#"id"))#0;
		if r =!= null then return r;
	);

	t := x.tag;
	if      t === "OMI"      then  r = fromOpenMathOMI(x)
	else if t === "OMF"      then  r = fromOpenMathOMF(x)
	else if t === "OMSTR"    then  r = fromOpenMathOMSTR(x)
	else if t === "OMOBJ"    then  r = fromOpenMathOMOBJ(x)
	else if t === "OMA"      then  r = fromOpenMathOMA(x)
	else if t === "OMS"      then  r = fromOpenMathOMS(x)
	else if t === "OMV"      then  r = fromOpenMathOMV(x)
	else if t === "OMBIND"   then  r = fromOpenMathOMBIND(x)
	else if t === "OMATTR"   then  r = fromOpenMathOMATTR(x)
	else if t === "OMR"      then  r = fromOpenMathOMR(x)
	else if t === "OME"      then  r = x
	else (
		recentOMProblem = concatenate("Could not parse XMLnode with tag ", t);
		print concatenate("WARNING -- ", recentOMProblem);
		r = x
	);

	--Store the id if it was set
	if x#?"id" then (
		if (class(x#"id") =!= String) then (
			theOMerror = "id was not a string"; error("whoops");
		);
		addOMref("#"|x#"id", r, x);
	);
		
	r
)

fromOpenMath Thing := x -> (
	print concatenate("fromOpenMath Thing on ", toString class x);
	x
)

------------------------------------------------------------------------
-------- The actual conversions, to be used by the user ----------------
-------- With error handling, and automatic reference thing ------------
------------------------------------------------------------------------

guard = f -> x -> (
	theOMerror = null;
	try f x else
		if theOMerror === null then
			OME "internal error: unidentified error"
		else if class theOMerror === String then
			OME theOMerror
		else
			theOMerror
)
openMath = method();
openMath Thing := guard (x -> replaceMultipleIDs toOpenMath x)

value XMLnode := guard fromOpenMath 
val = method();
val XMLnode := guard fromOpenMath
