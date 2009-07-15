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
toOpenMath = method()

toOpenMath String := x -> OMSTR(x)
toOpenMath ZZ     := x -> OMI(x)

toOpenMath RR     := x -> (
	sx := toExternalString x;
	m := regex("^([0-9\\.\\-]+)p([0-9]+)e([0-9\\-]+)$", sx);
	if m === null then return OME("RR elt does not match regex");
	
	OMA("bigfloat1", "bigfloatprec", {
		OMA("bigfloat1", "bigfloat", 
			{ 	OMF(substring(m#1, sx)), 
				OMI(10), 
				OMI(substring(m#3, sx)) 
			}),
		OMI(2),
		OMI(substring(m#2, sx))
	})
)

-- toOpenMath of an XMLnode will be just toOpenMath of that XMLnode
toOpenMath XMLnode := x -> x;

-- product will have to branch to different things...
toOpenMath Product := x -> (
	vx := value(x);
	if class(class(vx)) === PolynomialRing then
		--defined in polyd1
		toOpenMathFactoredPol(x)
	else
		OME(concatenate("Cannot handle product whose class is ", toString(class(vx))))
)

-- Symbols will be mapped to variables
toOpenMath Symbol := x->OMV(toString x);


------------------------
-------- From OM -------
------------------------
OMSEvaluators = new MutableHashTable;

fromOpenMathOMI = x->(
	s := x.children;
	if class(s) =!= String then return OME(concatenate("Illegal OMI: Children has type ", toString(class(s)), "."));
	m := regex("^(\\-)?[0-9]+$", s);
	if m === null then return OME(concatenate("Illegal OMI: '", s, "'"));
	value(s)
)
fromOpenMathOMF = x->(
	if not x#?"dec" then return OME("Illegal OMF: no \"dec\"");
	s := x#"dec";
	if class(s) =!= String then return OME(concatenate("Illegal OMF: \"dec\" has type ", toString(class(s)), "."));
	m := regex("^(\\-)?[0-9\\.]+$", s);
	if m === null then return OME(concatenate("Illegal OMF: '", s, "'"));
	value(s)
)
fromOpenMathOMSTR = x->(
	s := x.children;
	if class(s) =!= String then return OME(concatenate("Illegal OMSTR: children has type ", toString(class(s)), "."));
	s
)
fromOpenMathOMOBJ = x->(
	s := x.children;
	if class(s) =!= HashTable then return OME(concatenate("Illegal OMOBJ: children has type ", toString(class(s)), "."));
	fromOpenMath(s)
)
fromOpenMathOMS = x->(
	if not x#?"cd" then return OME("OMS has no cd");
	if not x#?"name" then return OME("OMS has no name");
		
	if OMSEvaluators#?(x#"cd") and OMSEvaluators#(x#"cd")#?(x#"name") then
		-- We can parse it!
		OMSEvaluators#(x#"cd")#(x#"name")
	else
		-- We cannot parse it -- leave as is.
		x
)

fromOpenMathOMA = x->(
	c := x.children;
	if class(c) =!= List then return OME(concatenate("Illegal OMA: children has type ", toString(class(c)), "."));
	if #c === 0 then return OME("Illegal OMA: has no children.");
	hd := fromOpenMath(c#0);
	attrs := if x.?attributes then x.attributes else null;

	if class(hd) === XMLnode then (
		-- We cannot parse it -- leave as is.
		print concatenate("WARNING -- Could not parse application of ", toString(hd));
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
		return OME("Illegal OMV: no \"name\"");
	
	vname := x#"name";
	
	--If we stored the value of the variable in storedOMVvals
	-- (i.e. if we're evaluating an OMBIND) then we return that
	if storedOMVvals#?vname then (
		return storedOMVvals#vname;
	);
		
	--Otherwise we turn it into a symbol.
	vname = replace("_", "$", toString(vname));
	if regex("^[a-zA-Z][a-zA-Z0-9\\$]*$", vname) === null then 
		return OME(concatenate("Illegal variable name: '", x#"name", "'"));

	value(concatenate("symbol ", vname))
)

evalBind = (hd, ombvars, expr, vals) -> (
	-- Check that we can evaluate the OMBIND
	fhd := null;
	try 
		fhd = fromOpenMath(hd)
	else
		return OME(concatenate("Cannot evaluate OMBIND with head '", toString hd , "'"));
	if class(fhd) === XMLnode then
		return OME(concatenate("Cannot evaluate OMBIND with head '", toString hd , "'"));


	-- We assume vars is an OMBVAR
	-- Store the values (as a stack)
	if class(ombvars) =!= XMLnode or ombvars.tag =!= "OMBVAR" then
		return OME("evalBind: ombvars should be an XMLnode of type OMBVAR");
	vars := ombvars.children;
	for i in 0..(#vars-1) do (
		v := vars#i;
		if class(v) =!= XMLnode or v.tag =!= "OMV" then 
			return OME("evalBind: vars are not all OMVs");
		
		--"Push"
		vname := v#"name";
		if storedOMVvals#?vname then
			storedOMVvals#vname = (fromOpenMath(vals#i), storedOMVvals#vname)
		else
			storedOMVvals#vname = fromOpenMath(vals#i);
	);
	
	-- Evaluate the bind...
	r := fhd(expr);
		
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
		return OME("Cannot evaluate an OMBIND with less than 3 children");
		
	hd := (x.children)#0;
	vars := (x.children)#1;
	expr := take(x.children, {2,(#(x.children)-1)});
	
	--We return a function that can be applied to things.
	vals -> evalBind(hd, vars, expr, vals)
)

fromOpenMathOMATTR := x -> (
	--we simply put the attributes as a hashtable into the child node, 
	--possibly to be looked at later.
	if #(x.children) =!= 2 then
		return OME("OMATTR should have exactly two children");
	
	--OMATP
	omatp := (x.children)#0;
	if omatp.tag =!= "OMATP" or (#(omatp.children) % 2 =!= 0) then
		return OME("1st argument of OMATTR should be an OMATP with an even number of children");
	chd := omatp.children;
	attrs := {};
	for i in 0..((#chd // 2)-1) do (
		attrs = append(attrs, chd#(2*i) => chd#(2*i + 1) )
	);
	attrs = new HashTable from attrs;
	
	--The child.
	child := (x.children)#1;
	child = new MutableHashTable from child;
	child.attributes = attrs;
	child = new XMLnode from child;
	fromOpenMath(child)
)



fromOpenMath = method()
fromOpenMath XMLnode := x->(
	t := x.tag;
	if      t === "OMI"   then  fromOpenMathOMI(x)
	else if t === "OMF"   then  fromOpenMathOMF(x)
	else if t === "OMSTR" then  fromOpenMathOMSTR(x)
	else if t === "OMOBJ" then  fromOpenMathOMOBJ(x)
	else if t === "OMA"   then  fromOpenMathOMA(x)
	else if t === "OMS"   then  fromOpenMathOMS(x)
	else if t === "OMV"   then  fromOpenMathOMV(x)
	else if t === "OMBIND"   then  fromOpenMathOMBIND(x)
	else if t === "OMATTR"   then  fromOpenMathOMATTR(x)
	else (
		print concatenate("WARNING -- Could not parse XMLnode with tag ", t);
		x
	)
)

fromOpenMath Thing := x -> (
	print concatenate("fromOpenMath Thing on ", toString class x);
	x
)

