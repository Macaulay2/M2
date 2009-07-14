--- To OpenMath ---
toOpenMath = method()

toOpenMath String := x -> OMSTR(x)
toOpenMath ZZ     := x -> OMI(x)

toOpenMath RR     := x -> (
	sx := toExternalString x;
	m := regex("^([0-9\\.\\-]+)p([0-9]+)e([0-9\\-]+)$", sx);
	if m === null then error("RR elt does not match regex");
	
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

-- From OpenMath ---
--    OMApply
--    OMBinary
--    OMBind
--    OMError
-- OK OMFloat
--    OMForeign
-- OK OMInteger
-- OK OMObject
--    OMReference
-- OK OMString
--    OMSymbol
--    OMVariable

OMSParsers = new MutableHashTable;

fromOpenMathOMI = x->(
	s := x.children;
	if class(s) =!= String then error(concatenate("Illegal OMI: Children has type ", toString(class(s)), "."));
	m := regex("^(\\-)?[0-9]+$", s);
	if m === null then error(concatenate("Illegal OMI: '", s, "'"));
	value(s)
)
fromOpenMathOMF = x->(
	if not x#?"dec" then error("Illegal OMF: no \"dec\"");
	s := x#"dec";
	if class(s) =!= String then error(concatenate("Illegal OMF: \"dec\" has type ", toString(class(s)), "."));
	m := regex("^(\\-)?[0-9\\.]+$", s);
	if m === null then error(concatenate("Illegal OMF: '", s, "'"));
	value(s)
)
fromOpenMathOMSTR = x->(
	s := x.children;
	if class(s) =!= String then error(concatenate("Illegal OMSTR: children has type ", toString(class(s)), "."));
	s
)
fromOpenMathOMOBJ = x->(
	s := x.children;
	if class(s) =!= HashTable then error(concatenate("Illegal OMOBJ: children has type ", toString(class(s)), "."));
	fromOpenMath(s)
)
fromOpenMathOMS = x->(
	if not x#?"cd" then error("OMS has no cd");
	if not x#?"name" then error("OMS has no name");
	s := concatenate(x#"cd", ".", x#"name");
	
	if OMSParsers#?s then
		-- We can parse it!
		OMSParsers#s
	else
		-- We cannot parse it -- leave as is.
		x
)

fromOpenMathOMA = x->(
	c := x.children;
	if class(c) =!= List then error(concatenate("Illegal OMA: children has type ", toString(class(c)), "."));
	if #c === 0 then error("Illegal OMA: has no children.");
	hd := fromOpenMath(c#0);

	if class(hd) === XMLnode then (
		-- We cannot parse it -- leave as is.
		print concatenate("WARNING -- Could not parse application of ", toString(hd));
		x
	) else
		-- We can parse it!
		hd(take(c, {1,#x}))
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
	else
		error(concatenate("Cannot handle XMLnode with tag ", t))
)

fromOpenMath Thing := x -> x;

