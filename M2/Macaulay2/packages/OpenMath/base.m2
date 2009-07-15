--- To OpenMath ---
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


-- From OpenMath ---
-- OK OMApply
--    OMBinary
--    OMBind
--    OMError
-- OK OMFloat
--    OMForeign
-- OK OMInteger
-- OK OMObject
--    OMReference
-- OK OMString
-- OK OMSymbol
-- OK OMVariable

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

	if class(hd) === XMLnode then (
		-- We cannot parse it -- leave as is.
		print concatenate("WARNING -- Could not parse application of ", toString(hd));
		x
	) else
		-- We can parse it!
		hd(take(c, {1,#c-1}))
)
fromOpenMathOMV = x->(
	if not x#?"name" then return OME("Illegal OMV: no \"name\"");
	x
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
	else (
		print concatenate("WARNING -- Could not parse XMLnode with tag ", t);
		x
	)
)

fromOpenMath Thing := x -> x;

