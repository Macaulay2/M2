-- Convenience constructors of OpenMath elements
-- These are then used in base.m2.

-- OK OMApply
--    OMBinary
--    OMBind
-- OK OMError
-- OK OMFloat
--    OMForeign
-- OK OMInteger
-- OK OMObject
-- OK OMReference
-- OK OMString
-- OK OMSymbol
-- OK OMVariable


--- Constructing OpenMath via XMLnode's ---
OMI = method()
OMI String := x -> new XMLnode from {symbol tag => "OMI", children => x }
OMI ZZ := x -> OMI(toString x)

OMSTR = method()
OMSTR String := x -> new XMLnode from {symbol tag => "OMSTR", children => x }

OMS = method()
OMS (String, String) := (x,y) -> new XMLnode from {symbol tag => "OMS", "cd" => x, "name" => y}

OMA = method()
OMA (XMLnode, List) := (s, l) -> new XMLnode from {symbol tag => "OMA",
	children => prepend(s, l)
}
OMA (String, String, List) := (a,b,l) -> OMA(OMS(a,b), l)

OME = method()
OME (XMLnode, List) := (s, l) -> new XMLnode from {symbol tag => "OME",
	children => prepend(s, l)
}
OME (String, String, List) := (a,b,l) -> OME(OMS(a,b), l)
OME (String) := s -> OME(OMS("scscp1", "error_system_specific"), {s})

OMV = method()
OMV (String) := x -> new XMLnode from {symbol tag => "OMV", "name" => x}

OMR = method()
OMR (String) := x -> new XMLnode from {symbol tag => "OMR", "href" => x}

OMOBJ = method()
OMOBJ XMLnode := x -> new XMLnode from {symbol tag => "OMOBJ", children => x}

OMF = method()
OMF String := x -> new XMLnode from {symbol tag => "OMF", "dec" => x}

OMBIND = method()
OMBIND (XMLnode, List, List) := (hd, vars, exprs) -> (
	vvars := new XMLnode from {
		symbol tag => "OMBVAR",
		symbol children => apply(vars, toOpenMath)
	};
	
	new XMLnode from { 
		symbol tag => "OMBIND", 
		symbol children => prepend(hd, prepend(vvars, exprs))
	}
)
OMBIND (String, String, List, List) := (cd, name, vars, exprs) -> OMBIND(OMS(cd, name), vars, exprs);
OMBIND (String, String, List, XMLnode) := (cd, name, vars, expr) -> OMBIND(OMS(cd, name), vars, {expr});
OMBIND (XMLnode, List, XMLnode) := (hd, vars, expr) -> OMBIND(hd, vars, {expr});


--- And some helper functions ---
isOMAOf = (x, cd, nm) -> (
	if not class(x) === XMLnode then return false; 
	if x.tag =!= "OMA" then return false;
	if #(x.children) === 0 then return false;
	if ((x.children)#0).tag =!= "OMS" then return false;
	if ((x.children)#0)#"cd" =!= cd then return false;
	if ((x.children)#0)#"name" =!= nm then return false;
	return true
)

