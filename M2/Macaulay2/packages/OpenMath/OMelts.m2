-- Convenience constructors of OpenMath elements
-- These are then used in base.m2.
-- OK OMA       (apply)
--    OMATP     (attribute pair)
--    OMATTR    (attributes)
--    OMB       (Binary data)
-- OK OMBIND    (Binding)
-- OK OMBVAR    (Binding variables)
-- OK OME       (Error)
-- OK OMF       (Float)
--    OMFOREIGN (Foreign XML)
-- OK OMI       (Integers)
-- OK OMOBJ     ("Object": the root node)
-- OK OMR       (References)
-- OK OMSTR     (String)
-- OK OMS       (Symbol)
-- OK OMV       (Variable)


--- Constructing OpenMath via XMLnode's ---
OMI = method()
OMI String := x -> new XMLnode from {symbol tag => "OMI", children => {x} }
OMI ZZ := x -> OMI(toString x)

OMSTR = method()
OMSTR String := x -> new XMLnode from {symbol tag => "OMSTR", children => {x} }

OMS = method()
OMS (String, String) := (x,y) -> new XMLnode from {symbol tag => "OMS", "cd" => x, "name" => y}

OMA = method()
OMA (XMLnode, List) := (s, l) -> (
	if #l > 0 and class(l#0) =!= XMLnode then l = apply(l, toOpenMath);
	new XMLnode from {symbol tag => "OMA", children => prepend(s, l) }
)
OMA (String, String, List) := (a,b,l) -> OMA(OMS(a,b), l)

OME = method()
OME (XMLnode, List) := (s, l) -> (
	if #l > 0 and class(l#0) =!= XMLnode then l = apply(l, toOpenMath);
	new XMLnode from {symbol tag => "OME", children => prepend(s, l) }
)
OME (String) := s -> OME(OMS("scscp1", "error_system_specific"), {OMSTR(s)});

OMV = method()
OMV (String) := x -> new XMLnode from {symbol tag => "OMV", "name" => x}

OMR = method()
OMR (String) := x -> new XMLnode from {symbol tag => "OMR", "href" => x}

OMOBJ = method()
OMOBJ XMLnode := x -> if x.tag === "OMOBJ" then x else new XMLnode from {symbol tag => "OMOBJ", children => {x}}

deOMOBJ = method()
deOMOBJ XMLnode := x -> if x.tag === "OMOBJ" then (x.children)#0 else x

OMF = method()
OMF String := x -> new XMLnode from {symbol tag => "OMF", "dec" => x}

OMBVAR = method()
OMBVAR List := x-> new XMLnode from {symbol tag => "OMBVAR", symbol children => apply(x, toOpenMath) };

OMBIND = method()
OMBIND (XMLnode, List, List) := (hd, vars, exprs) -> (
	vvars := OMBVAR(vars);
	
	new XMLnode from { 
		symbol tag => "OMBIND", 
		symbol children => prepend(hd, prepend(vvars, exprs))
	}
)
OMBIND (String, String, List, List) := (cd, name, vars, exprs) -> OMBIND(OMS(cd, name), vars, exprs);
OMBIND (String, String, List, XMLnode) := (cd, name, vars, expr) -> OMBIND(OMS(cd, name), vars, {expr});
OMBIND (XMLnode, List, XMLnode) := (hd, vars, expr) -> OMBIND(hd, vars, {expr});

OMATTR = method()
OMATTR (XMLnode, HashTable) := (child, attrs) -> (
	--construct the key,value,key,value, ... that will appear inside the OMATP
	attrsflat := {};
	for k in values(attrs) do (
		attrsflat = join(attrsflat, {k#0, k#1})
	);
	
	--clean out the attributes from the child
	clearOMAttr(child);

	--construct the OMATTR
	new XMLnode from {
		symbol tag => "OMATTR",
		symbol children => {
			new XMLnode from {
				symbol tag => "OMATP",
				symbol children => attrsflat
			},
			child
		} 
	}
)



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

