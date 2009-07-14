-- Convenience constructors of OpenMath elements

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



OMI = method()
OMI String := x -> new XMLnode from {symbol tag => "OMI", children => x }
OMI ZZ := x -> OMI(toString x)

OMSTR = method()
OMSTR String := x -> new XMLnode from {symbol tag => "OMSTR", children => x }

OMS = method()
OMS (String, String) := (x,y) -> new XMLnode from {symbol tag => "OMS", "cd" => x, "name" => y}

OMA = method()
OMA (HashTable, List) := (s, l) -> new XMLnode from {symbol tag => "OMA",
	children => prepend(s, l)
}
OMA (String, String, List) := (a,b,l) -> OMA(OMS(a,b), l)

OME = method()
OME (HashTable, List) := (s, l) -> new XMLnode from {symbol tag => "OME",
	children => prepend(s, l)
}
OME (String, String, List) := (a,b,l) -> OME(OMS(a,b), l)

OMV = method()
OMV (String) := x -> new XMLnode from {symbol tag => "OMV", "name" => x}

OMR = method()
OMR (String) := x -> new XMLnode from {symbol tag => "OMR", "href" => x}

OMOBJ = method()
OMOBJ HashTable := x -> new XMLnode from {symbol tag => "OMOBJ", children => x}

OMF = method()
OMF String := x -> new XMLnode from {symbol tag => "OMF", "dec" => x}

