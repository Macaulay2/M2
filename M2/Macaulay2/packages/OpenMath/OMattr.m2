--- Manipulation of "attributes" of XMLnode's ---
setOMAttr = method()
setOMAttr (XMLnode, XMLnode, XMLnode) := (x,k,v) -> (
	if not x.?OMattributes then (
		xnw := new MutableHashTable from x;
		xnw.OMattributes = new MutableHashTable;
		(xnw.OMattributes)#k = v;
		new XMLnode from (new HashTable from xnw)
	) else (
		(x.OMattributes)#k = v;
		x
	)
)
setOMAttr (XMLnode, MutableHashTable) := (x,t) -> (
	xnw := new MutableHashTable from x;
	xnw.OMattributes = t;
	new XMLnode from (new HashTable from xnw)
)

clearOMAttr = method()
clearOMAttr (XMLnode) := x -> (
	if not x.?OMattributes then (
		x
	) else (
		xnw := new MutableHashTable from x;
		remove(xnw, OMattributes);
		new XMLnode from (new HashTable from xnw)
	)
)

--Don't think I actually need these
-- hasOMAttr = method()
-- hasOMAttr (XMLnode, XMLnode) := (x, k) -> (
-- 	(x.?OMattributes) and ((x.OMattributes)#?k);
-- )
-- hasOMAttr (XMLnode, String, String) := (x, cd, name) -> hasOMAttr(x, OMS(cd, name));
-- 
-- getOMAttr = method()
-- getOMAttr (XMLnode, XMLnode) := (x, k) -> (
-- 	((x.OMattributes)#?k);
-- )
-- getOMAttr (XMLnode, String, String) := (x, cd, name) -> getOMAttr(x, OMS(cd, name));
