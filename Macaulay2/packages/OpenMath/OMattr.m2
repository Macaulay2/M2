--- Manipulation of "attributes" of XMLnode's ---

protect OMattributes

setOMAttr = method()
setOMAttr (XMLnode, XMLnode, XMLnode) := (x,k,v) -> (
	if k.tag =!= "OMS" then error "setOMAttr: keys must be OMSymbols";
	if not x.?OMattributes then x.OMattributes = new MutableHashTable;
	(x.OMattributes)#(k#"cd"|"."|k#"name") = (k,v);
)
setOMAttr (XMLnode, MutableHashTable) := (x,t) -> (x.OMattributes = t);

hasOMAttr = method()
hasOMAttr (XMLnode, String, String) := (x, cd, name) -> (
	(x.?OMattributes) and ((x.OMattributes)#?(cd|"."|name));
)
hasOMAttr (XMLnode, XMLnode) := (x, k) -> (
	assert k.tag === "OMS";
	hasOMAttr(x, k#"cd", k#"name");
)


getOMAttr = method()
getOMAttr (XMLnode, String, String) := (x, cd, name) -> ((x.OMattributes)#(cd|"."|name))#1;
getOMAttr (XMLnode, XMLnode) := (x, k) -> (
	assert k.tag === "OMS";
	getOMAttr(x, k#"cd", k#"name")
)


clearOMAttr = method()
clearOMAttr (XMLnode) := x -> if x.?OMattributes then remove(x, OMattributes);

createOMATTRObj = method()
createOMATTRObj (XMLnode) := x -> (
	xnw := if not x.?OMattributes then x else OMATTR(x, x.OMattributes);
	if xnw.?children then
		xnw.children = apply(xnw.children, createOMATTRObj);
	xnw
);
createOMATTRObj (Thing) := x -> x;

