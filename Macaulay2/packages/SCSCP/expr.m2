--------------------------------------------
--- Conveniently handling remote objects ---
--------------------------------------------

--"retrieve" is not really needed: it is more or less equivalent to just giving the reference
--with option_return_object. I suppose.
--  retrieve = method();
--  retrieve remoteObject := x -> ( value x.connection(OMA("scscp2", "retrieve", {toOpenMath x})) )

Thing ==> SCSCPConnection := (x,s) -> ( value s(toOpenMath x, "object") )
Thing ===> SCSCPConnection := (x,s) -> ( value s(toOpenMath x, "cookie") )

--^^ that makes this work:
-- i3 : a = 7 ===> gap
-- i4 : b = 7 ===> gap
-- i5 : a+b
-- i6 : a+b ==> gap




--------------------------
-- Building Expressions --
--------------------------
openMath remoteObject := x -> new XMLnode from x;

copyConnection := method();
copyConnection (remoteObject, remoteObject, XMLnode) := (a,b,o) -> (
	if a.connection === b.connection then (
		r := new remoteObject from o;
		r.connection = a.connection;
		r
	) else
		o
)
copyConnection (remoteObject, XMLnode) := (a,o) -> (r := new remoteObject from o; r.connection = a.connection; r)

remoteObject + remoteObject := (a,b) -> copyConnection(a,b, (lookup(symbol +, XMLnode, XMLnode))(a,b))
remoteObject + Thing        := (a,b) -> copyConnection(a,   (lookup(symbol +, XMLnode, XMLnode))(a,b))
Thing        + remoteObject := (a,b) -> copyConnection(b,   (lookup(symbol +, XMLnode, XMLnode))(a,b))

remoteObject - remoteObject := (a,b) -> copyConnection(a,b, (lookup(symbol -, XMLnode, XMLnode))(a,b))
remoteObject - Thing        := (a,b) -> copyConnection(a,   (lookup(symbol -, XMLnode, XMLnode))(a,b))
Thing        - remoteObject := (a,b) -> copyConnection(b,   (lookup(symbol -, XMLnode, XMLnode))(a,b))

remoteObject / remoteObject := (a,b) -> copyConnection(a,b, (lookup(symbol /, XMLnode, XMLnode))(a,b))
remoteObject / Thing        := (a,b) -> copyConnection(a,   (lookup(symbol /, XMLnode, XMLnode))(a,b))
Thing        / remoteObject := (a,b) -> copyConnection(b,   (lookup(symbol /, XMLnode, XMLnode))(a,b))

remoteObject * remoteObject := (a,b) -> copyConnection(a,b, (lookup(symbol *, XMLnode, XMLnode))(a,b))
remoteObject * Thing        := (a,b) -> copyConnection(a,   (lookup(symbol *, XMLnode, XMLnode))(a,b))
Thing        * remoteObject := (a,b) -> copyConnection(b,   (lookup(symbol *, XMLnode, XMLnode))(a,b))

remoteObject == remoteObject := (a,b) -> copyConnection(a,b, (lookup(symbol ==, XMLnode, XMLnode))(a,b))
remoteObject == Thing        := (a,b) -> copyConnection(a,   (lookup(symbol ==, XMLnode, XMLnode))(a,b))
Thing        == remoteObject := (a,b) -> copyConnection(b,   (lookup(symbol ==, XMLnode, XMLnode))(a,b))

remoteObject and remoteObject := (a,b) -> copyConnection(a,b, (lookup(symbol and, XMLnode, XMLnode))(a,b))
remoteObject and Thing        := (a,b) -> copyConnection(a,   (lookup(symbol and, XMLnode, XMLnode))(a,b))
Thing        and remoteObject := (a,b) -> copyConnection(b,   (lookup(symbol and, XMLnode, XMLnode))(a,b))

remoteObject or remoteObject := (a,b) -> copyConnection(a,b, (lookup(symbol or, XMLnode, XMLnode))(a,b))
remoteObject or Thing        := (a,b) -> copyConnection(a,   (lookup(symbol or, XMLnode, XMLnode))(a,b))
Thing        or remoteObject := (a,b) -> copyConnection(b,   (lookup(symbol or, XMLnode, XMLnode))(a,b))

size remoteObject := x -> copyConnection(x, (lookup(size, XMLnode))(x))
