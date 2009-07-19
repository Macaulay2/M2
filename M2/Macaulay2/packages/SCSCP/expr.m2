--------------------------------------------
--- Conveniently handling remote objects ---
--------------------------------------------

--"retrieve" is not really needed: it is more or less equivalent to just giving the reference
--with option_return_object. I suppose.
--  retrieve = method();
--  retrieve RemoteObject := x -> ( value x.connection(OMA("scscp2", "retrieve", {toOpenMath x})) )

Thing ==> SCSCPConnection := (x,s) ->  ( value s(toOpenMath x, "object") )
Thing ===> SCSCPConnection := (x,s) -> ( value s(toOpenMath x, "cookie") )
                                         
SCSCPConnection <== Thing := (s,x) ->  ( value s(toOpenMath x, "object") )
SCSCPConnection <=== Thing := (s,x) -> ( value s(toOpenMath x, "cookie") )

<== RemoteObject := r -> (r.connection <== r)
<=== RemoteObject := r -> (r.connection <=== r)


--------------------------
-- Building Expressions --
--------------------------
openMath RemoteObject := x -> new XMLnode from x;

copyConnection := method();
copyConnection (RemoteObject, RemoteObject, XMLnode) := (a,b,o) -> (
	if a.connection === b.connection then (
		r := new RemoteObject from o;
		r.connection = a.connection;
		r
	) else
		o
)
copyConnection (RemoteObject, XMLnode) := (a,o) -> (r := new RemoteObject from o; r.connection = a.connection; r)

RemoteObject + RemoteObject := (a,b) -> copyConnection(a,b, (lookup(symbol +, XMLnode, XMLnode))(a,b))
RemoteObject + Thing        := (a,b) -> copyConnection(a,   (lookup(symbol +, XMLnode, XMLnode))(a,b))
Thing        + RemoteObject := (a,b) -> copyConnection(b,   (lookup(symbol +, XMLnode, XMLnode))(a,b))

RemoteObject - RemoteObject := (a,b) -> copyConnection(a,b, (lookup(symbol -, XMLnode, XMLnode))(a,b))
RemoteObject - Thing        := (a,b) -> copyConnection(a,   (lookup(symbol -, XMLnode, XMLnode))(a,b))
Thing        - RemoteObject := (a,b) -> copyConnection(b,   (lookup(symbol -, XMLnode, XMLnode))(a,b))

RemoteObject / RemoteObject := (a,b) -> copyConnection(a,b, (lookup(symbol /, XMLnode, XMLnode))(a,b))
RemoteObject / Thing        := (a,b) -> copyConnection(a,   (lookup(symbol /, XMLnode, XMLnode))(a,b))
Thing        / RemoteObject := (a,b) -> copyConnection(b,   (lookup(symbol /, XMLnode, XMLnode))(a,b))

RemoteObject * RemoteObject := (a,b) -> copyConnection(a,b, (lookup(symbol *, XMLnode, XMLnode))(a,b))
RemoteObject * Thing        := (a,b) -> copyConnection(a,   (lookup(symbol *, XMLnode, XMLnode))(a,b))
Thing        * RemoteObject := (a,b) -> copyConnection(b,   (lookup(symbol *, XMLnode, XMLnode))(a,b))

RemoteObject == RemoteObject := (a,b) -> copyConnection(a,b, (lookup(symbol ==, XMLnode, XMLnode))(a,b))
RemoteObject == Thing        := (a,b) -> copyConnection(a,   (lookup(symbol ==, XMLnode, XMLnode))(a,b))
Thing        == RemoteObject := (a,b) -> copyConnection(b,   (lookup(symbol ==, XMLnode, XMLnode))(a,b))

RemoteObject and RemoteObject := (a,b) -> copyConnection(a,b, (lookup(symbol and, XMLnode, XMLnode))(a,b))
RemoteObject and Thing        := (a,b) -> copyConnection(a,   (lookup(symbol and, XMLnode, XMLnode))(a,b))
Thing        and RemoteObject := (a,b) -> copyConnection(b,   (lookup(symbol and, XMLnode, XMLnode))(a,b))

RemoteObject or RemoteObject := (a,b) -> copyConnection(a,b, (lookup(symbol or, XMLnode, XMLnode))(a,b))
RemoteObject or Thing        := (a,b) -> copyConnection(a,   (lookup(symbol or, XMLnode, XMLnode))(a,b))
Thing        or RemoteObject := (a,b) -> copyConnection(b,   (lookup(symbol or, XMLnode, XMLnode))(a,b))

size RemoteObject := x -> copyConnection(x, (lookup(size, XMLnode))(x))
