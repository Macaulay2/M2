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

remoteObject + remoteObject := (a,b) -> copyConnection(a,b, (toOpenMath a) + (toOpenMath b))
remoteObject + Thing        := (a,b) -> copyConnection(a,   (toOpenMath a) + (toOpenMath b))
Thing        + remoteObject := (a,b) -> copyConnection(b,   (toOpenMath a) + (toOpenMath b))

remoteObject - remoteObject := (a,b) -> copyConnection(a,b, (toOpenMath a) - (toOpenMath b))
remoteObject - Thing        := (a,b) -> copyConnection(a,   (toOpenMath a) - (toOpenMath b))
Thing        - remoteObject := (a,b) -> copyConnection(b,   (toOpenMath a) - (toOpenMath b))

remoteObject / remoteObject := (a,b) -> copyConnection(a,b, (toOpenMath a) / (toOpenMath b))
remoteObject / Thing        := (a,b) -> copyConnection(a,   (toOpenMath a) / (toOpenMath b))
Thing        / remoteObject := (a,b) -> copyConnection(b,   (toOpenMath a) / (toOpenMath b))

remoteObject * remoteObject := (a,b) -> copyConnection(a,b, (toOpenMath a) * (toOpenMath b))
remoteObject * Thing        := (a,b) -> copyConnection(a,   (toOpenMath a) * (toOpenMath b))
Thing        * remoteObject := (a,b) -> copyConnection(b,   (toOpenMath a) * (toOpenMath b))

remoteObject == remoteObject := (a,b) -> copyConnection(a,b, (toOpenMath a) == (toOpenMath b))
remoteObject == Thing        := (a,b) -> copyConnection(a,   (toOpenMath a) == (toOpenMath b))
Thing        == remoteObject := (a,b) -> copyConnection(b,   (toOpenMath a) == (toOpenMath b))

remoteObject and remoteObject := (a,b) -> copyConnection(a,b, (toOpenMath a) and (toOpenMath b))
remoteObject and Thing        := (a,b) -> copyConnection(a,   (toOpenMath a) and (toOpenMath b))
Thing        and remoteObject := (a,b) -> copyConnection(b,   (toOpenMath a) and (toOpenMath b))

remoteObject or remoteObject := (a,b) -> copyConnection(a,b, (toOpenMath a) or (toOpenMath b))
remoteObject or Thing        := (a,b) -> copyConnection(a,   (toOpenMath a) or (toOpenMath b))
Thing        or remoteObject := (a,b) -> copyConnection(b,   (toOpenMath a) or (toOpenMath b))

size remoteObject := x -> copyConnection(x, size toOpenMath x) 
