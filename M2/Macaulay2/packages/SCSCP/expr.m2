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
remoteObject + remoteObject := (a,b) -> ( value a.connection((toOpenMath a) + (toOpenMath b), "cookie") )
remoteObject + Thing        := (a,b) -> ( value a.connection((toOpenMath a) + (toOpenMath b), "cookie") )
Thing        + remoteObject := (a,b) -> ( value a.connection((toOpenMath a) + (toOpenMath b), "cookie") )

remoteObject - remoteObject := (a,b) -> ( value a.connection((toOpenMath a) - (toOpenMath b), "cookie") )
remoteObject - Thing        := (a,b) -> ( value a.connection((toOpenMath a) - (toOpenMath b), "cookie") )
Thing        - remoteObject := (a,b) -> ( value a.connection((toOpenMath a) - (toOpenMath b), "cookie") )

remoteObject * remoteObject := (a,b) -> ( value a.connection((toOpenMath a) * (toOpenMath b), "cookie") )
remoteObject * Thing        := (a,b) -> ( value a.connection((toOpenMath a) * (toOpenMath b), "cookie") )
Thing        * remoteObject := (a,b) -> ( value a.connection((toOpenMath a) * (toOpenMath b), "cookie") )

remoteObject / remoteObject := (a,b) -> ( value a.connection((toOpenMath a) / (toOpenMath b), "cookie") )
remoteObject / Thing        := (a,b) -> ( value a.connection((toOpenMath a) / (toOpenMath b), "cookie") )
Thing        / remoteObject := (a,b) -> ( value a.connection((toOpenMath a) / (toOpenMath b), "cookie") )

remoteObject ^ remoteObject := (a,b) -> ( value a.connection((toOpenMath a) ^ (toOpenMath b), "cookie") )
remoteObject ^ Thing        := (a,b) -> ( value a.connection((toOpenMath a) ^ (toOpenMath b), "cookie") )
Thing        ^ remoteObject := (a,b) -> ( value a.connection((toOpenMath a) ^ (toOpenMath b), "cookie") )

remoteObject == remoteObject := (a,b) -> ( value a.connection((toOpenMath a) == (toOpenMath b), "cookie") )
remoteObject == Thing        := (a,b) -> ( value a.connection((toOpenMath a) == (toOpenMath b), "cookie") )
Thing        == remoteObject := (a,b) -> ( value a.connection((toOpenMath a) == (toOpenMath b), "cookie") )

remoteObject and remoteObject := (a,b) -> ( value a.connection((toOpenMath a) and (toOpenMath b), "cookie") )
remoteObject and Thing        := (a,b) -> ( value a.connection((toOpenMath a) and (toOpenMath b), "cookie") )
Thing        and remoteObject := (a,b) -> ( value a.connection((toOpenMath a) and (toOpenMath b), "cookie") )

remoteObject or remoteObject := (a,b) -> ( value a.connection((toOpenMath a) or (toOpenMath b), "cookie") )
remoteObject or Thing        := (a,b) -> ( value a.connection((toOpenMath a) or (toOpenMath b), "cookie") )
Thing        or remoteObject := (a,b) -> ( value a.connection((toOpenMath a) or (toOpenMath b), "cookie") )

size remoteObject := x -> ( assert x.?connection; value x.connection(size toOpenMath x) )
