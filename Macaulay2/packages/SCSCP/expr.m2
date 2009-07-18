--------------------------------------------
--- Conveniently handling remote objects ---
--------------------------------------------

retrieve = method();
retrieve remoteObject := x -> ( value x.connection(OMA("scscp2", "retrieve", {toOpenMath x})) )

store = method();
store (SCSCPConnection, Thing) := (s,x) -> ( value s(toOpenMath x, "cookie") )

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
