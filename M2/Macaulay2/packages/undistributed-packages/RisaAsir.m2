newPackage(
        "RisaAsir",
        Version => "0.1", 
        Date => "7 July 2015",
        Authors => {{Name => "", 
                  Email => "", 
                  HomePage => ""}},
        Headline => "an interface to the Risa/Asir website and system",
        PackageExports => {"Dmodules"},
        DebuggingMode => false
        )

export {
    "toAsir",
    "asirCall"
}

toAsir = method()
toAsir Number := (n) -> toExternalString n
toAsir RingElement := (f) -> toExternalString f
toAsir Ideal := (I) -> toAsir I_*
toAsir BasicList := (L) -> (
    inside := L/toAsir//(f -> between(",",f))//concatenate;
    "[" | inside | "]"
    )
asirCall = args -> (
    a1 := drop(args,1);
    inside := a1/toAsir//(f -> between(",",f))//concatenate;    
    args#0 | "(" | inside | ")"
    )
fromAsir = method()
fromAsir String := (str) -> (
    -- We expect the result to be:
    --  (a) polynomial
    --  (b) list of polynomials
    --  (c) list of list of such things, etc.
    value str
    )
Dvars = method()
Dvars Ring := (D) -> (
    createDpairs D;
    D.dpairVars#0
    )
Ddiffs = method()
Ddiffs Ring := (D) -> (
    createDpairs D;
    D.dpairVars#1
    )

--bFunction = method()
bFunction(Ideal, List) := opts -> (I, wt) -> (
    args := toAsir I;
    cmd := asirCall("generic_bfct", I, Dvars ring I, Ddiffs ring I, wt);
    print cmd;
    url := "http://asir2.math.kobe-u.ac.jp/cgi-bin/cgi-generic_bfct.sh";
    val := getWWW(url, "oxMessageBody="|cmd);
    -- now 
    val
    )

beginDocumentation()

doc ///
Key
  RisaAsir
Headline
  an interface to the Risa/Asir website and system
Description
  Text
  Example
Caveat
SeeAlso
///

end
restart
loadPackage "Dmodules"
debug loadPackage  "RisaAsir"
R = QQ[x,y,z]
R = QQ[x,y]
W = makeWeylAlgebra R
I = ideal(y*dy-2*z*dz,2*y*z*dx+3*x^2*dy,y^2*dx+3*x^2*dz,3*x^2*dy^2+4*z^2*dx*dz+2*z*dx)

I = ideal (x*dx+y*dy-3,
        x^2*dx^2+x*dx + y^2*dy^2+y*dy - 4)
Dvars W
Ddiffs W
F = x^3-y^2*z-4*x*z
I = PolyAnn F
I = AnnFs F
bFunction(I,{1,1,1})
isHolonomic I
globalBFunction F
viewHelp Dmodules
R = QQ[a..d]
I = ideal((a*b-c-d)^2, (a-1)*(b-1), c^3-(c+d)^2+1)
bFunction(I, {2,3})
toAsir {I,I,I}
toAsir [I,I,I]
toAsir gens R

doc ///
Key
Headline
Usage
Inputs
Outputs
Consequences
Description
  Text
  Example
  Code
  Pre
Caveat
SeeAlso
///

TEST ///
-- test code and assertions here
-- may have as many TEST sections as needed
///

