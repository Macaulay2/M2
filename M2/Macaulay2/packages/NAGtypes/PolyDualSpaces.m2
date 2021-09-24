------------------------------------------------------
-- PolySpace and DualSpace
------------------------------------------------------
export { 
    "DualSpace", "BasePoint", "dualSpace", "PolySpace", "polySpace", "Reduced", "Gens", "Space",
    "reduceSpace",
    "addition", "isContained"
    }

-------------------------------------------
-- PolySpace
--   Basis: a row matrix of polynomials representing dual functionals
-- (Basis is expected to be linearly independent and reduced w.r.t. the monomial order)
PolySpace = new Type of MutableHashTable
globalAssignment PolySpace
polySpace = method(Options => {Reduced => false})
polySpace PolySpace := S -> new PolySpace from S
polySpace Matrix := o -> M -> (
    assert(numrows M == 1);
    new PolySpace from {Gens=>M,Reduced=>o.Reduced}
    )

-------------------------------------------
-- DualSpace 
--   Space: a PolySpace (closed under derivation) 
--   BasePoint: a point of localization
--   Tolerance: tolerance used to compute the DualSpace or associated to it; 0 in the exact setting
DualSpace = new Type of MutableHashTable
globalAssignment DualSpace
dualSpace = method()
dualSpace DualSpace := L -> new DualSpace from L
dualSpace (PolySpace, Point) := (S,p)-> (
    assert(numgens ring S.Gens == #coordinates p);
    new DualSpace from {Space=>S,BasePoint=>p}
    )
dualSpace (Matrix, Point) := (M,p)-> dualSpace(polySpace M,p)
-- what other constructors would we have?

gens PolySpace := o -> S -> S.Gens
gens DualSpace := o -> L -> gens L.Space

net PolySpace := S -> net gens S
net DualSpace := L -> net gens L

texMath PolySpace := S -> texMath gens S
texMath DualSpace := L -> texMath gens L

dim PolySpace := S -> numcols gens S
dim DualSpace := L -> numcols gens L

ring PolySpace := S -> ring gens S
ring DualSpace := L -> ring gens L

point DualSpace := L -> L.BasePoint

-- check PolySpace :=  L -> error "not implemented"
-- check DualSpace :=  L -> error "not implemented"

areEqual (PolySpace,PolySpace) := o -> (S,T) -> (
    n := dim addition(S,T,Tolerance=>o.Tolerance);
    n == dim S and n == dim T
    )
areEqual (DualSpace,DualSpace) := o -> (L,K) ->
    areEqual(L.BasePoint,K.BasePoint,Tolerance=>o.Tolerance) and areEqual(L.Space,K.Space,Tolerance=>o.Tolerance)
    
isContained = method(TypicalValue => Boolean, Options => {Tolerance=>1e-6})
isContained (PolySpace,PolySpace) := o -> (S,T) ->
    dim addition(S,T,Tolerance=>o.Tolerance) == dim T
isContained (DualSpace,DualSpace) := o -> (L,K) ->
    areEqual(L.BasePoint,K.BasePoint,Tolerance=>o.Tolerance) and isContained(L.Space,K.Space,Tolerance=>o.Tolerance)

intersection (PolySpace,PolySpace) := PolySpace => {Tolerance=>1e-6} >> o -> (S,T) -> (
    (mons,coefs) := coefficients (gens S|gens T);
    Scoefs := submatrix(coefs,(0..dim S-1));
    Tcoefs := submatrix'(coefs,(0..dim T-1));
    Sorth := numericalKernel(transpose Scoefs,o.Tolerance);
    Torth := numericalKernel(transpose Tcoefs,o.Tolerance);
    M := mons*numericalKernel(transpose (Sorth|Torth),o.Tolerance);
    polySpace M
    )

addition = method(TypicalValue => PolySpace, Options => {Tolerance=>1e-6})
addition (PolySpace,PolySpace) := o -> (S,T) -> (
    (mons,C) := coefficients (gens S | gens T);
    polySpace(mons*sub(numericalImage(C,o.Tolerance),ring S))
    )

random PolySpace := o -> S -> (
    F := ultimate(coefficientRing, ring S);
    ((gens S)*sub(random(F^(dim S),F^1), ring S))_(0,0)
    )


reduceSpace = method(Options => {Monomials => null,Tolerance=>1e-6})
reduceSpace PolySpace := o -> S -> (
    if dim S == 0 then return polySpace(gens S,Reduced=>true);
    (mons,coefs) := coefficients(gens S, Monomials => o.Monomials);
    M := mons*(colReduce(coefs,Reverse=>true,Tolerance=>o.Tolerance));
    polySpace(M,Reduced=>true)
    )
reduceSpace DualSpace := o -> L -> dualSpace(reduceSpace L.Space,L.BasePoint)

random DualSpace := o -> D -> random D.Space
random (ZZ,DualSpace) := o -> (d,D) -> random(d,D.Space)
random (ZZ,PolySpace) := o -> (d,S) -> (
    if not S.Reduced then S = reduceSpace S;
    Sd := polySpace sub(matrix{select(flatten entries gens S, q -> first degree q <= d)}, ring S);
    random Sd
    )

