 
-----------------------------------------------------------------------
-- POLYSYSTEM = {
--   NumberOfVariables => ZZ,
--   NumberOfPolys => ZZ,
--   PolyMap => Matrix, a column matrix over a polynomial ring (usually with complex coeffiecients)
--           or SLP;
--   Jacobian => Matrix or SLP, the jacobian of PolyMap
--   JacobianAndPolySystem => SLP, a circuit evaluating PolyMap and Jacobian (the two are not necessary then)
--   }
PolySystem.synonym = "polynomial system"
net PolySystem := p -> (
    if hasAnAttribute p then (
	if hasAttribute(p,PrintNet) then return getAttribute(p,PrintNet);
  	if hasAttribute(p,PrintNames) then return net getAttribute(p,PrintNames);
  	if hasAttribute(p,ReverseDictionary) then return toString getAttribute(p,ReverseDictionary);
  	);
     if p.?PolyMap then net p.PolyMap
     else if p.?NumberOfPolys and p.?NumberOfVariables 
     then net "a system of " | net p.NumberOfPolys | " polynomials in " | net p.NumberOfVariables | " variables" 
     else error "the polynomial system is corrupted"
    ) 
globalAssignment PolySystem

polySystem = method()
polySystem PolySystem := P -> new PolySystem from P
polySystem List := L -> (
    polySystem transpose matrix {L} 
    )
polySystem Matrix := M -> (
    assert(numcols M == 1);
    new PolySystem from {PolyMap=>M, NumberOfVariables=>numgens ring M, NumberOfPolys=>numrows M}
    )
polySystem Ideal := I -> polySystem transpose gens I

ring PolySystem := P -> ring P.PolyMap -- change this for SLP!!!
equations = method() -- returns list of equations
equations PolySystem := P -> flatten entries P.PolyMap -- change this for SLP!!!
ideal PolySystem := P -> ideal P.PolyMap -- change this for SLP!!!
toExternalString PolySystem := P -> "polySystem " | toExternalString equations P 

isHomogeneous PolySystem := P -> isHomogeneous ideal P.PolyMap -- change this for SLP!!!
XXXapply = method()
XXXapply(PolySystem,Function) := (P,f) -> polySystem apply(XXXtoList P, f) -- does not work for SLPs
substitute(PolySystem,Ring) := (P,R) -> polySystem sub(P.PolyMap, R) -- does not work for SLPs
XXXtoList = method()
XXXtoList PolySystem := P -> if P.?PolyMap then flatten entries P.PolyMap else error "polynomial system is not represented by a matrix"
homogenize (PolySystem,Ring,RingElement) := (P,R,h) -> polySystem homogenize(sub(P.PolyMap,R),h)
isSquare = method()
isSquare PolySystem := P -> P.NumberOfPolys == P.NumberOfVariables

toCCpolynomials = method()
toCCpolynomials (List,ZZ) := (F,prec) -> (
    R := CC_prec(monoid[gens commonRing F]);
    apply(F,f->sub(f,R)) 
    )    
toCCpolynomials (Matrix,InexactField) := (F,C) -> (
    R := C(monoid[gens ring F]);
    sub(F,R) 
    )    

-- evaluate = method()
evaluate (PolySystem,Point) := (P,p) -> evaluate(P, matrix p)
evaluate (Matrix,Point) := (M,p) -> evaluate(M, matrix p)
evaluate (PolySystem,Matrix) := (P,X) -> (
    if class P.PolyMap === Matrix 
    then evaluate(P.PolyMap,X)
    else error "evaluation not implemented for this type of PolyMap"
    )    
evaluate (Matrix,Matrix) := (M,X) ->  (
    C := coefficientRing ring M;
    -- work around a sub(CC,QQ) bug!!!
    if instance(ring X, InexactField) then 
	M = toCCpolynomials(M,C=ring X);   
    if numColumns X == 1 then X = transpose X;
    if numRows X == 1 then sub(M,sub(X,C))
    else error "expected a row or a column vector"
    )
evaluate (PolySystem,Point) := (P,p) -> evaluate(P,matrix p)

jacobian PolySystem := P -> (
    if P.?Jacobian then P.Jacobian
    else P.Jacobian = transpose jacobian(transpose P.PolyMap) -- TO DO: make "jacobian" work for SLPs
    )

