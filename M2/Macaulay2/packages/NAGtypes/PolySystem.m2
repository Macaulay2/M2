-----------------------------------------------------------------------
-- System is an ABSTRACT TYPE

parameters = method()
numParameters = method()
numVariables = method()
numFunctions = method()
evaluateJacobian = method()

numParameters System :=  
numVariables System := -- dimension of domain
numFunctions System := -- dimension of codomain
  S -> error "not implemented"   
evaluateJacobian(System, Matrix, Matrix) := 
evaluate (System, Matrix, Matrix) := Matrix => (S,p,x) -> error "not implemented"  
-- specialize(System, Matrix) := System => (S,p) -> error "not implemented" 
-- evaluation for systems with 0 parameters
evaluate (System, Matrix) := Matrix => (S,x) -> if numParameters S != 0 
    then error "this is a system with parameters: use, e.g., evaluate(S,p,x)" else 
    evaluate(S, matrix{{}}, matrix x) 
evaluateJacobian (System, Matrix) := Matrix => (S,x) -> if numParameters S != 0 
    then error "this is a system with parameters: use, e.g., evaluate(S,p,x)" else 
    evaluateJacobian(S, matrix{{}}, matrix x) 
-- convenience functions for points
evaluateJacobian(System, AbstractPoint, AbstractPoint) := Matrix => 
  (S,p,x) -> evaluateJacobian(S, matrix p, matrix x)
evaluate (System, AbstractPoint, AbstractPoint) := Matrix => 
  (S,p,x) -> evaluate(S, matrix p, matrix x)
-- specialize(System, AbstractPoint) := System => 
--  (S,p) -> specialize(S, matrix p)
evaluateJacobian(System, AbstractPoint) := Matrix =>
  (S,x) -> evaluateJacobian(S, matrix x) 
evaluate (System, AbstractPoint) := Matrix => 
  (S,x) -> evaluate(S, matrix x)   

-----------------------------------------------------------------------
-- PolySystem = {
--   NumberOfVariables => ZZ,
--   NumberOfPolys => ZZ,
--   PolyMap => Matrix, a column matrix over a polynomial ring (usually with complex coefficients),
--   Jacobian => Matrix, the jacobian of PolyMap
--   }
PolySystem.synonym = "polynomial system"
texMath PolySystem := x -> texMath net x
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
    if not instance(commonRing L,PolynomialRing) then error "expected a list of polynomials";
    polySystem transpose matrix {L} 
    )
polySystem Matrix := M -> (
    if not instance(ring M,PolynomialRing) then error "expected a matrix of polynomials";    
    if numcols M != 1 then error "expected a matrix with 1 column";
    new PolySystem from {PolyMap=>M, NumberOfVariables=>numgens ring M, NumberOfPolys=>numrows M}
    )
polySystem Ideal := I -> polySystem transpose gens I

numVariables PolySystem := P -> P.NumberOfVariables
numFunctions PolySystem := P -> P.NumberOfPolys
numParameters PolySystem := P -> (
    C := coefficientRing ring P;
    if instance(C,PolynomialRing) then numgens C else 0
    )
ring PolySystem := P -> ring P.PolyMap 
equations = method() -- returns list of equations
equations PolySystem := P -> flatten entries P.PolyMap -- change this for SLP!!!
parameters PolySystem := P -> (
    C := coefficientRing ring P;
    if instance(C,PolynomialRing) then gens C else {}
    )
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

evaluate (Matrix,AbstractPoint) := (M,p) -> evaluate(M, matrix p)
evaluate (PolySystem,Matrix) := (P,X) -> evaluate(P.PolyMap,X)
evaluate (Matrix,Matrix) := (M,X) ->  (
    C := coefficientRing ring M;
    RX := ring X;
    commonField := commonRing{1_C, 1_RX};--try promote(1_C, RX) then RX else try promote(1_RX, C) then C else error "common field not found";
    -- work around a sub(CC,QQ) bug!!!
    if instance(ring X, InexactField) then 
	M = toCCpolynomials(M,C=commonField);   
    if numColumns X == 1 then X = transpose X;
    if numRows X == 1 then sub(M,sub(X,C))
    else error "expected a row or a column vector"
    )

jacobian PolySystem := P -> (
    if P.?Jacobian then P.Jacobian
    else P.Jacobian = transpose jacobian(transpose P.PolyMap) -- TO DO: make "jacobian" work for SLPs
    )
evaluateJacobian (PolySystem,AbstractPoint) := (P,p) -> sub(jacobian P,sub(matrix p,coefficientRing ring P))
TEST /// 
CC[x,y]
S = polySystem {x^2+y^2-6, 2*x^2-y}
p = point({{1.0_CC,2.3_CC}});
assert(numVariables S == 2)
assert(numFunctions S == 2)
evaluate(S,p)
evaluateJacobian(S,p)
///
