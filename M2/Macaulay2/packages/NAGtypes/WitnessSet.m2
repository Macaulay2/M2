-----------------------------------------------------------------------
-- WITNESS SET = {
--   Equations,            -- an ideal or a polynomial system or a list
--   Slice,                -- a matrix of coefficients of linear equations 
--                            (e.g., row [1,2,3] corresponds to x+2y+3=0)
--   Points,	           -- a list of points (in the format of the output of solveSystem/track) 
--   cache.IsIrreducible,        -- true, false, or null
--   [ProjectionDimension]   -- an integer n, the set describes a projection a variety to the first n coordinates
--   [SolutionSystem]        -- a square system built from Equations and Slice that Points satisfy
--   [LiftedSystem]          -- a regularization of SolutionSystem (in case Points are not regular)
--   }
-- caveat: we do not assume that #Equations = dim(Slice) 
--
-- PROJECTIVE WITNESS SET = { ... same as WITNESS SET ..., 
--     AffineChart         -- one-row matrix of coefficients of a the linear equation of the chart
--                            (e.g., [1,2,3] corresponds to x+2y+3=1)  
--     }
WitnessSet.synonym = "witness set"
texMath WitnessSet := x -> texMath net x
net WitnessSet := W -> (
    if hasAnAttribute W then (
	if hasAttribute(W,PrintNet) then return getAttribute(W,PrintNet);
  	if hasAttribute(W,PrintNames) then return net getAttribute(W,PrintNames);
  	if hasAttribute(W,ReverseDictionary) then return toString getAttribute(W,ReverseDictionary);
  	);
    if not W.cache.?IsIrreducible or W.cache.IsIrreducible===null
    then "[dim=" | net dim W |",deg="| net degree W | "]-*may be reducible*-" 
    else "(dim=" | net dim W |",deg="| net degree W | ")"
    ) 
globalAssignment WitnessSet

dim WitnessSet := W -> ( if class W.Slice === List then #W.Slice 
     else if class W.Slice === Matrix then numrows W.Slice 
     else error "ill-formed slice in WitnessSet" )
codim WitnessSet := {} >> o -> W -> numgens ring W - dim W
ring WitnessSet := W -> if member(class W.Equations, {Ideal,PolySystem}) then ring W.Equations else ring ideal equations W
degree WitnessSet := W -> #W.Points
ideal WitnessSet := W -> if class W.Equations === PolySystem then ideal W.Equations else W.Equations
declareIrreducible = method()
declareIrreducible WitnessSet := W -> W.cache.IsIrreducible = true 

witnessSet = method(TypicalValue=>WitnessSet)
witnessSet (PolySystem,Matrix,List) := (F,S,P) -> 
  new WitnessSet from { Equations => F, Slice => S, Points => VerticalList P, cache => new CacheTable from {IsIrreducible=>null}}
witnessSet (Ideal,Ideal,List) := (I,S,P) -> witnessSet (I, sliceEquationsToMatrix S, P)
witnessSet (Ideal,Matrix,List) := (I,S,P) -> witnessSet (polySystem I, S, P)
witnessSet (PolySystem,PolySystem,List) := (F,S,P) -> witnessSet (F, sliceEquationsToMatrix ideal equations S, P)

-- points = method() -- strips all info except coordinates, returns a doubly-nested list
-- points WitnessSet := W -> apply(W.Points, coordinates) -- return Points (not just coordinates)???
points WitnessSet := W -> W.Points

equations WitnessSet := (W) -> if class W.Equations === PolySystem then XXXtoList W.Equations else 
if class W.Equations === Ideal then (W.Equations)_* else 
W.Equations

slice = method() -- returns linear equations for the slice (in both cases)   
slice WitnessSet := (W) -> ( 
    --if class W.Slice === List then W.Slice
    --else 
    if class W.Slice === Matrix then (
	if class W === ProjectiveWitnessSet 
	then projectiveSliceEquations(W.Slice, ring W)
	else sliceEquations(W.Slice, ring W)
	)
    else error "ill-formed slice in WitnessSet" )

sliceEquations = method(TypicalValue=>List) -- make slicing plane equations 
sliceEquations (Matrix,Ring) := (S,R) -> 
  apply(numrows S, i->(sub(S^{i},R) * transpose(vars R | matrix{{1_R}}))_(0,0)) 

projectiveSliceEquations = method(TypicalValue=>List) -- make slicing plane equations 
projectiveSliceEquations (Matrix,Ring) := (S,R) -> 
  apply(numrows S, i->(sub(S^{i},R) * transpose vars R)_(0,0)) 

sliceEquationsToMatrix = method()
sliceEquationsToMatrix Ideal := I -> (
    R := ring I;
    if numgens I > 0 then 
    matrix apply(I_*, f -> apply(gens R, x->coefficient(x,f))|{coefficient(1_R,f)})
    else 
    map(R^0,R^(numgens R + 1),0)
    )  

projectiveWitnessSet = method(TypicalValue=>ProjectiveWitnessSet)
projectiveWitnessSet (Ideal,Matrix,Matrix,List) := (I,C,S,P) -> 
  new WitnessSet from { 
      Equations => I, 
      AffineChart => C, 
      Slice => S, 
      Points => VerticalList P, 
      cache => new CacheTable from {IsIrreducible=>null}}

TEST /// --WitnessSet
CC[x,y,z]
I = ideal {z-x*y, x^2-y}
S = ideal (z-1)
P = apply(3, i->(
	x := exp(2*i*pi*ii/3);
	point {{x,x^2,x^3}}
	))
W = witnessSet(I,S,P)
M = matrix{{0,0,1,-1}}
W = witnessSet(I,M,P)
points W
equations W
slice W
assert (dim W == 1 and degree W ==3)
///

-**********************************************************************
NumericalVariety = {
     0 => list of (irreducible) witness sets
     1 => list of (irreducible) witness sets
     ...
     dim => list of (irreducible) witness sets
     ...
     }

SERVICE FUNCTIONS:
  dim
  degree
  isReduced
  NumericalVariety union NumericalVariety (binary)
  
*-
NumericalVariety.synonym = "numerical variety"
net NumericalVariety := V -> (
    if hasAnAttribute V then (
	if hasAttribute(V,PrintNet) then return getAttribute(V,PrintNet);
  	if hasAttribute(V,PrintNames) then return net getAttribute(V,PrintNames);
  	if hasAttribute(V,ReverseDictionary) then return toString getAttribute(V,ReverseDictionary);
  	);
    out := net ofClass class V | " with components in";
    scan(keys V, k->if class k === ZZ then (
	    row := "dim "|net k|": ";
	    scan(V#k, W->row = row|" "|net W);
	    out = out || row;
	    ));
    out
    )
globalAssignment NumericalVariety

dim NumericalVariety := V -> max select(keys V, k->class k === ZZ)
degree NumericalVariety := V -> (
     d := dim V;
     sum(keys V, k->if k =!= d then 0 else sum(V#k,degree))
     )
components NumericalVariety := V -> flatten apply(select(keys V, i->class i === ZZ), i->V#i)
components (NumericalVariety,ZZ) := (V,d) -> V#d
components (NumericalVariety,ZZ,ZZ) := (V,a,b) -> (
    a = max(0,a);
    b = min(b,dim V);
    flatten apply(select(keys V, d->a<=d and d<=b), d->V#d)  
    )
components (NumericalVariety,ZZ,InfiniteNumber) := (V,a,b) -> components(V,a,min(b,dim V))
    
numericalVariety = method(TypicalValue=>NumericalVariety)
numericalVariety List := Ws -> if #Ws==0 then new NumericalVariety else (
     T := class first Ws;
     if not ancestor(WitnessSet,T) then error "a list of WitnessSet-s expected";
     V := new NumericalVariety;
     scan(Ws, W->(
	     if class W =!= T then error "a list of witness sets of same type expected";
	     d := dim W;
	     if V#?d then V#d = V#d | {W} else V#d = {W};
	     ));     
     check V;
     V
     )
numericalAffineSpace = method()
numericalAffineSpace PolynomialRing := R -> (
    n := numgens R;
    C := coefficientRing R; 
    A := random(C^n,C^n);
    b := random(C^n,C^1);
    W := witnessSet(ideal R, A|(-b), {point entries transpose solve(A,b)});
    declareIrreducible W;
    numericalVariety {W}
    )
-- ProjectiveNumericalVariety is not a type!
projectiveNumericalVariety = method( -* TypicalValue=>ProjectiveNumericalVariety *- )
projectiveNumericalVariety List := Ws -> new ProjectiveNumericalVariety from numericalVariety Ws

check NumericalVariety := o-> V -> (
    if any(keys V, k->(class k =!= ZZ or k<0)) 
    then error "the keys of a NumericalVariety should be nonnegative integers";
    scan(keys V, k->if class k === ZZ then scan(V#k, W->(
		if dim W != k then 
		error "dimension of a witness set does not match the key in NumericalVariety";
		)));
    )
