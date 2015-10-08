------------------------------------------------------
-- operations for PolySpace and DualSpace
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
------------------------------------------------------
export { "addition", "intersection", "colon", "isContained" }

------------------------------------------------

check DualSpace :=  L -> error "not implemented"

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

intersection = method(TypicalValue => PolySpace, Options => {Tolerance=>1e-6})
intersection (PolySpace,PolySpace) := o -> (S,T) -> (
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

colon = method(TypicalValue => DualSpace, Options => {Tolerance=>1e-6})
colon (DualSpace, RingElement) := o-> (L,g) -> (
    (gmons,gcoefs) := coefficients g;
    (Lmons,Lcoefs) := coefficients gens L;
    M := matrix apply(flatten entries gmons, gm->(
	    apply(flatten entries Lmons, Lm->(
		    d := diff(gm,Lm);
		    if d == 0 then d else leadMonomial d
		    ))
	    ));
    if numcols M == 0 then M = map((ring L)^1,(ring L)^0,0);
    M = (transpose gcoefs)*M*Lcoefs;
    (Mmons,Mcoefs) := coefficients M;
    M = Mmons*sub(numericalImage(Mcoefs,o.Tolerance),ring Mmons);
    dualSpace(polySpace M, L.BasePoint)
    )
colon (DualSpace, Ideal) := (L,J) -> error "not implemented"

random PolySpace := o -> S -> (
    F := ultimate(coefficientRing, ring S);
    ((gens S)*sub(random(F^(dim S),F^1), ring S))_(0,0)
    )
random (ZZ,PolySpace) := o -> (d,S) -> (
    if not S.Reduced then S = reduceSpace S;
    Sd := polySpace sub(matrix{select(flatten entries gens S, q -> first degree q <= d)}, ring S);
    random Sd
    )
random DualSpace := o -> D -> random D.Space
random (ZZ,DualSpace) := o -> (d,D) -> random(d,D.Space)

interpolatedIdeal = method()
interpolatedIdeal DualSpace := L -> error "not implemented"
interpolatedIdeal List := LL -> error "not implemented"

beginDocumentation()

doc ///
     Key
          addition
	  (addition,PolySpace,PolySpace)
	  [addition,Tolerance]
     Headline
          Union of polynomial spaces
     Usage
          S = addition(T, U)
     Inputs
	  T:PolySpace
	  U:PolySpace
     Outputs
          S:PolySpace
     Description
          Text
	       Finds the union of two polynomial spaces.
///

doc ///
     Key
          intersection
	  (intersection,PolySpace,PolySpace)
	  [intersection,Tolerance]
     Headline
          Intersection of polynomial spaces
     Usage
          S = intersection(T, U)
     Inputs
	  T:PolySpace
	  U:PolySpace
     Outputs
          S:PolySpace
     Description
          Text
	       Finds the intersection of two polynomial spaces.
///

doc ///
     Key
          isContained
	  (isContained,PolySpace,PolySpace)
	  (isContained,DualSpace,DualSpace)
	  [isContained,Tolerance]
     Headline
          Is one space contained in the other
     Usage
          b = isContained(S, T)
	  b = isContained(D, E)
     Inputs
	  S:PolySpace
	  T:PolySpace
	  D:DualSpace
	  E:DualSpace
     Outputs
          b:Boolean
	       whether S is contained in T (or D in E).
     Description
          Text
	       Determines numerically whether the first polynomial space is contained in the second.
///

