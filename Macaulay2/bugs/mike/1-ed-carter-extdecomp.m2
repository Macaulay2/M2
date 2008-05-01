--load "/Users/mike/Library/Application Support/Macaulay2/local/share/Macaulay2/Kronecker.m2"
Q = ZZ/101[x,y]

tallMod = method()
tallMod(ZZ) := n -> (
     coker map(Q^(n+1), n, (i,j) -> (
	       if i == j then x
	       else if i == j+1 then y
	       else 0))
     )

shortMod = method()
shortMod(ZZ) := n -> (
     coker map(Q^n, n+1, (i,j) -> (
	       if i == j then x
	       else if j == i+1 then y
	       else 0))
     )
jordanMod = method()
jordanMod(RingElement, ZZ) := (c,n) -> (
     coker map(Q^n, n, (i,j) -> (
	       if i == j then x-c*y
	       else if j == i+1 then y
	       else 0))
     )

getPencilIndices = method()
getPencilIndices(Ring,Matrix,Matrix) := (Q,xi1,xi2) -> (
     Q2 := Q[X1,X2];
     use Q2;
     assert(numgens target xi1 == numgens target xi2);
     assert(numgens source xi1 == numgens source xi2);
     P := map(Q2^(numgens target xi1), Q2^(numgens source xi1) ** Q2^{-1}, 
	  (i,j) -> X1*xi1_j_i + X2*xi2_j_i +X1-X1, Degree => 0);
     --print P;
     kroneckerIndices(P, X1, X2)
     )

quotientRingInvariants = method()
quotientRingInvariants(Module, RingElement, RingElement) := (MM, f1, f2) -> (
     Q := ring MM;
     use Q;
     R := Q/(f1,f2);
     use R;
     M := trim(R ** MM);
     FF := res M;
     K := R^1/(x,y);
     use Q;
     {numgens FF_0, numgens FF_1,
	  max apply(degrees Ext(M,K), a -> -a#0)}
     )

decomposeExtOverTor = method()
decomposeExtOverTor(Module, RingElement, RingElement) := (MM, f1, f2) -> (
     Q := ring MM;
     use Q;
     M := MM ** Q^1/(f1,f2);
     K = Q^1/(x,y);
     FF := res M;
     xi1 := Hom(nullhomotopy(f1*id_FF), K);
     xi2 := Hom(nullhomotopy(f2*id_FF), K);
     
     freeimage := trim (image(xi1*xi2))_0;
     p := numgens freeimage;
     F2 := source(xi1_(-2));
     F0 := target((xi1*xi2)_(-2));
     freegens := map(F2, , map(F0, , gens freeimage) // (xi1*xi2)_(-2));
     F1 := target(xi1_(-2));
     ker2 := map(F2, , mingens (ker(xi1*xi2))_(-2)) ** K;
     im1 := map(F1, , mingens(image(xi1_(-2)*ker2) + image(xi2_(-2)*ker2))) ** K;
     xi12 := (xi1_(-2)*ker2) // im1;
     xi22 := (xi2_(-2)*ker2) // im1;
     source1 := map(F1, , mingens(F1/(image(xi1_(-2)) + image(xi2_(-2))))) ** K;
     target0 := map(F0, , mingens(F0/freeimage)) ** K;
     xi11 := (xi1_(-1)*source1) // target0;
     xi21 := (xi2_(-1)*source1) // target0;
     
     (p, getPencilIndices(Q, xi12, xi22), 
	  getPencilIndices(Q, xi11, xi21),
	  quotientRingInvariants(MM, f1, f2))
     )

--extRegularity = method()
--extRegularity(Module) := M -> (
--     R := ring M;
--     K := R^1/(promote(x,R),promote(y,R));
--     E := Ext(M,K);
--     Q := ring E;
--     EE := trim(E ** Q/image((vars Q)*matrix"0,0;0,0;1,0;0,1"));
--     print betti res EE;
--     regularity res(EE, LengthLimit => 15)
--     )

syzygy = method()
syzygy(ZZ, Module, RingElement, RingElement) := (n, M, f1, f2) -> (
     assert(n >= 0);
     Q := ring M;
     use Q;
     if n == 0 then (
	  M ** Q^1/(f1,f2)
	  )
     else (
	  FF := res(trim(Q/(f1,f2) ** M), LengthLimit => n+2);
	  f := map(ring coker FF.dd_(n+1), Q, {x,y});
	  pushForward(f, coker FF.dd_(n+1))
	  )
     )

freeRank = method()
freeRank(Module, RingElement, RingElement) := (MM, f1, f2) -> (
     a := decomposeExtOverTor(MM, f1, f2);
     a#0
     )

computeL = method()
computeL(Module, RingElement, RingElement) := (MM, f1, f2) -> (
     a := decomposeExtOverTor(MM, f1, f2);
     (#select(a#2#0, i -> i > 0))
         + (#select(a#1#1, i -> i > 0))
	 + (#select(a#2#0, i -> i == 0))
     )

maxKernelDegree = method()
maxKernelDegree(Module, RingElement, RingElement) := (MM, f1, f2) -> (
     a := decomposeExtOverTor(MM, f1, f2);
     max(a#1#0 | a#2#0)
     )

highestKernelDiff = method()
highestKernelDiff(Module, RingElement, RingElement) := (MM, f1, f2) -> (
     a := decomposeExtOverTor(MM, f1, f2);
     max(apply({a#1#0, a#2#0}, l -> (
		    n := #l;
		    if n >= 2 then l#(n-1) - l#(n-2)
		    else if n == 1 then l#0 else 0
		    )
	       )
	  )
     )

degreeDiff = method()
degreeDiff(Module, RingElement, RingElement) := (MM, f1, f2) -> (
     a := quotientRingInvariants(MM, f1, f2);
     if a#2 <= a#0 + a#1 then 0_ZZ else a#2 - a#0 - a#1
     --min(apply({a#2-a#0, a#2-a#1-1}, n -> if n < 0 then 0_ZZ else n))
     )

flattenList = method()
flattenList(List) := a -> (
     fold(a, (i,j) -> i | j)
     )

end

load "/Users/mike/src/M2/Macaulay2/bugs/mike/1-ed-carter-extdecomp.m2"
-- Here we give an engineer's proof that l can take any 
-- non-negative value other than 1.
apply(1..8, n -> computeL(tallMod n, x^n, y^n))

-- These computations give an engineer's proof for the complete 
-- picture over the ring k[x,y]/(x^2,y^2).
decomposeExtOverTor(Q^1/(x^2,y^2), x^2, y^2)
apply(1..8, n -> decomposeExtOverTor(shortMod n, x^2, y^2))
apply(1..8, n -> decomposeExtOverTor(tallMod n, x^2, y^2))
apply(1..8, n -> decomposeExtOverTor(jordanMod(0_Q, n), x^2, y^2))
-- The linear form should be c^2*X1+X2 in these three examples.
apply(1..5, n -> decomposeExtOverTor(jordanMod(1_Q, n), x^2, y^2))
apply(1..5, n -> decomposeExtOverTor(jordanMod(2_Q, n), x^2, y^2))
apply(1..5, n -> decomposeExtOverTor(jordanMod(3_Q, n), x^2, y^2))
decomposeExtOverTor(Q^1/(y), x^2, y^2)
decomposeExtOverTor(coker matrix"y,x;0,y", x^2, y^2)

-- Same picture as above, but just compute L instead of
-- giving the full decomposition.
computeL(Q^1/(x^2,y^2), x^2, y^2)
apply(1..8, n -> computeL(shortMod n, x^2, y^2))
apply(1..8, n -> computeL(tallMod n, x^2, y^2))
apply(1..8, n -> computeL(jordanMod(0_Q, n), x^2, y^2))
-- The linear form should be c^2*X1+X2 in these three examples.
apply(1..5, n -> computeL(jordanMod(1_Q, n), x^2, y^2))
apply(1..5, n -> computeL(jordanMod(2_Q, n), x^2, y^2))
apply(1..5, n -> computeL(jordanMod(3_Q, n), x^2, y^2))
computeL(Q^1/(y), x^2, y^2)
computeL(coker matrix"y,x;0,y", x^2, y^2)

-- Test the cokernels of random matrices.
maxTargetRank = 9
maxSourceRank = 8
maxDegree = 4
a = flattenList(apply(maxTargetRank, j -> flattenList(apply(maxSourceRank, i -> apply(maxDegree - 1, d -> (
		    {i, j, d+1, highestKernelDiff(coker random(Q^j, Q^i ** Q^{-d-1}), x^3, y^3)}
		    ))))))
apply(a, i -> i#3)
a = flattenList(apply(maxTargetRank, j -> flattenList(apply(maxSourceRank, i -> apply(maxDegree - 1, d -> (
		    {i, j, d+1, degreeDiff(coker random(Q^j, Q^i ** Q^{-d-1}), x^4, y^4)}
		    ))))))
apply(a, i -> i#3)

M = Q^1/(x^4,y^4) ** coker random(Q^7, Q^5 ** Q^{-2})
decomposeExtOverTor(M, x^4, y^4)
for i from 1 to 3 do (
     print decomposeExtOverTor(syzygy(i, M, x^4, y^4), x^4, y^4);
     )
betti res(trim(Q/(x^4,y^4) ** syzygy(4, M, x^4, y^4)), LengthLimit => 20)

M = Q^1/(x^2,y^2) ** tallMod 5
decomposeExtOverTor(M, x^2, y^2)
for i from 1 to 4 do (
     print decomposeExtOverTor(syzygy(i, M, x^2, y^2), x^2, y^2);
     )

M = coker random(Q^4, Q^3 ** Q^{-2})
decomposeExtOverTor(M, x^3, y^3)
decomposeExtOverTor(syzygy(1, M, x^3, y^3), x^3, y^3)
decomposeExtOverTor(syzygy(2, M, x^3, y^3), x^3, y^3)
decomposeExtOverTor(syzygy(3, M, x^3, y^3), x^3, y^3)
decomposeExtOverTor(syzygy(4, M, x^3, y^3), x^3, y^3)

-- Here is a module where shortMod exists in the Ext decomposition without
-- the next shortMod down.  However, it also exists with some other things.
M = Q^1/(x^3,y^3) ** coker matrix{{40*x^2+46*x*y-10*y^2,-44*x^2+27*x*y-43*y^2},
     {-43*x^2-15*x*y+29*y^2,-8*x^2-16*x*y-25*y^2}}
decomposeExtOverTor(M, x^3, y^3)
-- For comparison, look at this module where shortMod's exist in pairs.
decomposeExtOverTor(tallMod 6, x^2, y^2)
FF = res M
K = Q^1/(x,y)
Hom(nullhomotopy(x^3*id_FF), K)
Hom(nullhomotopy(y^3*id_FF), K)
(A,P1,P2) = kroneckerNormalForm matrix{{0,0,-35*x,-49*x-y,-41*x},{0,0,19*x,0,-y}}
B = matrix{{1_Q,0},{0,1},{0,0},{0,0},{0,0}} | (matrix{{x^3,y^3,0},{0,x^3,y^3}} // FF.dd_1)
FF.dd_1*B

