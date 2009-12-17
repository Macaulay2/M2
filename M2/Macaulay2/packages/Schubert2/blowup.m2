-- Code to be included in Schubert2

loadPackage "Schubert2"
loadPackage "PushForward"

-- this is difficult!
-- instead, let's create the rings by hand:
A = QQ[H]/H^6
B = QQ[h]/h^3
P5 = abstractVariety(5, A)
P2 = abstractVariety(2, B)
P5.TangentBundle = abstractSheaf(P5, Rank=>5, ChernClass=>(1+H)^6)
P2.TangentBundle = abstractSheaf(P2, Rank=>2, ChernClass=>(1+h)^3)
iupper = map(B,A,{2*h})
ilower = map(A^1, A^1/(H^3), {{4*H^3}})

blowup = method()
blowup(AbstractVariety, AbstractVariety, RingMap, Matrix) := 
  (X,Y,iupper, ilower) -> (
     -- eventually, the input will be an AbstractVarietyMap
     -- assumption: 
     A := intersectionRing Y;
     B := intersectionRing X;
     if A =!= source iupper then error "expected intersection ring";
     if B =!= target iupper then error "expected intersection ring";
     -- next line should be: 
     -- N := iupper tangentBundle Y - tangentBundle X;
     TY := abstractSheaf(X, Rank=>dim Y, ChernClass=>iupper chern tangentBundle Y);
     N :=  TY - tangentBundle X;
     x := local x;
     d := rank N;
     PN := projectiveBundle(dual N, VariableNames => {,{x}});
     F := first PN.Bundles;
     C := intersectionRing PN;
     (BasAModule, bas, iLowerMod) := pushFwd iupper;
     -- iLowerMod(element b of B) = one column matrix over A whose product with bas is b
     n := numgens BasAModule;
     D1 := A[E_0..E_(n-1), Join=>false];
     alphas := first entries bas;
     Ndual := dual N;
     blist := for i from 1 to d list chern(d-i, Ndual);
     -- three types of relations.
     -- 1. relations on the generators of B as an A-module
     I1 := ideal((vars D1 * (relations BasAModule)));
     -- 2. mult map on the E_i and E_j
     I2 := ideal flatten (
             for i from 1 to n-1 list 
	       for j from i to n-1 list (
		    f := (vars D1) * iLowerMod (alphas#i * alphas#j);
		    E_i * E_j - E_0 * f
	  ));
     if I2 == 0 then I2 = trim ideal(0_D1);
     -- 3. linear relations
     I3 := ideal for i from 0 to n-1 list (
     	  f1 := matrix ilower * (iLowerMod alphas#i);
	  f2 := sum for j from 0 to d-1 list (
     	       E_0^j * ((vars D1) * iLowerMod(blist#j * alphas#i))
	       );
	  f1-f2);
     D := D1/(I1 + I2 + I3);
     Ytilde := abstractVariety(dim Y, D);
     xpowers := matrix {for i from 0 to d-1 list x^i};
     E0powers := transpose matrix {for i from 0 to d-1 list (-E_0)^i};
     jLower := (f) -> (
	  -- takes an element f of C, returns an element of D
	  cf := last coefficients(f, Monomials => xpowers);
	  cf = lift(cf, B);
	  cfA := matrix {apply(flatten entries cf, iLowerMod)};
	  ((vars D) * cfA * E0powers)_(0,0)
	  );
     -- Now compute the tangent bundle
     Ytilde.TangentBundle = xx;
     jLower, C
     )

end

restart
load "blowup.m2"

-- example to consider: blowup of P5 along the veronese

P5 = projectiveSpace(5, VariableName => symbol k)
P2 = projectiveSpace(2, VariableName => symbol h)
use P5
chern tangentBundle P5
-- now we need to make iupper, ilower
-- iupper takes H to 2*h, but we need to write where the H_(i,j) go to.
A = intersectionRing P5
B = intersectionRing P2
iupper = map(B,A,{-2*h,4*h^2, -8*h^3, 16*h^4, -32*h^5, 2*h})
isWellDefined iupper -- gives cryptic  error message
J5 = ideal first flattenRing A
A[h]/(2*h-k)
ideal 
use ring J5
sub(J5, {k => 2*k})
intersectionRing P5

sub(sub(ideal {-k, k^2, -k^3, k^4, -k^5, k}, {k=>2*k}), 

restart
load "blowup.m2"
(F,C) = blowup(P2,P5,iupper,ilower)
AYtilde = ring (F (C_2))
C_2
F (C_2)
F (C_2^2)
g h

viewHelp coefficients

