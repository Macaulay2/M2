needsPackage "Schubert2"
needs "Schubert2/pushfor.m2"
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
     PN := projectiveBundle'(dual N, VariableNames => {,{x}});
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
     I2 := promote( ideal select( flatten (
             for i from 1 to n-1 list 
	       for j from i to n-1 list (
		    f := (vars D1) * iLowerMod (alphas#i * alphas#j);
		    E_i * E_j - E_0 * f
	  )), x -> x != 0), D1);
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
	  (vars D * cfA * E0powers)_(0,0)
	  );
     Ytilde.TangentBundle = abstractSheaf(Ytilde, 
	  ChernCharacter => ch tangentBundle Y - jLower(ch tangentBundle(PN/X) * (todd OO(x))^-1));
     (Ytilde, PN, jLower)
     )
