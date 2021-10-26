-- taken from Avramov-Grayson chapter, and fixed to use more modern ordering.

changeRing = H -> (
   S := ring H;
   K := coefficientRing S;
   degs := select(degrees source vars S,
        d -> 0 != first d);
   R := K[X_1 .. X_#degs, Degrees => degs,
        Heft => (options S).Heft];
   phi := map(R,S,join(gens R,(numgens S - numgens R):0));
   prune (phi ** H));
Ext(Module,Ring) := (M,k) -> (
   B := ring M;
   if ideal k != ideal vars B
   then error "expected the residue field of the module";
   changeRing Ext(M,coker vars B));
T = ZZ[t,u, MonomialOrder => RevLex, Inverses=>true,Weights=>{-3,-1}];
poincareSeries2 = M -> (
   B := ring M;
   k := B/ideal vars B;
   H := Ext(M,k);
   S := ring H;
   T' := degreesRing ring H;
   substitute(hilbertSeries H, {T'_0=>t^-1,T'_1=>u^-1} ));
poincareSeries1 = M -> (
   substitute(poincareSeries2 M, {u=>1_T})
   );
K = ZZ/103;
A = K[x,y,z];
I = trim ideal(x^3,y^4,z^5)
B = A/I;
f = random (B^3, B^{-2,-3})
M = cokernel f;
k = B/(x,y,z);
H = Ext(M,k);
S = ring H;
g = poincareSeries1 M
expansion = (n,g) -> (value numerator g + t^n)//(value denominator g)

r = expansion(20,poincareSeries1 M)
--                  2      3      4      5      6      7      8      9      10       11       12       13       14
-- o18 = 3 + 2t + 4t  + 10t  + 15t  + 25t  + 32t  + 46t  + 55t  + 73t  + 84t   + 106t   + 119t   + 145t   + 160t

m = 7
rks1 = take (flatten entries last coefficients r, m)
psi = map(K,B)
rks2 = apply(m, i -> rank (psi ** Ext^i(M,coker vars B)))
assert( rks1 == rks2 )
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test/slow complete-intersections.out"
-- End:
