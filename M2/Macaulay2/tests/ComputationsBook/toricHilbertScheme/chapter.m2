A = {{1,1,1,1,1},{0,1,2,7,8}}; 
R = QQ[a..e,Degrees=>transpose A]; 
describe R 
B = transpose syz matrix A 
load "LLL.m2"; 
LLL syz matrix A 
B = transpose LLL syz matrix A 
toBinomial = (b,R) -> (
     top := 1_R; bottom := 1_R;
     scan(#b, i -> if b_i > 0 then top = top * R_i^(b_i)
          else if b_i < 0 then bottom = bottom * R_i^(-b_i));
     top - bottom); 
J = ideal apply(entries B, b -> toBinomial(b,R)) 
scan(gens ring J, f -> J = saturate(J,f))
toricIdeal = (A) -> (
    n := #(A_0);  
    R = QQ[vars(0..n-1),Degrees=>transpose A,MonomialSize=>16]; 
    B := transpose LLL syz matrix A;
    J := ideal apply(entries B, b -> toBinomial(b,R));
    scan(gens ring J, f -> J = saturate(J,f));
    J
    ); 
I = toricIdeal A; 
transpose mingens I
graver = (I) -> (
    R := ring I;
    k := coefficientRing R;
    n := numgens R;
    -- construct new ring S with 2n variables
    S := k[Variables=>2*n,MonomialSize=>16];
    toS := map(S,R,(vars S)_{0..n-1});
    toR := map(R,S,vars R | matrix(R, {toList(n:1)}));
    -- embed I in S
    m := gens toS I;
    -- construct the toric ideal of the Lawrence 
    -- lifting of A
    i := 0;
    while i < n do (
        wts := join(toList(i:0),{1},toList(n-i-1:0));
        wts = join(wts,wts);
        m = homogenize(m,S_(n+i),wts);
        i=i+1;
        );
   J := ideal m;
   scan(gens ring J, f -> J = saturate(J,f));
   -- apply the map toR to the minimal generators of J 
   f := matrix entries toR mingens J;
   p := sortColumns f;
   f_p) ;  
Graver = graver I 
graverFibers = (Graver) -> (
     ProductIdeal := (I) -> ( trim ideal(
        apply(numgens I, a -> ( 
            f := I_a; leadTerm f * (leadTerm f - f))))); 
     PI := ProductIdeal ideal Graver; 
     R := ring Graver; 
     new HashTable from apply(
         unique degrees source Graver,
         d -> d => compress (basis(d,R) % PI) ));
fibers = graverFibers Graver 
generateAmonos = (Graver) -> (
     trueHS := poincare coker Graver;
     fibers := graverFibers Graver;
     fibers = apply(sort pairs fibers, last);
     monos = {};
     selectStandard := (fibers, J) -> (
     if #fibers == 0 then (
        if trueHS == poincare coker gens J
        then (monos = append(monos,flatten entries mingens J));
     ) else (
        P := fibers_0;
        fibers = drop(fibers,1);
        P = compress(P % J);
        nP := numgens source P; 
        -- nP is the number of monomials not in J.
        if nP > 0 then (
           if nP == 1 then selectStandard(fibers,J)
           else (--remove one monomial from P,take the rest.
                 P = flatten entries P;
                 scan(#P, i -> (
                      J1 := J + ideal drop(P,{i,i});
                      selectStandard(fibers, J1)))));
     ));
     selectStandard(fibers, ideal(0_(ring Graver)));
     ) ; 
generateAmonos Graver;
#monos 
scan(0..9, i -> print toString monos#i) 
findPositiveVector = (m,s) -> (
     expvector := first exponents s - first exponents m;
     n := #expvector;
     i := first positions(0..n-1, j -> expvector_j > 0);
     splice {i:0, 1, (n-i-1):0}
     );
flips = (M) -> (
     R := ring M;
     -- store generators of M in monoms
     monoms := first entries generators M;
     result := {};
     -- test each generator of M to see if it leads to a neighbor 
     scan(#monoms, i -> (
       m := monoms_i;
       rest := drop(monoms,{i,i});
       b := basis(degree m, R);
       s := (compress (b % M))_(0,0);
       J := ideal(m-s) + ideal rest;
       if poincare coker gens J == poincare coker gens M then (
         w := findPositiveVector(m,s);
         R1 := (coefficientRing R)[generators R, Weights=>w];
         J = substitute(J,R1);
         J = trim ideal leadTerm J;
         result = append(result,J);
         )));
     result
);
R = QQ[a..e,Degrees=>transpose A];
M = ideal(a*e,c*d,a*c,a^2*d^2,a^2*b*d,a^3*d,c^2*e^3,
          c^3*e^2,c^4*e,c^5,c*e^5,a*d^5,b*e^6);
F = flips M
#F
scan(#F, i -> print toString entries mingens F_i)
stdMonomials = (M) -> (
     R := ring M;
     RM := R/M;
     apply(numgens M, i -> (
           s := basis(degree(M_i),RM); lift(s_(0,0), R)))
     ); 
R = QQ[a..e,Degrees => transpose A ]; 
M = ideal(a^3*d, a^2*b*d, a^2*d^2, a*b^3*d, a*b^2*d^2, a*b*d^3, 
          a*c, a*d^4, a*e, b^5*d, b^4*d^2, b^3*d^3, b^2*d^4, 
          b*d^5, b*e, c*e^5); 
toString stdMonomials M 
inequalities = (M) -> (
        stds := stdMonomials(M);
        transpose matrix apply(numgens M, i -> (
            flatten exponents(M_i) - 
                flatten exponents(stds_i)))); 
inequalities M
primitive := (L) -> (
     n := #L-1; g := L#n;
     while n > 0 do (n = n-1; g = gcd(g, L#n););
     if g === 1 then L else apply(L, i -> i // g));
load "polarCone.m2" 
decideCoherence = (M) -> (
     ineqs := inequalities M;
     c := first polarCone ineqs;
     m := - sum(numgens source c, i -> c_{i});
     prods := (transpose m) * ineqs;
     if numgens source prods != numgens source compress prods
     then false else primitive (first entries transpose m)); 
decideCoherence M
N = ideal(a*e,c*d,a*c,c^3*e,a^3*d,c^4,a*d^4,a^2*d^3,c*e^5,
           c^2*e^4,d^7);
decideCoherence N
A22 =
  {{1,1,1,1,1,1,1,1,1},{0,0,0,1,1,1,0,0,0},{0,0,0,0,0,0,1,1,1},
  {1,0,0,1,0,0,1,0,0},{0,1,0,0,1,0,0,1,0},{0,0,1,0,0,1,0,0,1}}; 
I22 = toricIdeal A22
Graver22 = graver I22;
generateAmonos(Graver22);
#monos
scan(0..9,i->print toString monos#i) 
localCoherentEquations = (IA) -> (
     -- IA is the toric ideal of A living in a ring equipped
     -- with weight order w, if we are computing the local 
     -- equations about the initial ideal of IA w.r.t. w.
     R := ring IA;
     w := (monoid R).Options.Weights;
     M := ideal leadTerm IA;
     S := first entries ((gens M) % IA);
     -- Make the universal family J in a new ring.
     nv := numgens R; n := numgens M;
     T = (coefficientRing R)[generators R, z_1 .. z_n, 
                             Weights => flatten splice{w, n:0},
                             MonomialSize=>16];
     M = substitute(generators M,T);
     S = apply(S, s -> substitute(s,T));
     J = ideal apply(n, i -> 
               M_(0,i) - T_(nv + i) * S_i);
     -- Find the ideal Ihilb of local equations about M:
     spairs := (gens J) * (syz M);
     g := forceGB gens J;
     B = (coefficientRing R)[z_1 .. z_n,MonomialSize=>16];
     Fones := map(B,T, matrix(B,{splice {nv:1}}) | vars B);
     Ihilb := ideal Fones (spairs % g);
     Ihilb
     );
IA = toricIdeal A;
Y = QQ[a..e, MonomialSize => 16,
            Degrees => transpose A, Weights => {9,3,5,0,0}];
IA = substitute(IA,Y);
JM = localCoherentEquations(IA)
load "minPres.m2";
G = removeRedundantVariables JM
ideal gens gb(G JM)
CX = QQ[a..e, z_5,z_10,z_11,z_13, Weights =>
      {9,3,5,0,0,0,0,0,0}];
F = map(CX, ring J, matrix{{a,b,c,d,e}} | 
            substitute(G.matrix,CX))
J1 = F J
substitute(ideal(z_11^2),CX) + J1
A = {{1,1,1,1,1,1,1},{0,6,7,5,8,4,3},{3,7,2,0,7,6,1},
   {6,5,2,6,5,0,0}};
IA = toricIdeal A
Y = QQ[a..g, MonomialSize => 16,
           Weights => {0,0,276,220,0,0,215},
           Degrees =>transpose A];
IA = substitute(IA,Y);
M = ideal leadTerm IA
JM = localCoherentEquations(IA)
G = removeRedundantVariables JM;
toString ideal gens gb(G JM)
K = ideal(z_32*z_42*z_44-z_37^2,z_32^4*z_35-z_42,
    z_32^3*z_35*z_37^2-z_42^2*z_44,z_32^2*z_35*z_37^4-z_42^3*z_44^2,
    z_32*z_35*z_37^6-z_42^4*z_44^3,z_35*z_37^8-z_42^5*z_44^4);
GG = removeRedundantVariables K;
ideal gens gb (GG K)
A = {{1,1,1,1},{0,1,2,3}};
I = toricIdeal A;
Graver = graver I;
fibers = graverFibers Graver;
peek fibers
G = trim product(values fibers, ideal)
numgens G
z = symbol z;
S = QQ[a,b,c,d,z];
zG = z ** substitute(gens G, S);
R = QQ[y_1 .. y_22];
F = map(S,R,zG)
PA = trim ker F
codim PA
degree PA
Aff = apply(1..22, v -> (
                       K = substitute(PA,y_v => 1);
                       FF = removeRedundantVariables K;
                       ideal gens gb (FF K)));
scan(Aff, i -> print toString i);
code primitive
code toZZ
code rotateMatrix
code isRedundant
code fourierMotzkin
code(polarCone,Matrix,Matrix)
code(polarCone,Matrix)
H = transpose matrix{
{1,2,3},
{1,3,2},
{2,1,3},
{2,3,1},
{3,1,2},
{3,2,1}};
P = polarCone H
Q = polarCone P_0
A = QQ[a..e];
I = ideal(a-b^2-1, b-c^2, c-d^2, a^2-e^2)
F = removeRedundantVariables I
I1 = ideal gens gb(F I)
ideal compress (F.matrix - vars A) + I1
code findRedundant
code removeRedundantVariables
