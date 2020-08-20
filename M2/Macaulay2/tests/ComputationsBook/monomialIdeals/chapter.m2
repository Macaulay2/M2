S = QQ[a, b, c, d]; 
I = monomialIdeal(a^2, a*b, b^3, a*c)
J = monomialIdeal{a^2, a*b, b^2}
monomialIdeal(a^2+a*b, a*b+3, b^2+d)
K = ideal(a^2, b^2, a*b+b*c)
monomialIdeal K
monomialIdeal gens K
isMonomialIdeal K
isMonomialIdeal ideal(a^5, b^2*c, d^11)
I+J
fvector = I -> (
     R := (ring I)/I;
     d := dim R;
     N := poincare R;
     t := first gens ring N;
     while 0 == substitute(N, t => 1) do N = N // (1-t);
     h := apply(reverse toList(0..d), i -> N_(t^i));
     f := j -> sum(0..j+1, i -> binomial(d-i, j+1-i)*h#(d-i));
     apply(toList(0..d-1), j -> f(j)));
S = QQ[x_1 .. x_6];
octahedron = monomialIdeal(x_1*x_2, x_3*x_4, x_5*x_6)
fvector octahedron
simplicial2sphere = v -> ( 
     S := QQ[x_1..x_v]; 
     if v === 4 then monomialIdeal product gens S 
     else ( 
          L := {};
          scan(1..v-4, i -> L = L | apply(v-i-3, 
                    j -> x_i*x_(i+j+4))); 
          scan(2..v-3, i -> L = L | {x_i*x_(i+1)*x_(i+2)}); 
          monomialIdeal L));
apply({4,5,6,7,8}, j -> fvector simplicial2sphere(j))
supp = r -> select(gens ring r, e -> r % e == 0);
monomialDecompose = method();
monomialDecompose List := L -> (
     P := select(L, I -> all(first entries gens I, 
               r -> #supp(r) < 2) === false);
     if #P > 0 then (
          I := first P;
          m := first select(first entries gens I, 
               r -> #supp(r) > 1);
          E := first exponents m;
          i := position(E, e -> e =!= 0);
          r1 := product apply(E_{0..i}, (gens ring I)_{0..i}, 
               (j, r) -> r^j);
          r2 := m // r1;
          monomialDecompose(delete(I, L) | {I+monomialIdeal(r1),
                    I+monomialIdeal(r2)}))
     else L);
monomialDecompose MonomialIdeal := I -> monomialDecompose {I};
S = QQ[a,b,c,d];
I = monomialIdeal(a^3*b, a^3*c, a*b^3, b^3*c, a*c^3, b*c^3)
P = monomialDecompose I;
scan(P, J -> << endl << J << endl);
I == intersect(P)
code(dual, MonomialIdeal, List)
code(primaryDecomposition, MonomialIdeal)
L = primaryDecomposition I;
scan(L, J -> << endl << J << endl);
I == intersect L
treeIdeal = n -> (
     S = QQ[vars(0..n-1)];
     L := delete({}, subsets gens S);
     monomialIdeal apply(L, F -> (product F)^(n - #F +1)));
apply(2..6, i -> #primaryDecomposition treeIdeal i)
minorsIdeal = (m,n,k) -> (
     S := QQ[x_1..x_(m*n), MonomialOrder => Lex];
     I := minors(k, matrix table(m, n, (i,j) -> x_(i*n+n-j)));
     forceGB gens I;
     I);
apply(2..8, i -> time codim monomialIdeal minorsIdeal(i,2*i,2))
erase symbol x;
stdPairs = I -> (
     S := ring I;
     X := gens S;
     std := {};
     J := I;
     while J != S do (
          w1 := 1_S;
          F := X;
          K := J;
          while K != 0 do (
               g1 := (ideal mingens ideal K)_0;
               x := first supp g1;
               w1 = w1 * g1 // x;
               F = delete(x, F);
               K = K : monomialIdeal(g1 // x);
               L := select(first entries gens K, 
                    r -> not member(x, supp r));
               if #L > 0 then K = monomialIdeal L
               else K = monomialIdeal 0_S;);
          w2 := w1;
          scan(X, r -> if not member(r, supp w1) or member(r, F)
               then w2 = substitute(w2, {r => 1}));
          P := monomialIdeal select(X, r -> not member(r, F));
          if (I:(I:P) == P) and (all(std, p -> 
                    (w2 % (first p) != 0) or not
                    isSubset(supp(w2 // first p) | F, last p)))
          then std = std | {{w2, F}};
          J = J + monomialIdeal(w1););
     std);
S = QQ[x,y,z];
I = monomialIdeal(x*y^3*z, x*y^2*z^2, y^3*z^2, y^2*z^3);
scan(time stdPairs I, P -> << endl << P << endl);
code(standardPairs, MonomialIdeal, List)
time standardPairs I;
permutohedronIdeal = n -> (
     S := QQ[X_1..X_n];
     monomialIdeal terms det matrix table(n ,gens S, 
          (i,r) -> r^(i+1)));
L = apply({2,3,4,5}, j -> standardPairs(permutohedronIdeal(j)));
apply(L, i -> #i)
erase symbol x; erase symbol z;
toBinomial = (b, S) -> (
     pos := 1_S;
     neg := 1_S;
     scan(#b, i -> if b_i > 0 then pos = pos*S_i^(b_i)
                   else if b_i < 0 then neg = neg*S_i^(-b_i));
     pos - neg);
toricIdeal = (A, omega) -> (
     n := rank source A;
     S = QQ[x_1..x_n, Weights => omega, MonomialSize => 16];
     B := transpose matrix syz A;
     J := ideal apply(entries B, b -> toBinomial(b, S));
     scan(gens S, r -> J = saturate(J, r));
     J);
IP = (A, omega, beta) -> (
     std := standardPairs monomialIdeal toricIdeal(A, omega);
     n := rank source A;
     alpha := {};
     Q := first select(1, std, P -> (
          F := apply(last P, r -> index r);
          gamma := transpose matrix exponents first P;
          K := transpose syz (submatrix(A,F) | (A*gamma-beta));
          X := select(entries K, k -> abs last(k) === 1);
          scan(X, k -> if all(k, j -> j>=0) or all(k, j -> j<=0)
               then alpha = apply(n, j -> if member(j, F) 
                    then last(k)*k_(position(F, i -> i === j))
                    else 0));
          #alpha > 0));
     if #Q > 0 then (matrix {alpha})+(matrix exponents first Q)
     else 0);
A = matrix{{1,1,1,1,1},{1,2,4,5,6}}
w1 = {1,1,1,1,1};
w2 = {2,3,5,7,11};
b1 = transpose matrix{{3,9}}
b2 = transpose matrix{{5,16}}
IP(A, w1, b1)
IP(A, w2, b1)
IP(A, w1, b2)
IP(A, w2, b2)
S = QQ[a,b,c,d];
isBorel monomialIdeal(a^2, a*b, b^2)
isBorel monomialIdeal(a^2, b^2)
borel monomialIdeal(b*c)
borel monomialIdeal(a,c^3)
gin = method();
gin Ideal := I -> (
     S := ring I;
     StoS := map(S, S, random(S^{0}, S^{numgens S:-1}));
     monomialIdeal StoS I);
gin MonomialIdeal := I -> gin ideal I;
genericForms = (p,q) -> ideal(random(p,S), random(q,S));
gin genericForms(2,2)
gin genericForms(2,3)
J = ideal(a^2, a*b+b^2, a*c)
ginJ = gin J
inJ = monomialIdeal J
isBorel inJ and isBorel ginJ
S = QQ[a,b,c,d, MonomialOrder => Lex];
gin genericForms(2,2)
gin genericForms(2,3)
projection = I -> (
     S := ring I;
     n := numgens S;
     X := gens S;
     monomialIdeal mingens substitute(ideal I, 
          {X#(n-2) => 1, X#(n-1) => 1}));
polarization = I -> (
     n := numgens ring I;
     u := apply(numgens I, i -> first exponents I_i);
     I.lcm = max \ transpose u;
     Z := flatten apply(n, i -> apply(I.lcm#i, j -> z_{i,j}));
     R := QQ(monoid[Z]);
     Z = gens R;
     p := apply(n, i -> sum((I.lcm)_{0..i-1}));
     monomialIdeal apply(u, e -> product apply(n, i -> 
               product(toList(0..e#i-1), j -> Z#(p#i+j)))));
distraction = I -> (
     S := ring I;
     n := numgens S;
     X := gens S;
     J := polarization I;
     W := flatten apply(n, i -> flatten apply(I.lcm#i, 
               j -> X#i));
     section := map(S, ring J, apply(W, r -> r - 
               random(500)*X#(n-2) - random(500)*X#(n-1)));     
     section ideal J);
S = QQ[x_0 .. x_4, MonomialOrder => GLex];
I = monomialIdeal(x_0^2, x_0*x_1^2*x_3, x_1^3*x_4)
projection I
polarization I
distraction I
m =  matrix table({0,1,2}, {0,1,2}, (i,j) -> (gens S)#(i+j))
rationalQuartic = minors(2, m);
H = hilbertPolynomial(S/rationalQuartic);
hilbertPolynomial(S/rationalQuartic, Projective => false)
L = {monomialIdeal(x_0^2, x_0*x_1, x_0*x_2, x_1^2, x_1*x_2, x_2^2), monomialIdeal(x_0^2, x_0*x_1, x_0*x_2, x_0*x_3, x_1^2, x_1*x_2, x_2^3), monomialIdeal(x_0, x_1^2, x_1*x_2^2, x_1*x_2*x_3, x_2^3), monomialIdeal(x_0, x_1^2, x_1*x_2, x_2^4, x_2^3*x_3), monomialIdeal(x_0, x_1, x_2^5, x_2^4*x_3^3), monomialIdeal(x_0, x_1^2, x_1*x_2, x_1*x_3, x_2^5, x_2^4*x_3^2), monomialIdeal(x_0^2, x_0*x_1, x_0*x_2, x_0*x_3, x_1^2, x_1*x_2, x_1*x_3, x_2^5, x_2^4*x_3), monomialIdeal(x_0, x_1^2, x_1*x_2, x_1*x_3^2, x_2^5, x_2^4*x_3), monomialIdeal(x_0^2, x_0*x_1, x_0*x_2, x_0*x_3, x_1^2, x_1*x_2, x_1*x_3^2, x_2^4), monomialIdeal(x_0, x_1^2, x_1*x_2^2, x_1*x_2*x_3, x_1*x_3^2, x_2^4), monomialIdeal(x_0, x_1^2, x_1*x_2, x_1*x_3^3, x_2^4), monomialIdeal(x_0, x_1, x_2^6, x_2^5*x_3, x_2^4*x_3^2)};
scan(#L, i -> << endl << i+1 << " : " << L#i << endl);
all(L, I -> isBorel I and hilbertPolynomial(S/I) == H)
class1 = projection L#0
class2 = projection L#1
class3 = projection L#4
all(1..3, i -> projection L#i == class2)
all(4..11, i -> projection L#i == class3)
all(L, I -> I == monomialIdeal distraction I)
all(0..3, i -> projection gin distraction L#i == class3)
hasChainProperty = I -> (
     L := ass I;
     radI := radical I;
     all(L, P -> radI : (radI : P) == P or (
               gensP := first entries gens P;
               all(gensP, r -> (
                         Q := monomialIdeal delete(r, gensP);
                         I : (I : Q) == Q)))));
A = matrix{{1,1,1,1,1,1,1}, {2,0,0,0,1,0,0}, {0,2,0,0,0,1,0}, {2,2,0,2,1,1,1}}
IA = toricIdeal(A, {1,1,1,1,1,1,1})
inIA = monomialIdeal IA
hasChainProperty inIA
StoS = map(S, S, {x_1, x_2, x_3, x_3 - x_4, x_5, x_6, x_7});
J = StoS IA
inJ = monomialIdeal J
hasChainProperty inJ
A = matrix{{2,0,0,1,0,0,2,1,1,3,2,2,2,3,3,3},
           {0,2,0,0,1,0,1,2,1,2,3,2,3,2,3,3},
           {0,0,2,0,0,1,1,1,2,2,2,3,3,3,2,3}};
D = A^{0}+A^{1}+A^{2} || A
D = entries transpose D;
S = QQ[vars(0..15), Degrees => D, MonomialSize => 16];
I = monomialIdeal(d*j, d*k, d*l, d*m, d*n, d*o, d*p, e*j, e*k,
    e*l, e*m, e*n, e*o, e*p, f*j, f*k, f*l, f*m, f*n, f*o, f*p,
    g*j, g*k, g*l, g*m, g*n, g*o, g*p, h*j, h*k, h*l, h*m, h*n,
    h*o, h*p, i*j, i*k, i*l, i*m, i*n, i*o, i*p, g^2, g*h, g*i,
    h^2, h*i, i^2, j^2, j*k, j*l, j*m, j*n, j*o, j*p, k^2, k*l,
    k*m, k*n, k*o, k*p, l^2, l*m, l*n, l*o, l*p, m^2, m*n, m*o,
    m*p, n^2, n*o, n*p, o^2, o*p, p^2, d^2, e^2, f^2, d*h, e*i,
    f*g, f*d*i, d*e*g, e*f*h, c*d*g, a*e*h, b*f*i, c*e*g, 
    a*f*h, b*d*i, c*d*e, a*e*f, b*f*d, c*b*d, a*c*e, b*a*f, 
    c*b*g, a*c*h, b*a*i);
apply(D, d -> rank source basis(d, (S^1)/ ideal I))
hasChainProperty I
