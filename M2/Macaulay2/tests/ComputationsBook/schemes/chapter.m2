S = ZZ[x, y, z];
elementaryBasis = ideal(x+y+z, x*y+x*z+y*z, x*y*z);
saturate(elementaryBasis, x)
powerSumBasis = ideal(x+y+z, x^2+y^2+z^2, x^3+y^3+z^3);
saturate(powerSumBasis, x)
clearAll
S = QQ[t, y_0 .. y_8, a..i, MonomialOrder => Eliminate 10];
N3 = (matrix {{0,1,0},{0,0,1},{0,0,0}}) ** S
G = genericMatrix(S, y_0, 3, 3)
classicalAdjoint = (G) -> (
     n := degree target G;
     m := degree source G;
     matrix table(n, n, (i, j) -> (-1)^(i+j) * det(
               submatrix(G, {0..j-1, j+1..n-1}, 
                    {0..i-1, i+1..m-1}))));
num = G * N3 * classicalAdjoint(G);
D = det(G);
M = genericMatrix(S, a, 3, 3);
elimIdeal = minors(1, (D*id_(S^3))*M - num) + ideal(1-D*t);
closureOfOrbit = ideal selectInSubring(1, gens gb elimIdeal);
X = ideal substitute(
        contract(matrix{{t^2,t,1}}, det(t-M)),
        {t => 0_S})
closureOfOrbit == X
clearAll
S = QQ[x, y, z, a..j, MonomialOrder => Eliminate 2];
F = a*x^3+b*x^2*y+c*x^2*z+d*x*y^2+e*x*y*z+f*x*z^2+g*y^3+h*y^2*z+
             i*y*z^2+j*z^3;
partials = submatrix(jacobian matrix{{F}}, {0..2}, {0})
singularities = ideal(partials) + ideal(F);
elimDiscr = time ideal selectInSubring(1,gens gb singularities);
elimDiscr = substitute(elimDiscr, {z => 1});
A = contract(matrix{{x^2,x*y,y^2,x*z,y*z,z^2}},
        diff(transpose matrix{{x,y,z}},F))
hess = det submatrix(jacobian ideal partials, {0..2}, {0..2});
B = contract(matrix{{x^2,x*y,y^2,x*z,y*z,z^2}},
        diff(transpose matrix{{x,y,z}},hess))
detDiscr = ideal det (A || B);
detDiscr == elimDiscr
detDiscr_0
numgens detDiscr
# terms detDiscr_0
clearAll
S = QQ[a,b,x,y, MonomialOrder => Eliminate 2];
I1 = ideal(x-a, y-a, a^2-2);
ideal selectInSubring(1, gens gb I1)
I2 = ideal(x-a, y-b, a^2-2, b^2-3);
ideal selectInSubring(1, gens gb I2)
I3 = ideal(x-a, y-a^4, a^4+a^3+a^2+a+1);
ideal selectInSubring(1, gens gb I3)
I4 = ideal(a*x+b*y, a^2-2, b^2-3);
ideal selectInSubring(1, gens gb I4)
I5 = ideal(a*x+b*y-1, a^2-2, b^2-3);
ideal selectInSubring(1, gens gb I5)
clearAll
S = QQ[x, y, z];
I = ideal(x^5+y^3+z^3, x^3+y^5+z^3, x^3+y^3+z^5);
multiplicity = degree(I : saturate(I))
clearAll
PP3 = QQ[t, x, y, z, w];
L = ideal(x, y);
M = ideal(x-t*z, y+t^2*w);
X = intersect(L, M);
Xzero = trim substitute(saturate(X, t), {t => 0})
Xzero == intersect(ideal(x^2, y), ideal(x, y^2, z))
degree(ideal(x^2, y ) / ideal(x, y^2, z))
clearAll
S = QQ[a, b, c, d, e];
IX = trim minors(2, matrix{{a, b^2, b*d, c},{b, a*c, c^2, d}})
IY = ideal(a, d);
codim IX + codim IY == codim (IX + IY)
(degree IX) * (degree IY)
degree (IX + IY)
J = ideal mingens (IX + ideal(a))
J == intersect(ideal(a, b*c, b^2, c^3-b*d^2), 
     ideal(a, d, b*c, c^3, b^3)) -- embedded point
clearAll
blowUpIdeal = (I) -> (
     r := numgens I;
     S := ring I;
     n := numgens S;
     K := coefficientRing S;
     tR := K[t, gens S, vars(0..r-1), 
               MonomialOrder => Eliminate 1];
     f := map(tR, S, submatrix(vars tR, {1..n}));
     F := f(gens I);
     J := ideal apply(1..r, j -> (gens tR)_(n+j)-t*F_(0,(j-1)));
     L := ideal selectInSubring(1, gens gb J);
     R := K[gens S, vars(0..r-1)];
     g := map(R, tR, 0 | vars R);
     trim g(L));
S = QQ[x, y];
I = ideal(x^3, x*y, y^2);
J = blowUpIdeal(I)
J + ideal jacobian J == ideal gens ring J
clearAll
PP4 = QQ[a..e];
S = QQ[r..t, A..E, MonomialOrder => Eliminate 3];
I = ideal(A - r^2, B - s^2, C - r*s, D - r*t, E - s*t);
phi = map(PP4, S, matrix{{0_PP4, 0_PP4, 0_PP4}} | vars PP4)
surfaceA = phi ideal selectInSubring(1, gens gb I)
R = QQ[t, x, y, z, u, v, MonomialOrder => Eliminate 1];
blowUpIdeal = ideal selectInSubring(1, gens gb ideal(u-t*x, 
     v-t*y))
PP2xPP1 = QQ[x, y, z, u, v];
embed = map(PP2xPP1, R, 0 | vars PP2xPP1);
blowUp = PP2xPP1 / embed(blowUpIdeal);
PP5 = QQ[A .. F];
segre = map(blowUp, PP5, matrix{{x*u,y*u,z*u,x*v,y*v,z*v}});
ker segre
projection = map(PP4, PP5, matrix{{a, c, d, c, b, e}})
surfaceB = trim projection ker segre
determinantal = minors(2, matrix{{a, c, d}, {b, d, e}})
sigma = map( PP4, PP4, matrix{{d, e, a, c, b}});
surfaceC = sigma determinantal
surfaceA == surfaceB
surfaceB == surfaceC
clearAll
PP3 = QQ[t, x, y, z, w];
Q = ideal( t*x^2+t*y^2+t*z^2+w^2 );
R = QQ[t, u, v, A .. H];
phi = map(R, PP3, matrix{{t}} | 
        u*matrix{{A, B, C, D}} + v*matrix{{E, F, G, H}});
imageFamily = phi Q;
coeffOfFamily = contract(matrix{{u^2,u*v,v^2}}, gens imageFamily)
S = QQ[t, A..H];
coeffOfFamily = substitute(coeffOfFamily, S);
Sbar = S / (ideal coeffOfFamily);
psi = matrix{{t}} | exteriorPower(2, 
            matrix{{A, B, C, D}, {E, F, G, H}})
PP5 = QQ[t, a..f];
fanoOfFamily = trim ker map(Sbar, PP5, psi);
zeroFibre = trim substitute(saturate(fanoOfFamily, t), {t=>0})
transpose gens zeroFibre
oneFibre = trim substitute(saturate(fanoOfFamily, t), {t => 1})
oneFibre == intersect(ideal(c-d, b+e, a-f, d^2+e^2+f^2), 
     ideal(c+d, b-e, a+f, d^2+e^2+f^2))
