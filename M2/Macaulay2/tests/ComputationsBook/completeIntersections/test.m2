setRandomSeed();
A = QQ[w,x,y,z]
U = matrix {{w,x},{y,z}}
C = chainComplex U
L = HH_0 C
f = -det U
f * L == 0
s = nullhomotopy (-f * id_C)
s * C.dd + C.dd * s == -f
V = s_0
A = QQ[x,y,z];
f = x^3 + 3*y^3 - 2*y*z^2 + 5*z^3;
B = A/f;
m = ideal(x,y,z)
M = B^1/m^2;
F = resolution(M, LengthLimit=>8)
restrict1 = N -> coker(lift(presentation N,A) | f);
L = restrict1 cokernel F.dd_4;
C = res L;
U = C.dd_1;
print U
s = nullhomotopy (-f * id_C);
V = s_0;
print V
U*V+f==0
V*U+f==0
matrixFactorization = M -> (
   B := ring M;
   f := (ideal B)_0;
   e := numgens B;
   F := resolution(M, LengthLimit => e+1);
   L := restrict1 cokernel F.dd_(e+1);
   C := res L;
   U := C.dd_1;
   s := nullhomotopy (-f * id_C);
   V := s_0;
   assert( U*V + f == 0 );
   assert( V*U + f == 0 );
   return (U,V));
time (U,V) = matrixFactorization(B^1/m^3);
U;
V;
F.dd_3 - F.dd_5 == 0
F.dd_4 - F.dd_6 == 0
F.dd_5 - F.dd_7 == 0
M = B^1/m^2;
G = resolution(M, LengthLimit => 8, Strategy => 0)
G.dd_3 - G.dd_5 == 0
G.dd_4 - G.dd_6 == 0
G.dd_5 - G.dd_7 == 0
M = B^1/m^3;
F = resolution(M, LengthLimit=>8)
M' = restrict1 M;
C = res M'
K = ZZ/103; 
A = K[x,Degrees=>{5}];
B = A/(x^3);
M = B^1/(x^2);
N = B^1/(x);
H = Ext(M,N);
ring H
degree \ gens ring H
S = ring H;
H
A = K[x,y];
J = ideal(x^3,y^2);
B = A/J;
N = cokernel matrix{{x^2,x*y}}
time H = Ext(N,N);
ring H
S = ring H;
transpose vars S
trim J
H
partSelector = predicate -> H -> (
   R := ring H;
   H' := prune image matrix {
       select(
           apply(numgens H, i -> H_{i}),
           f -> predicate first first degrees source f
           )
       };
   H');
evenPart = partSelector even; oddPart = partSelector odd;
evenPart H
oddPart H
print code(Ext,Module,Module)
A = K[x,y,z];
J = trim ideal(x^3,y^4,z^5)
B = A/J;
f = random (B^3, B^{-2,-3})
f_{1}
M = cokernel f;
time P = Ext(M,B^1/(x,y,z));
S = ring P;
transpose vars S
R = K[X_1..X_3,Degrees => {{-2,-3},{-2,-4},{-2,-5}}];
phi = map(R,S,{X_1,X_2,X_3,0,0,0})
P = prune (phi ** P);
transpose vars ring P
evenPart P
oddPart P
changeRing = H -> (
   S := ring H;
   K := coefficientRing S;
   degs := select(degrees source vars S,
        d -> 0 != first d);
   R := K[X_1 .. X_#degs, Degrees => degs];
   phi := map(R,S,join(gens R,(numgens S - numgens R):0));
   prune (phi ** H)
   );
Ext(Module,Ring) := (M,k) -> (
   B := ring M;
   if ideal k != ideal vars B
   then error "expected the residue field of the module";
   changeRing Ext(M,coker vars B)
   );
use B;
k = B/(x,y,z);
use B;
P = Ext(M,k);
time oddPart P
Ext(Ring,Module) := (k,M) -> (
   B := ring M;
   if ideal k != ideal vars B
   then error "expected the residue field of the module";
   changeRing Ext(coker vars B,M)
   );
time I = Ext(k,M);
evenPart I
oddPart I
T = ZZ[t,u,Inverses=>true,MonomialOrder=>RevLex,Weights=>{-3,-1}];
poincareSeries2 = M -> (
   B := ring M;
   k := B/ideal vars B;
   P := Ext(M,k);
   h := hilbertSeries P;
   T':= degreesRing P;
   substitute(h, {T'_0=>t^-1,T'_1=>u^-1})
   );
poincareSeries1 = M -> (
   substitute(poincareSeries2 M, {u=>1_T})
   );
A' = K[x,y,z];
B' = A'/(x^2,y^2,z^3);
C' = res(B'^1/(x,y,z), LengthLimit => 6)
M' = coker transpose C'.dd_5
poincareSeries2 M'
p = poincareSeries1 M
load "simplify.m2"
simplify p
T' = QQ[t,Inverses=>true,MonomialOrder=>RevLex,Weights=>{-1}];
expansion = (n,q) -> (
    t := T'_0;
    rho := map(T',T,{t,1});
    num := rho value numerator q;
    den := rho value denominator q;
    n = n + first degree den;
    n = max(n, first degree num + 1);
    (num + t^n) // den
    );
expansion(20,p)
psi = map(K,B)
apply(10, i -> rank (psi ** Ext^i(M,coker vars B)))
use T;
complexity = M -> dim Ext(M,coker vars ring M);
complexity M
k = coker vars ring H;
prune Hom(k,H)
criticalDegree = M -> (
   B := ring M;
   k := B / ideal vars B;
   P := Ext(M,k);
   k  = coker vars ring P;
   - min ( first \ degrees source gens prune Hom(k,P))
   );
criticalDegree M
criticalDegree M'
supportVarietyIdeal = M -> (
   B := ring M;
   k := B/ideal vars B;
   ann Ext(M,k)
   );
K'' = ZZ/7;
A'' = K''[x,y,z];
J'' = ideal(x^7,y^7,z^7);
B'' = A''/J'';
scan((1,1) .. (3,3), (r,d) -> (
        V := cokernel random (B''^r,B''^{-d});
        << "------------------------------------------------------------------"
        << endl
        << "V = " << V << endl
        << "support variety ideal = "
        << timing supportVarietyIdeal V
        << endl))
bassSeries2 = M -> (
   B := ring M;
   k := B/ideal vars B;
   I := Ext(k,M);
   h := hilbertSeries I;
   T':= degreesRing I;
   substitute(h, {T'_0=>t^-1, T'_1=>u})
   );
bassSeries1 = M -> (
   substitute(bassSeries2 M, {u=>1_T})
   );
use B;
L = B^1/(x,y,z);
p = poincareSeries2 L
b = bassSeries2 L
b2 = bassSeries2 M
b1 = bassSeries1 M;
simplify b1
ext = (M,N) -> changeRing Ext(M,N);
use B;
N = B^1/(x^2 + z^2,y^3);
time rH = ext(M,N);
evenPart rH
oddPart rH
N' = B^1/(x^2 + z^2,y^3 - 2*z^3);
time rH' = ext(M,N');
evenPart rH'
oddPart rH'
extgenSeries2 = (M,N) -> (
   H := ext(M,N);
   h := hilbertSeries H;
   T':= degreesRing H;
   substitute(h, {T'_0=>t^-1,T'_1=>u^-1})
   );
extgenSeries1 = (M,N) -> (
   substitute(extgenSeries2(M,N), {u=>1_T})
   );
time extgenSeries2(M,N)
g=time extgenSeries1(M,N)
simplify g
time extgenSeries2(M,N')
g'=time extgenSeries1(M,N')
simplify g'
complexityPair = (M,N) -> dim ext(M,N);
time complexityPair(M,N)
time complexityPair(M,N')
supportVarietyPairIdeal = (M,N) -> ann ext(M,N);
time supportVarietyPairIdeal(M,N)
time supportVarietyPairIdeal(M,N')
