newPackage(
	"GenericPoints",
    	Version => "0.1", 
    	Date => "May 22, 2008",
    	Authors => {{Name => "Mike Stillman", 
		  Email => "mike@math.cornell.edu", 
		  HomePage => "http://www.math.cornell.edu/~mike/"}},
    	Headline => "a Macaulay2 package for finding ideals associated to generc points",
    	DebuggingMode => true
    	)

export {idealOfGenericPoint, columnNorms}

columnNorms = method()
columnNorms(Matrix) := A -> (
     ncols := rank source A;
     for i from 0 to ncols-1 list ((transpose A_{i})*A_{i})_(0,0)
     )

idealOfGenericPoint = method(TypicalValue => Ideal)
idealOfGenericPoint(Ring, List, ZZ, ZZ) := (R, L, d, M) -> (
     -- R is a polynomial ring over ZZ or QQ or RR or CC with #L variables
     -- L is a list of lists of elements of CC or RR.
     -- d is a degree bound
     -- Return value: an ideal in R of elements vanishing at the point L.
     --  all polys have degree <= d
     Z := ZZ[gens R];
--     Br := flatten entries sort basis(-infinity, d, R);
     Bz := flatten entries sort basis(-infinity, d, Z);
     a := #L; 
     B1 := matrix {apply(Bz, m -> floor(realPart (M*sub(m, matrix{L#0}))))} 
     || matrix {apply(Bz, m -> floor(imaginaryPart (M*sub(m, matrix{L#0}))))};
     for i from 1 to a-1 do (
	  B1 = B1 || matrix {apply(Bz, m -> floor(realPart (M*sub(m, matrix{L#i}))))}
	  || matrix {apply(Bz, m -> floor(imaginaryPart (M*sub(m, matrix{L#i}))))};
		    );
     G = id_(ZZ^#Bz) || B1;
--     G1 := sub(LLL G,R);
     G1z := LLL G;
--     G1r := sub(G1z,R);
     << columnNorms(G1z) << endl;
     B2z := matrix{Bz}|matrix{{2*a:0_Z}}; 
     (G1z, B2z) --, (B2 * G1_{0})_(0,0))
     )

beginDocumentation()
document { 
	Key => GenericPoints,
    	Headline => "a Macaulay2 package for finding ideals associated to generc points",	
	EM "FirstPackage", "a Macaulay2 package for finding ideals associated to generc points."
	}

document {
	Key => {(idealOfGenericPoint,Ring,List,ZZ,ZZ),idealOfGenericPoint},
	Headline => "vanishing ideal of a generic point",
	Usage => "idealOfGenericPoint(R,L,d,M)",
	Inputs => { "R", "L", "d" => " a degree bound", "M" => "the multiplier to use" },
	Outputs => {{ "eventually, an ideal" }},
	EXAMPLE lines ///
     	  R = ZZ[x];
	  r = sqrt(2.0p200) + sqrt(3.0p200) + sqrt(5.0p200)
	  idealOfGenericPoint(R,{r},8,10^50)
     	///
	}

TEST ///
     1 == 1
///

end
restart
loadPackage "GenericPoints"
rs = {sqrt(2.0p200) + sqrt(3.0p200) + sqrt(5.0p200),
     -sqrt(2.0p200) + sqrt(3.0p200) + sqrt(5.0p200),
     sqrt(2.0p200) - sqrt(3.0p200) + sqrt(5.0p200),
     -sqrt(2.0p200) - sqrt(3.0p200) + sqrt(5.0p200),
     sqrt(2.0p200) + sqrt(3.0p200) - sqrt(5.0p200),
     -sqrt(2.0p200) + sqrt(3.0p200) - sqrt(5.0p200),
     sqrt(2.0p200) - sqrt(3.0p200) - sqrt(5.0p200),
     -sqrt(2.0p200) - sqrt(3.0p200) - sqrt(5.0p200)}
sum rs
product rs
pi0 = numeric(200,pi)
idealOfGenericPoint(R,{pi0},10,10^70)
R = ZZ[x]
idealOfGenericPoint(R,{(2.0p200)^(1/5)},5,10^70)

restart 
path
path = append(path,"/Users/abo/M2/aim2009/aim2009/")
loadPackage "GenericPoints"
loadPackage "NumericalAlgebraicGeometry";


-- Example: twisted cubic

restart
loadPackage "GenericPoints"
nbits = 200;
rr = RR_nbits
R = rr[a,b,c]
p = numeric(nbits,pi)
q = log numeric_nbits 2 
G = idealOfGenericPoint(R,{{p,p^2,p^3},{q,q^2,q^3}},2,2^100)
ideal ((G#1)*(G#0)_{0..2})

-- Example: minimal polynomial 

R = QQ[x]
p = sqrt numeric 2
q = sqrt numeric 3
G = idealOfGenericPoint(R,{{p+q}},4,2^50)


-- Example: 3x3 minors of a 3x5 matrix in C^4

restart
loadPackage "GenericPoints"
debug loadPackage "NumericalAlgebraicGeometry"
U = ZZ/5[a..d,f];
A = random(U^{3:0},U^{5:-1})
S = RR[a..d,f]
I = minors(3,sub(A,S),Strategy => Cofactor);
J = sub(I,{f=>1});
KK = CC_500
R = KK[a..d] 
K = sub(J,R);
T = K_*;
D = 3;
RM = random(KK^D,KK^#T)
RT = flatten entries (RM*transpose matrix{T})
L = sum(gens R, v->v*random KK) + 1 
points = solveSystem(RT|{L}) 
pt = first select(points, pt -> norm (sub(K, matrix pt))_* < 10^(-1))
point = refinePHCpack((RT|{L}), pt, Iterations => 20, Bits => 500, ErrorTolerance => 1p500e-130)/first 
norm (sub(sub(ideal (RT|{L}),CC_500[gens R]),matrix point))_*
G = idealOfGenericPoint(R,point,3,2^300);
idl = ideal ((G#1)*(G#0)_{0..9})
sub(idl,R) == K

--norm (sub(sub(ideal (RT|{L}),R'),matrix pt))_*
apply(10, i->(
	  L' = sum(gens R, v->v*random CC) + 1;
	  track(RT|{L},RT|{L'},point)
	  ))
L = oo;
M = for i from 0 to #L-1 list ((L#i)#0)#0
G = idealOfGenericPoint(R,M,2,2^300);
idl = ideal ((G#1)*(G#0)_{0..9})
idl == K

-- Example: projection of a rational quintic

restart
loadPackage "GenericPoints"
nbits = 200;
rr = RR_nbits
R = rr[a..d]
p = numeric(nbits,pi)
G1 = idealOfGenericPoint(R,{{p,p^2,p^4,p^5}},2,2^100)
I = ideal ((G1#1)*(G1#0)_{0..3})
G2 = idealOfGenericPoint(R,{{p,p^2,p^4,p^5}},3,2^100)
J = trim ideal ((G2#1)*(G2#0)_{0..18})
S = rr[x,a..d,MonomialOrder=>Eliminate 1]
sub (selectInSubring(1,gens gb ideal (sub(vars R,S) - matrix{{x,x^2,x^4,x^5}})),R)
gens gb J

-- Example: Union of a line and a degree 5 curve. 

restart 
loadPackage "GenericPoints"
debug loadPackage "NumericalAlgebraicGeometry"
R = ZZ[a..d];
l' = ideal (2*a+3*b+5*c+7*d,a+2*b+3*c+4*d);
V' = ideal(gens l'*random(source gens l',R^{-2,-3}))
S = QQ[a..d]
V = sub(V',S);
l = sub(l',S);
C = V:l
W' = sub(V,{d=>1})
Q = CC[a..c]
W = sub(W',Q)
T = W_*
L = sum(gens Q, v->v*random CC) + 1 
points = solveSystem(T|{L}) 
apply(20, i->(
	  L' = sum(gens Q, v->v*random CC) + 1;
	  track(T|{L},T|{L'},points#0)
	  ))
N = oo;
M = for i from 0 to #N-1 list ((N#i)#0)#0;
G0 = idealOfGenericPoint(Q,M,3,2^50);
idl = ideal ((G0#1)*(G0#0)_{0..5})
sub(idl,S) == sub(sub(C,{d=>1}),S)

apply(10, i->(
	  L1 = sum(gens Q, v->v*random CC) + 1;
	  track(T|{L},T|{L1},points#1)
	  ))
N' = oo;
M'= for i from 0 to #N'-1 list ((N'#i)#0)#0
G1 = idealOfGenericPoint(Q,M',3,2^50);
J = ideal ((G1#1)*(G1#0)_{0..5})
J == W

-- Example: blow-up

restart 
loadPackage "GenericPoints"
nbits = 3000
R = RR_nbits[x,y];
p = numeric(nbits,pi)-3
q = sqrt(p^2*(p+1))
Q1 = 2*x^2+3*x*y+5*y^2+7*x+y
Q2 = 3*x^2+7*x*y+3*y^2+4*x+5*y
Q3 = 5*x^2+7*x*y+2*y^2+9*x+4*y 
Q4 = x^2+2*x*y+3*y^2+4*x+5*y 
L = {{sub(Q1,matrix{{p,q}}),sub(Q2,matrix{{p,q}}),sub(Q3,matrix{{p,q}}),sub(Q4,matrix{{p,q}})}}
L' = {{L#0#0/L#0#3,L#0#1/L#0#3,L#0#2/L#0#3}}
S = RR_nbits[a..c]
G = idealOfGenericPoint(S,L',3,2^2900);
I = ideal ((G#1)*(G#0)_{0..6})
T = QQ[a..d]
J = homogenize(sub(I,T),d)
hilbertPolynomial J


-- Example: Secant


restart 
loadPackage "GenericPoints"
debug loadPackage "NumericalAlgebraicGeometry"
rr = CC
R = rr[a,b,c]
p = numeric(pi)
q = log numeric 2 
pt1 = {0_CC,0_CC,0_CC,0_CC,p,p^2,p^3}
pt2 = {0_CC,q,q^2,q^3,0_CC,0_CC,0_CC}
pt3 = {1_CC,0_CC,0_CC,0_CC,p+q,p^2+q^2,p^3+q^3}
R = QQ[a..c];
L = minors (2,matrix{{a,b},{b,c},{c,1}});
T = CC[s,x_0..x_2,a..c]
num = # gens T
Ly = sub(L,T);
Lx = sub(L,matrix{{x_0..x_2}})
W  = ideal (sub(matrix{{2,3,5}},T)-s*matrix{{a..c}}-matrix{{x_0..x_2}})
W' = ideal (sub(matrix {{p+q,p^2+q^2,p^3+q^3}},T) - s*matrix{{a..c}}-matrix{{x_0..x_2}})
IT' = Ly + Lx + W
IS' = Ly + Lx + W'
ncols = rank source gens IT'
ran = random(T^num,T^ncols)
IT = flatten entries (ran*transpose gens IT')
IS = flatten entries (ran*transpose gens IS')
sols = {pt1,pt2,pt3}
track(IS,IT,sols)
pt1
