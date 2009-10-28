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
     B := flatten entries sort basis(-infinity, d, R);
     a := #L; 
     B1 := matrix {apply(B, m -> floor(realPart (M*sub(m, matrix{L#0}))))} 
     || matrix {apply(B, m -> floor(imaginaryPart (M*sub(m, matrix{L#0}))))};
     for i from 1 to a-1 do (
	  B1 = B1 || matrix {apply(B, m -> floor(realPart (M*sub(m, matrix{L#i}))))}
	  || matrix {apply(B, m -> floor(imaginaryPart (M*sub(m, matrix{L#i}))))};
		    );
--     B1 := apply(B, m -> floor(M*sub(m, matrix{L})));
     G = id_(ZZ^#B) || B1;
     G1 := sub(LLL G,R);
--     << "LLL matrix is " << G1 << endl;
     << columnNorms(G1) << endl;
     B2 := matrix{B}|matrix{{2*a:0_R}};
     (G1, B2, (B2 * G1_{0})_(0,0))
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
idealOfGenericPoint(R,{{p,p^2,p^3},{q,q^2,q^3}},2,2^100)

-- Example: minimal polynomial 

R = QQ[x]
p = sqrt numeric 2
q = sqrt numeric 3
G = idealOfGenericPoint(R,{{p+q}},4,2^50)


-- Example: 

restart
loadPackage "GenericPoints"
loadPackage "NumericalAlgebraicGeometry"
U = ZZ/5[a..e];
A = random(U^{3:0},U^{5:-1})
S = RR[a..e]
I = minors(3,sub(A,S),Strategy => Cofactor);
J = sub(I,{e=>1});
R = CC[a..d] 
K = sub(J,R);
T = K_*;
d = 3;
RM = random(CC^d,CC^#T)
RT = flatten entries (RM*transpose matrix{T})
L = sum(gens R, v->v*random CC) + 1 
points = solveSystem(RT|{L}) 
point = points#2
for i from 0 to #points-1 list norm (sub(K, matrix points#i))_*
apply(10, i->(
	  L' = sum(gens R, v->v*random CC) + 1;
	  track(RT|{L},RT|{L'},point)
	  ))
L = oo;
M = for i from 0 to #L-1 list ((L#i)#0)#0
idealOfGenericPoint(R,M,3,2^50);


