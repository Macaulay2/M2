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

export {idealOfGenericPoint}

idealOfGenericPoint = method(TypicalValue => Ideal)
idealOfGenericPoint(Ring, List, ZZ, ZZ) := (R, L, d, M) -> (
     -- R is a polynomial ring over ZZ or QQ or RR or CC with #L variables
     -- L is a list of elements of CC or RR.
     -- d is a degree bound
     -- Return value: an ideal in R of elements vanishing at the point L.
     --  all polys have degree <= d
     B := flatten entries sort basis(-infinity, d, R);
     B1 := apply(B, m -> floor(M*sub(m, matrix{L})));
     G = id_(ZZ^#B) || matrix{B1};
     G1 := LLL G;
     << "LLL matrix is " << G1 << endl;
     B2 := matrix{B}|matrix{{0_R}};
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
