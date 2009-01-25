needsPackage "BoijSoederberg"
newPackage(
	"BGG",
    	Version => "0.2", 
    	Date => "June 30, 2008",
    	Authors => {
	     {Name => "Hirotachi Abo", Email => "abo@uidaho.edu", HomePage => "http://www.webpages.uidaho.edu/~abo/"},
	     {Name => "Wolfram Decker", Email => "decker@math.uni-sb.de", HomePage => "http://www.math.uni-sb.de/ag/decker/"},
	     {Name => "David Eisenbud", Email => "de@msri.org", HomePage => "http://www.msri.org/~de/"},	     
	     {Name => "Frank Schreyer", Email => "schreyer@math.uni-sb.de", HomePage => "http://www.math.uni-sb.de/ag/schreyer/"},
	     {Name => "Gregory G. Smith", Email => "ggsmith@mast.queensu.ca", HomePage => "http://www.mast.queensu.ca/~ggsmith/"},
	     {Name => "Mike Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike/"}
	     },
    	Headline => "Bernstein-Gelfand-Gelfand correspondence",
    	DebuggingMode => true
    	)
needsPackage "BoijSoederberg"

export {
     symExt, bgg, tateResolution, beilinson, cohomologyTable
     }

symExt = method()
symExt(Matrix, PolynomialRing) := Matrix => (m,E) ->(
     ev := map(E,ring m,vars E);
     mt := transpose jacobian m;
     jn := gens kernel mt;
     q  := vars(ring m)**id_(target m);
     ans:= transpose ev(q*jn);
     --now correct the degrees:
     map(E^{(rank target ans):1}, E^{(rank source ans):0}, 
         ans));

bgg = method()
bgg(ZZ,Module,PolynomialRing) := Matrix => (i,M,E) ->(
     S :=ring(M);
     numvarsE := rank source vars E;
     ev := map(E,S,vars E);
     f0 := basis(i,M);
     f1 := basis(i+1,M);
     g := ((vars S)**f0)//f1;
     b := (ev g)*((transpose vars E)**(ev source f0));
     --correct the degrees (which are otherwise wrong in the transpose)
     map(E^{(rank target b):i+1},E^{(rank source b):i}, b));

tateResolution = method(TypicalValue => ChainComplex)
tateResolution(Matrix, PolynomialRing, ZZ, ZZ) := ChainComplex => (m,E,loDeg,hiDeg)->(
     M := coker m;
     reg := regularity M;
     bnd := max(reg+1,hiDeg-1);
     mt  := presentation truncate(bnd,M);
     o   := symExt(mt,E);
     --adjust degrees, since symExt forgets them
     ofixed   :=  map(E^{(rank target o):bnd+1},
                E^{(rank source o):bnd},
                o);
     res(coker ofixed, LengthLimit=>max(1,bnd-loDeg+1)));

sortedBasis = (i,E) -> (
     m := basis(i,E);
     p := sortColumns(m,MonomialOrder=>Descending);
     m_p);

beilinson1=(e,dege,i,S)->(
     E := ring e;
     mi := if i < 0 or i >= numgens E then map(E^1, E^0, 0)
           else if i === 0 then id_(E^1)
           else sortedBasis(i+1,E);
     r := i - dege;
     mr := if r < 0 or r >= numgens E then map(E^1, E^0, 0)
           else sortedBasis(r+1,E);
     s = numgens source mr;
     if i === 0 and r === 0 then
          substitute(map(E^1,E^1,{{e}}),S)
     else if i>0 and r === i then substitute(e*id_(E^s),S)
     else if i > 0 and r === 0 then
          (vars S) * substitute(contract(diff(e,mi),transpose mr),S)
     else substitute(contract(diff(e,mi), transpose mr),S));

UU = (i,S) -> (
     if i < 0 or i >= numgens S then S^0
     else if i === 0 then S^1
     else cokernel koszul(i+2,vars S) ** S^{i});

beilinson = method()
beilinson(Matrix,PolynomialRing) := Matrix => (o,S) -> (
     coldegs := degrees source o;
     rowdegs := degrees target o;
     mats = table(numgens target o, numgens source o,
              (r,c) -> (
                   rdeg = first rowdegs#r;
                   cdeg = first coldegs#c;
                   overS = beilinson1(o_(r,c),cdeg-rdeg,cdeg,S);
                   -- overS = substitute(overE,S);
                   map(UU(rdeg,S),UU(cdeg,S),overS)));
     if #mats === 0 then matrix(S,{{}})
     else matrix(mats));


tateToCohomTable = (T) -> (
     C := betti T;
     n := max apply(keys C, (i,d,d1) -> i-d1);
     new CohomologyTally from apply(keys C, (i,d,d1) -> (
	       ((n-(i-d1), -d1), C#(i,d,d1))
	       ))
     )

cohomologyTable = method()
cohomologyTable(CoherentSheaf, ZZ, ZZ) := CohomologyTally => (F,lo,hi) -> (
     M := module F;
     S := ring M;
     E := (coefficientRing S)[Variables=>numgens S, SkewCommutative=>true];
     T := tateResolution(presentation M, E, lo, hi);
     tateToCohomTable T
     )
cohomologyTable(Matrix, PolynomialRing, ZZ, ZZ) := CohomologyTally => (m,E,lo,hi) -> (
     T := tateResolution(m, E, lo, hi);
     tateToCohomTable T
     )


beginDocumentation()

document {
     Key => BGG, 
     Headline => "Bernstein-Gel'fand-Gel'fand correspondence", 
     "The Bernstein-Gel'fand-Gel'fand correspondence is an isomorphism between the derived category of 
     bounded complexes of finitely generated modules over a polynomial ring and the derived category of 
     bounded complexes of finitely generated module over an exterior algebra (or of certain Tate resolutions). 
     This package implements routines for investigating the BGG correspondence.", 
     PARA {}, 
     "More details can be found in ",  
     HREF("http://www.math.uiuc.edu/Macaulay2/Book/", "Sheaf Algorithms Using Exterior Algebra"), ".", 
     } 
     
document { 
     Key => {symExt,(symExt,Matrix,PolynomialRing)}, 
     Headline => "the first differential of the complex R(M)",
     Usage => "symExt(m,E)",
     Inputs => {
	  "m" => Matrix => "a presentation matrix for a positively graded module M over a polynomial ring",
	  "E" => PolynomialRing => "exterior algebra"
	  },
     Outputs => {
	  Matrix => {"a matrix representing the map ",  TT "M_1 ** omega_E <-- M_0 ** omega_E"}  
	  },
     "This function takes as input a matrix ", TT "m", " with linear entries, which we think of as 
     a presentation matrix for a positively graded ", TT "S", "-module ", TT "M", " matrix representing 
     the map " , TT "M_1 ** omega_E <-- M_0 ** omega_E", " which is the first differential of 
     the complex ", TT "R(M)",    ".", 
     EXAMPLE lines ///
	  S = ZZ/32003[x_0..x_2]; 
	  E = ZZ/32003[e_0..e_2, SkewCommutative=>true];
	  M = coker matrix {{x_0^2, x_1^2}};
	  m = presentation truncate(regularity M,M);
	  symExt(m,E)
     	  ///,
     Caveat => "This function is a quick-and-dirty tool which requires little computation. However 
     if it is called on two successive truncations of a module, then the maps it produces may NOT 
     compose to zero because the choice of bases is not consistent.",   
     SeeAlso => {bgg}
     }

document { 
     Key => {bgg,(bgg,ZZ,Module,PolynomialRing)}, 
     Headline => "the ith differential of the complex R(M)",
     Usage => "bgg(i,M,E)",
     Inputs => {
	  "i" => ZZ => "the cohomological index",
	  "M" => Module => {"graded ", TT "S", "-module"},  
	  "E" => PolynomialRing => "exterior algebra"
	  },
     Outputs => {
	  Matrix => {"a matrix representing the ith differential"}  
	  },
     "This function takes as input an integer ", TT "i", " and a finitely generated graded ", TT "S", 
     "-module ", TT "M", ", and returns the ith map in ", TT "R(M)", ", which is an adjoint 
     of the multiplication map between ", TT "M_i", " and ", TT "M_{i+1}", ".",    
     EXAMPLE lines ///
	  S = ZZ/32003[x_0..x_2]; 
	  E = ZZ/32003[e_0..e_2, SkewCommutative=>true];
	  M = coker matrix {{x_0^2, x_1^2, x_2^2}};
	  bgg(1,M,E)
	  bgg(2,M,E)
     	  ///,
     SeeAlso => {symExt}
     }

document { 
     Key => {tateResolution,(tateResolution, Matrix,PolynomialRing,ZZ,ZZ)}, 
     Headline => "finite piece of the Tate resolution",
     Usage => "tateResolution(m,E,l,h)",
     Inputs => {
	  "m" => Matrix => "a presentation matrix for a module",
	  "E" => PolynomialRing => "exterior algebra",
	  "l" => ZZ => "lower cohomological degree", 
	  "h" => ZZ => "upper bound on the cohomological degree"
	  },
     Outputs => {
	  ChainComplex => {"a finite piece of the Tate resolution"}  
	  },
     "This function takes as input a presentation matrix ", TT "m", " of a finitely generated graded "
     , TT "S", "-module ", TT "M", " an exterior algebra ", TT "E", " and two integers ", TT "l", 
     " and ", TT "h", ". If ", TT "r", " is the regularity of ", TT "M", ", then this function 
     computes the piece of the Tate resolution from cohomological degree ", TT "l", 
     " to cohomological degree ", TT "max(r+2,h)", ". For instance, for the homogeneous 
     coordinate ring of a point in the projective plane:",  
     EXAMPLE lines ///
	  S = ZZ/32003[x_0..x_2]; 
	  E = ZZ/32003[e_0..e_2, SkewCommutative=>true];
	  m = matrix{{x_0,x_1}};
	  regularity coker m
	  T = tateResolution(m,E,-2,4)
	  betti T
	  T.dd_1
     	  ///,
     SeeAlso => {symExt}
     }

document { 
     Key => {cohomologyTable,(cohomologyTable,CoherentSheaf,ZZ,ZZ), (cohomologyTable,Matrix,PolynomialRing,ZZ,ZZ)}, 
     Headline => "dimensions of cohomology groups",
     Usage => "cohomologyTable(F,l,h) or cohomologyTable(m,E,l,h)",
     Inputs => {
	  "F" => CoherentSheaf => "a coherent sheaf on a projective scheme", 
  	  "l" => ZZ => "lower cohomological degree", 
	  "h" => ZZ => "upper bound on the cohomological degree",
	  "m" => Matrix => "a presentation matrix for a module",
	  "E" => PolynomialRing => "exterior algebra"
	   },
     Outputs => {
	  {TO "BoijSoederberg::CohomologyTally", " dimensions of cohomology groups"}  
	  },
     "
     This function takes as input a coherent sheaf ", TT "F", ", two integers ", TT "l", 
     " and ", TT "h", ", and prints the dimension ", 
     TT "dim HH^j F(i-j)", " for ", TT "h>=i>=l", ". As a simple example, we comput the dimensions of cohomology groups 
     of the projective plane.",   
     EXAMPLE lines ///
	  S = ZZ/32003[x_0..x_2]; 
	  PP2 = Proj S; 
	  F =sheaf S^1
          cohomologyTable(F,-10,5)
     	  ///,
     "There is also a built-in sheaf cohomology function ", TO2((cohomology, ZZ, CoherentSheaf), "HH"), " in Macaulay2. 
     However, these algorithms are much slower than ", TT "cohomologyTable", ".",   
     PARA {}, 
     "Alternatively, this function takes as input      
     a presentation matrix ", TT "m", " of a finitely generated graded "
     , TT "S", "-module ", TT "M", "and an exterior algebra ", TT "E", "with the same number of variables. 
     In this form, the function is equivalent to the fucntion ", TT "sheafCohomology", 
     " in ", HREF("http://www.math.uiuc.edu/Macaulay2/Book/", "Sheaf Algorithms Using Exterior Algebra"),  ".",
     EXAMPLE lines ///
	  S = ZZ/32003[x_0..x_2]; 
	  E = ZZ/32003[e_0..e_2, SkewCommutative=>true];
	  m  = koszul (3, vars S); 
	  regularity coker m 
	  betti tateResolution(m,E,-6,2)
          cohomologyTable(m,E,-6,2)
     	  ///,
     PARA {}, 
     "As the third example, we compute the dimensions of cohomology groups of the structure sheaf of an 
     irregular elliptic surface.", 
     EXAMPLE lines /// 
          S = ZZ/32003[x_0..x_4]; 
	  X = Proj S; 
          ff = res coker map(S^{1:0},S^{3:-1,2:-2},{{x_0..x_2,x_3^2,x_4^2}}); 
	  alpha = map(S^{1:-2},target ff.dd_3,{{1,4:0,x_0,2:0,x_1,0}})*ff.dd_3; 
	  beta = ff.dd_4//syz alpha; 
	  K = syz syz alpha|beta;
	  fK = res prune coker K;
	  s = random(target fK.dd_1,S^{1:-4,3:-5});
	  ftphi = res prune coker transpose (fK.dd_1|s);
	  I = ideal ftphi.dd_2;
	  F = sheaf S^1/I; 
	  cohomologyTable(F,-2,6)
     ///, 	  
     SeeAlso => {symExt, tateResolution}
     }

document { 
     Key => {beilinson,(beilinson, Matrix,PolynomialRing)}, 
     Headline => "Vector bundle map associated to the Beilinson monad",
     Usage => "beilinson(m,S)",
     Inputs => {
	  "m" => Matrix => {"a presentation matrix for a module over an exterior algebra ", TT "E"},
	  "S" => PolynomialRing => {"polynomial ring with the same number of variables as ", TT "E"},
	  },
     Outputs => {
	  Matrix => {"vector bundle map"}  
	  },
     "The BGG correspondence is an equivalence between complexes of modules over exterior algebras and complexes 
     of coherent sheaves over projective spaces. This function takes as input a map between two free ", TT "E", "-modules, 
     and returns the associate map between direct sums of exterior powers of cotangent bundles. In particular, it is useful 
     to construct the Belinson monad for a coherent sheaf.",  
     EXAMPLE lines ///
	  S = ZZ/32003[x_0..x_2]; 
	  E = ZZ/32003[e_0..e_2, SkewCommutative=>true];
	  alphad = map(E^1,E^{2:-1},{{e_1,e_2}});
	  alpha = map(E^{2:-1},E^{1:-2},{{e_1},{e_2}});
	  alphad' = beilinson(alphad,S)
	  alpha' = beilinson(alpha,S)
	  F = prune homology(alphad',alpha')
	  betti F
	  cohomologyTable(presentation F,E,-2,3)
     	  ///,
     "As the next example, we construct the monad of the Horrock-Mumford bundle:", 
      	  EXAMPLE lines ///
	  S = ZZ/32003[x_0..x_4]; 
	  E = ZZ/32003[e_0..e_4, SkewCommutative=>true];
	  alphad = map(E^5,E^{2:-2},{{e_4*e_1,e_2*e_3},{e_0*e_2,e_3*e_4},{e_1*e_3,e_4*e_0},{e_2*e_4,e_0*e_1},{e_3*e_0,e_1*e_2}})
	  alpha = syz alphad
	  alphad' = beilinson(alphad,S)
	  alpha' = beilinson(alpha,S)
	  F = prune homology(alphad',alpha');
	  betti res F
	  regularity F
	  cohomologyTable(presentation F,E,-6,6)
     	  ///,
     SeeAlso => {symExt}
     }, 

TEST ///
          S = ZZ/32003[x_0..x_2]; 
	  E = ZZ/32003[e_0..e_2, SkewCommutative=>true];
	  M = coker matrix {{x_0^2, x_1^2}};
	  m = presentation truncate(regularity M,M);
	  assert(symExt(m,E)==map(E^{4:1},E^4,{{e_2,e_1,e_0,0},{0,e_2,0,e_0},{0,0,e_2,e_1},{3:0,e_2}}))
///
TEST ///
          S = ZZ/32003[x_0..x_2]; 
	  E = ZZ/32003[e_0..e_2, SkewCommutative=>true];
	  M = coker matrix {{x_0^2, x_1^2, x_2^2}};
	  assert(bgg(1,M,E)==map(E^{3:2},,{{e_1, e_0, 0}, {e_2, 0, e_0}, {0, e_2, e_1}}))
///
TEST ///	  
	  S = ZZ/32003[x_0..x_2]; 
	  E = ZZ/32003[e_0..e_2, SkewCommutative=>true];
	  m = matrix{{x_0,x_1}};
	  regularity coker m
	  T = tateResolution(m,E,-2,4)
          assert(toList(7:1) == apply(length T+1, i-> rank T#i))
	  assert(all(toList(1..length T), i-> (matrix entries T.dd_i)==matrix {{e_2}}))
///
TEST /// 
	  S = ZZ/32003[x_0..x_2]; 
	  PP2 = Proj S; 
	  F =sheaf S^1
          C = cohomologyTable(F,-10,5)
	  assert(values C == apply(keys C, p -> rank HH^(p#0)(F(p#1))))
///
TEST /// 	  
	  S = ZZ/32003[x_0..x_2]; 
	  E = ZZ/32003[e_0..e_2, SkewCommutative=>true];
	  alphad = map(E^1,E^{2:-1},{{e_1,e_2}})
          assert(matrix entries beilinson(alphad,S) == matrix {{x_0, 0, -x_2, 0, x_0, x_1}})
          alpha = map(E^{2:-1},E^{1:-2},{{e_1},{e_2}});
	  assert(matrix entries beilinson(alpha,S) ==  map(S^6,S^1,{{0}, {-1}, {0}, {1}, {0}, {0}}))
///,
end
uninstallPackage "BGG"
restart
path = append(path, homeDirectory | "Snowbird/")
installPackage("BGG", UserMode => true) 

restart
loadPackage "BGG0"
loadPackage "BoijSoderberg"
loadPackage "SchurFunctors"

kk = ZZ/32003
S = kk[x_0..x_2]
E = kk[e_0..e_2, SkewCommutative=>true]

M = ker vars S
N = coker schur({2,1},syz vars S);
betti res N
poincare N
apply(-10..10, d -> hilbertFunction(d,N))
res N
betti oo
tateResolution(presentation N, E, -5, 5)
betti oo
F = sheaf N
cohomologyTable(F, -5, 5)

HH^0(F)
HH^0(F(1))
HH^0(F(2))
HH^0(F(3))
HH^0(F(4))

HH^1(F)
HH^1(F(1))
HH^1(F(2))
HH^1(F(3))
HH^1(F(4))

HH^2(F(-2))
HH^2(F(-1))
HH^2(F)
HH^2(F(1))
HH^2(F(2))
HH^2(F(3))
HH^2(F(4))


hilbertPolynomial(F, Projective=>false)
factor oo
3 * pureCohomologyTable({2,0},-5,5)

cohomologyTable(CoherentSheaf, ZZ, ZZ) := (F,lo,hi) -> (
     -- this is the stoopid version
     n := numgens ring F - 1;
     new CohomologyTally from flatten (
     for i from 0 to n list
       for d from lo-i to hi-i list ((i,d) => rank HH^i(F(d)))
     ))

tateToCohomTable = (T) -> (
     C := betti T;
     n := max apply(keys C, (i,d,d1) -> i-d1);
     new CohomologyTally from apply(keys C, (i,d,d1) -> (
	       (n-(i-d1), -d1)
	       ))
     )
time cohomologyTable(F,-5,5)

betti tateResolution(presentation N, E, -5, 5)
peek oo
