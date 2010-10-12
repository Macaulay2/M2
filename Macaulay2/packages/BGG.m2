needsPackage "BoijSoederberg"
newPackage(
	"BGG",
    	Version => "1.4", 
    	Date => "Oct 1, 2010",
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
     symExt, bgg, tateResolution, 
     beilinson, cohomologyTable, 
     directImageComplex, universalExtension, Regularity, 
     Exterior,
     pureResolution
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
     s := numgens source mr;
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
     mats := table(numgens target o, numgens source o,
              (r,c) -> (
                   rdeg := first rowdegs#r;
                   cdeg := first coldegs#c;
                   overS := beilinson1(o_(r,c),cdeg-rdeg,cdeg,S);
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


-------new 8/11/10 -- DE+MES
--degreeD takes the part of a free module, matrix or chain complex where
--all first components of the degrees of the generators of the
--modules involved are d.

degreeD = method()
degreeD(ZZ,Module) := (d,F) -> (
     -- assume, for now, that F is a free module
     if not isFreeModule F then error "required a free module";
     R := ring F;
     R^(-select(degrees F, e -> e#0 == d))
     )

degreeD(ZZ,Matrix) := (d,m) -> (
     tar := positions(degrees target m, e -> e#0 == d);
     src := positions(degrees source m, e -> e#0 == d);
     submatrix(m, tar, src)
     )

degreeD(ZZ,ChainComplex) := (d, F) -> (
     -- takes the first degree d part of F
     a := min F;
     b := max F;
     G := new ChainComplex;
     G.ring = ring F;
     for i from a to b do
	  G#i = degreeD(d, F#i);
     for i from a+1 to b do (
	  G.dd#i = map(G#(i-1), G#i, degreeD(d, F.dd_i));
	  );
     G
     )
degreeD(ZZ,ChainComplexMap) := (d, f) -> (
     -- takes the first degree d part of f : S --> T
     -- assumption: that min C === min D
     -- returning a new ChainComplexMap
     S := source f;
     T := target f;
     Sd := degreeD(d, S);
     Td := degreeD(d, T);
     a := min(min Sd, min Td);
     b := max(max Sd, max Td);
     map(Td, Sd, i -> (
	       -- need the degree d part of the map from Sd_i --> Td_i
	       degreeD(d, f_i)
	       ))
     )


symmetricToExteriorOverA=method()
symmetricToExteriorOverA(Matrix,Matrix,Matrix):= (m,e,x) -> (
--this function converts between a  presentation matrix m with 
--entries m^i_j of degree deg_x m^i_j = 0 or 1 only 
--of a module over a symmetric algebra A[x] and the linear part of the
--presentation map for the module 
--    P=ker (Hom_A(E,(coker m)_0) -> Hom_A(E,(coker m)_1))
--over the  exterior algebra A<e>.
--                                 Berkeley, 19/12/2004, Frank Schreyer.
     S:= ring x; E:=ring e;
     a:=rank source m;
     La:=degrees source m;
     co:=toList select(0..a-1,i->  (La_i)_0==0);
     M0:=coker substitute(m_co,vars E);
     M:=coker m;
     m1:=presentation (ideal x * M);
-- script uses the fact that the generators of ideal x* M are ordered
---as follows
-- x_0 generators of M,x_1*generators of M, ...
     b:=rank source m1;
     Lb:=degrees source m1;     
     cob:=toList select(0..b-1,i->  (Lb_i)_0==1);
     M1:=coker substitute(m1_cob,vars E);
     F:=substitute(id_(target m),vars E);
     G:=e_{0}**F;
     n:=rank source e -1;
     apply(n,j->G=G|(e_{j+1}**F)); -- (vars E)**F
     phi:=map(M1,M0,transpose G)
     )

symmetricToExteriorOverA(Module) := M -> (
     --M is a module over S = A[x0...].  must be gen in x-degree 0,
     --related in x-degree 1
     S := ring M;
     xvars := vars S;
     A := coefficientRing S;
     if not S.?Exterior then(
	  --S.Exterior = exterior alg over A on dual vars to the vars of S (new vars have deg = {-1,0})
	  S.Exterior = A[Variables => numgens S, SkewCommutative => true, Degrees=>{numgens S:-1}]
	  );
     E := S.Exterior;
     symmetricToExteriorOverA(presentation M, vars E, vars S)
     )

directImageComplex = method(Options => {Regularity=>null})
directImageComplex Module := opts -> (M) -> (
     S := ring M;
     regM := if opts.Regularity === null then regularityMultiGraded M
          else opts.Regularity;
     if regM < 0 then (M = truncate(0, M); regM = 0);
     degsM := degrees M/first;
     if max degsM > regM then error("regularity is higher than you think!");
     N := if min degsM === regM then M else image basis(regM,M);
     xm := regM * degree(S_0);
     phi := symmetricToExteriorOverA(N ** S^{xm});
     E := ring phi;
     F := complete res( image phi, LengthLimit => max(1,1+regM));
     F = E^{-xm} ** F[regM];
     F0 := degreeD(0, F);
     toA := map(coefficientRing E,E,DegreeMap=> i -> drop(i,1));
     --we should truncate away the terms that are 0, and (possibly) the terms above the (n+1)-st
     F0A := toA F0;
     G := new ChainComplex;
     G.ring = ring F0A;
     n := numgens ring M;
     for i from -n+1 to 1 do(
	  G.dd_i = F0A.dd_i);
     G
     )

RingMap ChainComplexMap := ChainComplexMap => (f,phi) -> map(f target phi, f source phi, i -> f phi_i)

directImageComplex Matrix := opts -> (f) -> (
     -- plan: compute both regularities
     --   if a value is given, then it should be the max of the 2 regularities
     -- 
     S := ring f;
     A := coefficientRing S;
     regMN := if opts.Regularity === null 
              then (
		   regM := regularityMultiGraded source f;
		   regN := regularityMultiGraded target f;
		   max(regM, regN))
	      else opts.Regularity;
     if regMN < 0 then (f = basis(0, f); regMN = 0);
     degsM := degrees (source f)/first;
     if max degsM > regMN then error("regularity is higher than you think!");
     degsN := degrees (target f)/first;
     if max degsN > regMN then error("regularity is higher than you think!");
     truncf := if min degsM === regMN and min degsN === regMN then f else (
        basis(regMN,f));
     M := source truncf;
     N := target truncf;
     StoA := map(A,S,DegreeMap => i -> drop(i,1));
     truncfA := StoA matrix truncf;
     xm := regMN * degree(S_0);
     phiM := symmetricToExteriorOverA(M ** S^{xm});
     phiN := symmetricToExteriorOverA(N ** S^{xm});
     E := ring phiM;
     FM := complete res( image phiM, LengthLimit => max(1,1+regMN));
     FN := complete res( image phiN, LengthLimit => max(1,1+regMN));
     fMN := extend(FN, FM, truncfA ** E);
     fMN = E^{-xm} ** fMN[regMN];
     FM = E^{-xm} ** FM[regMN];
     FN = E^{-xm} ** FN[regMN];
     fMN0 := degreeD(0, fMN);
     EtoA := map(A,E,DegreeMap=> i -> drop(i,1));
--     error("debug");
     EtoA fMN0
     )

{* -- we will probably remove this soon (9/30/2010 DE+MES)
truncateMultiGraded = method()
truncateMultiGraded (ZZ, Module) := (d,M) -> (
     --Assumes that M is a module over a polynomial ring S=A[x0..xn]
     --where the x_i have first-degree 1.
     --forms the submodule generated in x-degrees >= d.
     S := ring M;
     kk := ultimate (coefficientRing, S);
     S0 := kk[Variables => numgens S];
     f := map(S,S0, vars S);
     L := (degrees M)/first;
     Md := image M_{};
     scan(#L, i-> 
	  if L#i >= d then Md = Md + image M_{i} 
	  else Md = Md+((ideal f(basis(d-L#i, S0^1)))*(image M_{i})));
     Md
     )
*}

regularityMultiGraded = method()
regularityMultiGraded (Module) := (M) -> (
     S := ring M;
     (R,f) := flattenRing S;
     deglen := #degree R_0;
     w := flatten {1,toList(deglen-1:0)};
     regularity (coker f presentation M, Weights=>w)
     )

universalExtension = method()
universalExtension(List,List) := (La,Lb) -> (
     --Let Fi be the direct sum of line bundles on P1
     --F1 = O(a), F2 = O(b)
     --The script makes a module E representing the bundle that is
     --the universal extension of F2 by F1 on P^1; so the extension is
     --  0 --> F1 --> E --> F2 --> 0.
     --The answer is defined over A[y_0,y_1] representing 
     -- P^1 x Ext(Sheaf F2,Sheaf F1).
     -- The matrix obtained has relations in total degree {c,-1}
     --assumes the existence of
--     kk := ZZ/101;
--     A := kk[x_0..x_(2*d-2)];
--     S := A[y_0,y_1];
     kk := ZZ/101;
     la :=#La;
     lb :=#Lb;
     c := min La;
     N := sum(la, p-> sum(lb, q-> Lb#q-c-1));
     x := local x;
     y := local y;
     A := kk[x_0..x_(N-1)];
     S := A[y_0,y_1];

     m := bblock(Lb,c,S);
     n := ablock(La, Lb, S);
     coker (n||m)
   )
bblock = method()
bblock(ZZ,ZZ, Ring) := (b,c,S) -> map(S^{(b-c):{c+1,-1}}, S^{(b-c-1):{c,-1}}, 
	             (i,j)->
	       if i == j then S_0 else
	       if i == j+1 then S_1 else 0_S
	       )
	  
bblock(List, ZZ,Ring) := (Lb, c, S) ->directSum apply(#Lb, i->bblock(Lb#i, c, S))

ablock = method()
{*ablock(ZZ,ZZ,ZZ,ZZ,Ring) := (a,b,c,offset, S) -> 
     	  A:=coefficientRing S;
           map(S^{{a,1}}, S^{(b-c-1):{c,0}},
		(i,j) -> if j==0 then *S_1^(a-c) else 0_S)
*}

ablock(List, List, Ring) := (La,Lb,S) ->(
     --works over a tower ring S = A[S_0,S_1], where a
     --has at least #La*#Lb variables
     A:=coefficientRing S;
     c := min La;
     offset := 0;
     matrix apply(#La, p->apply(#Lb, q-> (
     	  	    mpq := map(S^{{La#p, 0}}, S^{(Lb#q-c-1):{c, -1}}, (i,j)->(
	       		      sub(A_(offset+j),S)*S_1^(La#p-c))); --was La#q-c
		    offset = offset+Lb#q-c-1;
		    mpq)
     		 ))
     )


pars = (k, mm) -> (
     --list of ways of writing k as a sum of #mm
     --non-negative numbers of which the i-th is <=mm_i
     ell := #mm;
     if k<0 or ell <1 then error("needs arguments k,ell with k>=0, ell >0");
     if k == 0 then return {toList(ell:0)};
     if ell == 1 and  k<= mm_(ell-1) then return {{k}};
     if ell == 1 and k>mm_(ell-1) then return{{}}; 
     P := {};
     for i from 0 to min(k,mm_0) do
         P = P | apply(
	      pars(k-i,drop(mm,1)), 
	               s->prepend(i,s)
		       );
    select(P,p->#p == #mm))


pureResolution = method()
pureResolution(Ring,List,List) := (A,p,e) -> (
     --n = number of variables = length of resolution
     --p = {p0,p1} where to make the two non linear steps
     -- with 0 < p0 < p1 < n
     --e = {e_0,e_1} jumps in the degrees at p0 and p1
     --(so F_(p_0) is gen in deg p_0+e_0, and
     --F_(p_1) is gen in deg p_0+e_0+e_1,
     --and the maps are of degrees e_0+1 and e_1+1
     n := numgens A;
     if #p!= 2 then error("second arg  must be a list of length 2");
     if #e!= 2 then error("third arg  must be a list of length 2");   
     if p_0<0 or p_1<0 or p_1< p_0 or p_1>= n or p_0>=n then 
         print("need 0<p_0<=p_1<numgens A to get a pure resolution");
     if e_0<0 or e_1 < 0 then error("e_i must be positive");
     kk := coefficientRing A;
     x := local x;
     S := A[x_(1,0)..x_(1,e_0)];
     B := kk[x_(0,0)..x_(0,n-1),x_(1,0)..x_(1,e_0)];
     use S;
     gotoS := map(S,B, sub((vars A),S)|matrix{{x_(1,0)..x_(1,e_0)}});
     T := B[x_(2,0)..x_(2,e_1)];

     M := n-1+e_0+e_1; 
     ell := 3;  -- number of sets of vars
     params := matrix{for k from 0 to M list(
	  P := pars(k,{n-1,e_0,e_1});
	  sum(P, p -> product(ell, i->sub(x_(i, p_i), T))
	  ))};
     len := n+e_0+e_1;
     kn := T^{{p_1+e_0-1,0}}**koszul(len,params);

     D := directImageComplex kn;
     DS := gotoS(D_(-e_1));
     pi1kn := map(S^{(rank target DS):{-len+1, -len+1}},
	  S^{(rank source DS):{-len, -len}}, DS);
     twistedpi1kn := pi1kn**S^{{p_0-1,0}};

     D2 := directImageComplex twistedpi1kn;

     (dual res coker dual D2_(-e_0))[-n]
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
     TT "dim HH^j F(i-j)", " for ", TT "h>=i>=l", ". As a simple example, we compute the dimensions of cohomology groups 
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
     In this form, the function is equivalent to the function ", TT "sheafCohomology", 
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
     }
doc ///
   Key 
     directImageComplex
     (directImageComplex, Module)
     [directImageComplex, Regularity]
   Headline
     Complex representing the direct image 
   Usage
     F = directImageComplex M
   Inputs 
     M: Module
       graded over a ring of the form S = A[y_0..y_n], representing a sheaf on ${\bf P}^n_A$,
       where A is a polynomial ring.
     Regularity=>ZZ
       the Castelnuovo-Mumford regularity of {\tt M}.  If not provided, this value will be computed
   Outputs
     F: ChainComplex
       complex of free modules over A. Homology in homological degree -i is 
       $R^i \pi_* {\mathcal M}$, where ${\mathcal M}$ is the sheaf on ${\bf P}^n_A$ represented by M.
   Description
    Text
      The computation is done using the exterior algebra method described by Eisenbud and Schreyer,
      in Eisenbud, David; Schreyer, Frank-Olaf 
      ``Relative Beilinson monad and direct image for families of coherent sheaves.'' 
      Trans. Amer. Math. Soc. 360 (2008), no. 10, 5367--5396.
      
      The computation requires knowing the Castelnuovo-Mumford regularity of M 
      in the variables y_i. If not provided by the user,
      it is computed by the function. The default is Regularity => null, 
      which means it must be computed.
      
      The ring A must be a polynomial ring. For the moment, the module M must be homogeneous
      for the variables of A as well as for the variables of S (bihomogeneous).
      
      It is proven in loc. cit. that every complex of free modules can be realized
      as the direct image of a vector bundle on ${\bf P}^n_A$.
      
      The following example can be used to study the loci in the family of extensions
      of a pair of vector bundles on $\bf P^1$ where the extension bundle has a given 
      splitting type: this type is calculated by the Fitting ideals of the
      matrices defining the direct image complexes of various twists of the bundle.
      See Section 5 in loc. cite. It is conjectured there that all the sums of
      these Fitting ideals for the universal extension of 
      $\mathcal O_{P^1}^{r-1}$ by $\mathcal O_{P^1}(d)$ are radical, 
      as in the example below.
      
      First we examine the extensions of ${\mathcal O}_{P^1}(1)$ by ${\mathcal O}_{P^1}(-3)$.
      There is a 3-dimensional vector space 
      $$
      Ext^1({\mathcal O}_{P^1}(1),{\mathcal O}_{P^1}(-3))
      $$
      of extensions. The ``universal extension'' is thus a bundle on
      ${\bf P^1}\times Ext$. The locus where the extension
      bundle splits as ${\mathcal O}_{P^1}(-2) \oplus {\mathcal O}_{P^1}$ is the
      locus where the map in the direct image complex drops rank, and this is
      the (cone over a) conic, defined by the determinant of this matrix.
    Example
      M=universalExtension({-3}, {1})
      S = ring M;
      A = coefficientRing S;
      F = directImageComplex M
      F.dd_0
      det (F.dd_0)
    Text
      Here is a larger example, the extension of 
      ${\mathcal O}_{P^1}^2$ by ${\mathcal O}_{P^1}(6)$
    Example
      r=3;
      d=6;
      M=universalExtension(splice {(r-1):0}, {d})
      S = ring M;
      A = coefficientRing S;
      L = for i from -d to 0 list directImageComplex(S^{{i,0}}**M);
      netList L
      maps = apply(L, F-> F.dd_0)
   SeeAlso 
     universalExtension
     (directImageComplex, Matrix)
///

doc ///
   Key
     (directImageComplex, Matrix)
     pureResolution
     (pureResolution, Ring, List, List)
   Headline
     map of direct image complexes
   Usage
     piF = directImageComplex F
   Inputs
     F:Matrix
       a homomorphism $F : M \rightarrow N$ of graded $S = A[y_0..y_n]$ modules, graded of degree 0,
       where we think of $M$ and $N$ as representing sheaves on ${\bf P}^n_A$
   Outputs
     piF:ChainComplexMap
       the induced map on chain complexes {\tt piF : directImageComplex M --> directImageComplex N}
   Description
    Text
     We give an application of this function to create pure free resolutions.
     
     A "pure free resolution of type (d_0,d_1,..,d_n)" is a resolution of a graded Cohen-Macaulay
     module M over a polynomial ring such that for each 
     i = 1,..,n, the module of i-th syzygies of M is generated by
     syzygies of degree d_i. Eisenbud and Schreyer constructed
     such free resolutions in all characteristics and for all
     degree sequences $d_0 < d_1 < \cdots < d_n$ by pushing forward
     appropriate twists of a Koszul complex. (The construction
     was known for the Eagon-Northcott complex since work of Kempf). 
     
     If one of the 
     differences $d_{i+1} - d_i$ is equal to 1, then it turns out that one of
     the maps in the pure resolution is part of the map of complexes
     directImageComplex k_j, where k_j is a map in this 
     Koszul complex. Here is a simple example, where we produce
     one of the complexes in the family that included the
     Eagon-Northcott complex (see for example "Commutative Algebra
     with a View toward Algebraic Geometry", by D. Eisenbud.)
    Example
     kk = ZZ/101
     A = kk[u,v,w]
     T = A[x,y]
     params = matrix"ux,uy+vx,vy+wx,wy"
     kn = koszul(4,params)
     D = directImageComplex kn
    Text
     The direct image complexes each have only one nonzero
     term,and so D has only one nonzero component. According
     to Eisenbud and Schreyer, this is the last map in a pure
     resolution. Since the dual of a pure resolution is again
     a resolution, we can simply take the dual of this map
     and resolve to see the dual of the resolution (or dualize
     again to see the resolution itself, which is the Eagon-Northcott
     complex itself in this case.
    Example
     m = transpose D_(-1)
     betti res coker m
     (dual oo)[-3]
    Text
     If we twist the map kn a little before taking the direct image
     we get the last map in a different complex:
    Example
     D1 = directImageComplex (T^{{1,0}}**kn)
     betti res coker transpose (D1_(-1))
    Text
     For more complex examples, we make a script, {\tt pureResolution}, which
     we have included in this package. In the following
     we specify a ground ring A, and two places p={p_0,p_1} at
     which we want to control the degree of the maps in our
     resolution (0<p_0<p_1<numgens A). We also specify
     e = {e_0,e_1}, where we want 1+e_i to be the degree of the
     p_i-th map in the resolution.
    Example
     A = kk[a,b,c,d]
     betti (P=pureResolution(A,{1,3},{2,1}))
     betti (Q=pureResolution(A,{1,1},{3,0}))
   Caveat
     Currently, the direct image complexes of the source and target are recomputed, not stashed anywhere.
     The script pureResolution should be extended to provide all pure
     resolutions. For this one would need the direct image of a complex;
     and this can be computed with the same tools as directImageComplex Module
     uses. This should be added in a later version.
   SeeAlso
     directImageComplex
     universalExtension
///

doc ///
   Key 
     universalExtension
   Headline
     Universal extension of vector bundles on P^1
   Usage
     E = universalExtension(La, Lb)
   Inputs
     La: List 
       of integers
     Lb: List 
       of integers
   Outputs 
     E: Module 
       representing the extension
   Description
    Text
     Every vector bundle E on ${\mathbb P}^1$ splits as a sum of line bundles 
     OO(a_i). If La is a list of integers, we write E(La) for the direct sum of the
     line bundle OO(La_i).  Given two such bundles specified by the lists
     La and Lb this script constructs a module representing the universal
     extension of E(Lb) by E(La). It is defined on the product variety
     Ext^1(E(La), E(Lb)) x ${\mathbb P}^1$, and represented here by
     a graded module over the coordinate ring S = A[y_0,y_1] of this variety;
     here A is the coordinate ring of Ext^1(E(La), E(Lb)), which is a polynomial
     ring.
    Example
     M = universalExtension({-2}, {2})
     M = universalExtension({-2,-3}, {2,3})
    Text
     It is interesting to consider the loci in Ext where the extension has
     a particular splitting type. See the documentation for directImageComplex
     for a conjecture about the equations of these varieties.
   SeeAlso
    directImageComplex
///



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
///


TEST ///
  R = ZZ/101[x,y]
  M = R^{4}
  C = directImageComplex M
  assert(C^1 == 0)
  assert(rank C^0 == 5)
  assert(C^-1 == 0)
///

TEST ///
  R = ZZ/101[x,y]
  M = R^{-4}
  C = directImageComplex M
  assert(rank C^1 == 3)
  assert(C^0 == 0)
  assert(C^-1 == 0)
///

TEST ///
  R = ZZ/101[x,y]
  M = R^{0}
  C = directImageComplex M
  assert(C^1 == 0)
  assert(rank C^0 == 1)
  assert(C^-1 == 0)
///

TEST ///
  needsPackage "ChainComplexExtras"
  kk = ZZ/101
  A = kk[a]
  S = A[x,y]
  M1 = S^{{-1,0},{0,0}}
  C1 = directImageComplex(id_M1)
  assert(C1_0 == 1)
  assert isChainComplexMap C1

  M2 = S^{{-1,0}}
  C2 = directImageComplex(id_M2)
  assert isChainComplexMap C2
  assert(C2_0 == 0)

  M3 = S^{{-2,0}}
  C3 = directImageComplex(id_M3)
  assert isChainComplexMap C3
  assert(C3_0 == 0)
///

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

restart
path = prepend( "/Users/david/src/Colorado-2010/PushForward",path)
uninstallPackage "BGG"
installPackage "BGG"
check BGG
viewHelp BGG

{*    
    Example
      A = ZZ/101[a,b]
      B = A[c,d]
      C = first flattenRing B
      F = random(C^{{-2,1},{1,1}}, C^{{-3,0},{-3,0}})
      F = sub(F, B)
      isHomogeneous F
      C1 = directImageComplex source F
      C2 = directImageComplex target F
      piF = directImageComplex F
*}
