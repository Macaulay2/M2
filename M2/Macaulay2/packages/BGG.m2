newPackage(
	"BGG",
    	Version => "1.4.2", 
    	Date => "Jan 11, 2016",
    	Authors => {
	     {Name => "Hirotachi Abo", Email => "abo@uidaho.edu", HomePage => "http://www.webpages.uidaho.edu/~abo/"},
	     {Name => "Wolfram Decker", Email => "decker@math.uni-sb.de", HomePage => "http://www.math.uni-sb.de/ag/decker/"},
	     {Name => "David Eisenbud", Email => "de@msri.org", HomePage => "http://www.msri.org/~de/"},	     
	     {Name => "Frank-Olaf Schreyer", Email => "schreyer@math.uni-sb.de", HomePage => "http://www.math.uni-sb.de/ag/schreyer/"},
	     {Name => "Gregory G. Smith", Email => "ggsmith@mast.queensu.ca", HomePage => "http://www.mast.queensu.ca/~ggsmith/"},
	     {Name => "Mike Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike/"}
	     },
    	Headline => "Bernstein-Gelfand-Gelfand correspondence",
	Keywords => {"Commutative Algebra"},
	PackageExports => {"BoijSoederberg", "Complexes"},
	PackageImports => {"Truncations","Varieties"},
    	DebuggingMode => false
    	)

export {
     "symExt", "bgg", "tateResolution", 
     "beilinson", "cohomologyTable", 
     "directImageComplex", 
     "pureResolution",
     "Regularity", 
     "Exterior",
     "universalExtension",
     "projectiveProduct"
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

tateResolution = method()
tateResolution(Matrix, PolynomialRing, ZZ, ZZ) := Complex => (m,E,loDeg,hiDeg)->(
     M := coker m;
     reg := regularity M;
     bnd := max(reg+1,hiDeg-1);
     mt  := presentation truncate(bnd,M);
     o   := symExt(mt,E);
     --adjust degrees, since symExt forgets them
     ofixed   :=  map(E^{(rank target o):bnd+1},
                E^{(rank source o):bnd},
                o);
     freeResolution(coker ofixed, LengthLimit=>max(1,bnd-loDeg+1)));

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
     e := local e;
     E := (coefficientRing S)[e_0..e_(numgens S-1), SkewCommutative=>true];
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

degreeD(ZZ, Complex) := Complex => (d, F) -> (
    -- takes the first degree d part of F
    (lo, hi) := concentration F;
    modules := applyValues(F.module, m -> degreeD(d, m));
    if lo === hi then return complex(modules#lo, Base => lo);
    maps := hashTable for i from lo+1 to hi list
                i => map(modules#(i-1), modules#i, degreeD(d, dd^F_i));
    complex maps
    )

degreeD(ZZ, ComplexMap) := ComplexMap => (d, f) -> (
     -- takes the first degree d part of f : S --> T
     -- assumption: that min C === min D
     -- returning a new ComplexMap
     S := source f;
     T := target f;
     Sd := degreeD(d, S);
     Td := degreeD(d, T);
     a := min(min Sd, min Td);
     b := max(max Sd, max Td);
     map(Td, Sd, i -> degreeD(d, f_i))
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
     S:= ring x; 
     E:=ring e;
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
	  e := local e;
	  --S.Exterior = exterior alg over A on dual vars to the vars of S (new vars have deg = {-1,0})
	  S.Exterior = A[e_0 .. e_(numgens S - 1), SkewCommutative => true, Degrees=>{numgens S:-1}]
	  );
     E := S.Exterior;
     symmetricToExteriorOverA(presentation M, vars E, vars S)
     )


directImageComplex = method(Options => {Regularity=>null})
directImageComplex Module := Complex => opts -> (M) -> (
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
     F := freeResolution( image phi, LengthLimit => max(1,1+regM));
     F = E^{-xm} ** F[regM];
     F0 := degreeD(0, F);
     toA := map(coefficientRing E,E,DegreeMap=> i -> drop(i,1));
     --we should truncate away the terms that are 0, and (possibly) the terms above the (n+1)-st
     F0A := toA F0;
     n := numgens ring M;
     naiveTruncation(F0A, -n+1, 1)
     )

directImageComplex Matrix := ComplexMap =>  opts -> (f) -> (
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
     FM := freeResolution( image phiM, LengthLimit => max(1,1+regMN));
     FN := freeResolution( image phiN, LengthLimit => max(1,1+regMN));
     fMN := extend(FN, FM, truncfA ** E);
     fMN = E^{-xm} ** fMN[regMN];
     FM = E^{-xm} ** FM[regMN];
     FN = E^{-xm} ** FN[regMN];
     fMN0 := degreeD(0, fMN);
     EtoA := map(A,E,DegreeMap=> i -> drop(i,1));
     EtoA fMN0
     )

--New 10/2010 -- David Eisenbud

directImageComplex Complex := Complex => opts -> F -> (
     --The idea is to take the Tate resolutions of th modules
     --in F at a point where they're all linear (beyond all the
     --regularities) and form one map of the double complex
     --there. Then resolve backward and take the degree zero part
     --as usual.

     --at the moment we don't handle the user-supplied regularity
     --option.

     S := ring F;
     A := coefficientRing S;
     StoA := map(A,S,DegreeMap => i -> drop(i,1));

     minF := min F; --lowest index i such that F_i is defined
     maxF := max F; --highest index i such that F_i is defined     
     --could save some work by testing for a complex of zero sheaves here; but it probably doesn't matter.
     
     len := max F-min F; -- number of maps given in F

     modules := apply(toList(minF..maxF), i-> F_i);
     --note: modules_i = F_(minF+i)
     maps := apply(toList(minF+1..maxF), i->F.dd_i);
     --note: maps_i = is the map with TARGET F_(minF+i)

     regF := if opts.Regularity === null 
              then (max apply(modules, 
			M -> regularityMultiGraded M))
	      else opts.Regularity;
     if regF < 0 then regF = 0;

     --at the moment we don't handle the user-supplied regularity
     --option.
     
     --truncate len+1 steps beyond regF. This can be refined a bit.
     truncMaps := apply(len, i -> basis(regF+len+1,maps_i)); -- the maps are now numbered by the module they go TO.
     truncModules := prepend(target truncMaps_0, apply(truncMaps, d->source d));
     mapsA := apply(truncMaps, d->StoA matrix d);
     
     xm := (1+len+regF) * degree(S_0);
     Ediffs := apply(truncModules, M -> 
	  symmetricToExteriorOverA(M ** S^{xm}));
     --note: the sources of Ediffs seem to  be free modules over E
     --generated in degree zero.

     E := ring Ediffs_0;
     EtoA := map(A,E,DegreeMap=> i -> drop(i,1));     

     Ereslen := apply(len+1, i -> freeResolution( image Ediffs_i, LengthLimit => len+1));
     mapsE := apply(len, i -> map((Ereslen_i)_0, Ereslen_(i+1)_0, mapsA_i ** E));
     FE := apply(len, i -> extend(Ereslen_i, Ereslen_(i+1), mapsE_i));

     Ereslen = apply(Ereslen, EE -> E^{-xm} ** EE[regF+len+1]);     	 
     FE = apply(FE, FEi -> E^{-xm} ** FEi[regF+len+1]);     	 

     E1 := directSum apply(len+1, i-> (Ereslen_i)_(-regF-i));
     CE1 := components E1;

     E0 := directSum apply(len+1, i-> (Ereslen_i)_(-regF-i-1));     
     CE0 := components E0;
          
     D :=  apply(len+1, i-> apply(len+1, j->
	       if j == i   then -((Ereslen_i).dd_(-regF-i)) else 
	       if j == i+1 then ((FE_i)_(-regF-j)) else 
	       map(CE0_i, CE1_j, 0)));
     Dmat := matrix D;
     Eres := freeResolution(coker Dmat, LengthLimit => max(1,1+regF+len));
     dirIm := (EtoA degreeD(0,Eres))[regF+1-minF];
     --now truncate away the parts below zero and above 
     nonzero := positions(apply(min dirIm..max dirIm, i -> rank dirIm_i != 0), t -> t);
     minDirIm := min nonzero + min dirIm;
     maxDirIm := max nonzero + min dirIm;
     if minDirIm>maxDirIm then complex A^0 else
     (complex(apply(toList(minDirIm..maxDirIm), i-> dirIm.dd_i)))[-minDirIm+1]
     )


regularityMultiGraded = method()
regularityMultiGraded (Module) := (M) -> (
     S := ring M;
     (R,f) := flattenRing S;
     deglen := #degree R_0;
     w := flatten {1,toList(deglen-1:0)};
     regularity (coker f presentation M, Weights=>w)
     )

--FIX -- put in whatever you need on the command line.
     --assumes the existence of
--     kk := ZZ/101;
--     A := kk[x_0..x_(2*d-2)];
--     S := A[y_0,y_1];
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

--files to produce pure resolutions as direct images of 
--twisted Koszul complexes. Both the sparse sop and arbitrary
--multilinear sop's are implemented.


partitions(ZZ, List) := (k, mm) -> (
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
	      partitions(k-i,drop(mm,1)), 
	               s->prepend(i,s)
		       );
    select(P,p->#p == #mm))

projectiveProduct = method()
projectiveProduct(Ring, List) := (A,D) -> (
     --Takes a list of dimensions D = d_1..d_r
     --and makes the  product 
     --P_A^{d_1}x..xP_A^{d_r} 
     --of projective spaces over the base A, as a tower ring.
     --Returns the tower ring
     --together with a system of multilinear parameters
     --(degree = {1,..,1})
     --for the whole product.
     --The length of the sop is 
     --numgens A + sum D, 1 more than 
     --the projective dimension of the whole product.
     --The sop is formed from the symmetric functions
     --using the functions "partitions" above. (It could
     --also be done putting an appropriate matrix
     --in the next routine.)
     S := A;
     x := local x;
     SList := apply (#D, i->S = S[x_(i,0)..x_(i,D_i)]);
     SList = prepend(A,SList);
     SS := last SList;
     dimList := prepend(numgens A-1, D);
        --now make the parameters
     params := matrix{
	  for k from 0 to sum dimList list(
     P := partitions(k,dimList);
     sum(P, p -> product(#dimList, i->sub(SList_i_(p_i), SS))
	  )
                                           ) 
                     };
     (SS,params)
     )
///
restart
path = prepend( "/Users/david/src/Colorado-2010/PushForward",path)
notify=true
loadPackage("BGG", Reload =>true)
--load "examples2.m2"
dL = {1,1}
dLPlus = dL +splice {#dL:1}
A = kk[vars (0..product(dLPlus)-1)]
L = projectiveProduct (A,dL,Sparse =>false)
betti L_1
transpose L_1

dimList = {1,1}
dimList = {2}
dimList = {0,1}
A = kk[a,b]
L = projectiveProduct (A,dimList)
betti L_1
///

projectiveProduct(Matrix, List) := (M,D) -> (
     --Takes a list D of dimensions
     --and makes the appropriate product of 
     --projective spaces over a base ring A = ring M, as a tower ring.
     --Returns the tower ring
     --together with a system of q linear combinations of
     --the 1...1 forms specified by M, which must
     --have product(D_0+1, .. ,D_r+1) rows (and q columns),
     A := ring M;
     S := A;
     x := local x;
     SList := apply (#D, i->S = S[x_(i,0)..x_(i,D_i)]);
     SList = prepend(A,SList);
     SS := last SList;
   --now make the parameters
     if numrows M != product(D, d->1+d) then
	       error("M has the wrong number of rows");
     N := gens trim product apply(#D, i->
	  promote(ideal vars SList_(i+1), SS));
     params := map(SS^1, 
	  SS^(splice{numcols M:splice{1+#D:-1}}), N*M);
     (SS,params)
     )
///
restart
path = prepend( "/Users/david/src/Colorado-2010/PushForward",path)
notify=true
loadPackage("BGG", Reload =>true)
load "examples2.m2"
dL = {1,1}
q = 3

t = product(dL, i->1+i)
A = kk[vars(0..q*t-1)]

M = genericMatrix(A,A_0,t,q)
L = projectiveProduct (M,dL)
betti L_1
numcols M
///

--Now given a degree list degList, form the corresponding
--product of projective spaces and the Koszul complex over
--it, and take the direct image to get a pure resolution.

pureResolution = method()
pureResolution(Ring, List) := (A, degListOrig) -> (
     n := #degListOrig - 1;
     --normalize the degList to make the first entry zero:
     degList := apply(n+1, i-> degListOrig_i - degListOrig_0);
          
     --check the conditions for a pure resolution:
     if numgens A < n then 
        error("number of vars should be >= (length of list)-1");
     for i from 1 to n do (
	  if degList_i <= degList_(i-1) then 
	      error("list must be strictly increasing")
	      );

     --get ready to form the product of projective spaces
     dimList1 := apply(#degList-1, i->degList_(i+1)-degList_i-1);
     --dimList1 = {m_1..m_n} in the notation of our paper.
     degList1 := degList_{0..n-1};
     --degList1 = {0,d_1,..,d_(n-1)} in the notation of our paper.
     
     --Now drop the terms where m_i = 0
     jumpList := positions(dimList1, i-> i>0);
     dimList := dimList1_jumpList;
     
     twists := {0}|degList1_jumpList;
     --the leading zero corresponds to the base ring A.
     --Note that degList1 already begins with a zero corresponding
     --to the normalized degreeList_0.

     (S,params) := projectiveProduct(A, dimList);
     K := S^{reverse twists}**koszulComplex(params);
     while ring K =!= A do (
	  K = directImageComplex K;
	  S = ring K);
     
     --now restore the zero-th twist of degListOrig
     A^{-degListOrig_0} ** K
     )

pureResolution(Matrix, List) := (M, degListOrig) -> (
     A := ring M;
     n := #degListOrig - 1;
     --normalize the degList to make the first entry zero:
     degList := apply(n+1, i-> degListOrig_i - degListOrig_0);
          
     for i from 1 to n do (
	  if degList_i <= degList_(i-1) then 
	      error("list must be strictly increasing")
	      );

     --get ready to form the product of projective spaces
     dimList1 := apply(#degList-1, i->degList_(i+1)-degList_i-1);
     --dimList1 = {m_1..m_n} in the notation of our paper.
     degList1 := degList_{0..n-1};
     --degList1 = {0,d_1,..,d_(n-1)} in the notation of our paper.
     
     --Now drop the terms where m_i = 0
     jumpList := positions(dimList1, i-> i>0);
     dimList := dimList1_jumpList;
     --check for input errors
     if numrows M != product(dimList, d->d+1) then
     	  error("M has the wrong number of rows");
     
     twists := {0}|degList1_jumpList;
     --the leading zero corresponds to the base ring A.
     --Note that degList1 already begins with a zero corresponding
     --to the normalized degreeList_0.

     --now set up the direct image computation
     (S,params) := projectiveProduct(M, dimList);
     K := S^{reverse twists}**koszulComplex(params);
     while ring K =!= A do (
	  K = directImageComplex K;
	  S = ring K);
     
     --now restore the zero-th twist of degListOrig
     A^{-degListOrig_0} ** K
     )

pureResolution(ZZ, List) := (p, degList) -> (
--a version with the sparse system of parameters, giving
--just the characteristic and letting the program supply the
--ground ring.(Produces module of finite length with the given
--pure resolution type.)
     a := local a;
     kk := if p == 0 then QQ else ZZ/p;
     A := kk[a_0..a_(#degList-2)];
     pureResolution(A, degList)
     )


pureResolution(ZZ,ZZ,List):= (p, q, degList) -> (
     --A version with a generic system of q parameters.     
     --p will be the characteristic, q the number of parameters
     --(the codimension of support, 
     --at least when q is large enough.)
     
     --first compute the number of variables needed:
     dimList1 := apply(#degList-1, i->degList_(i+1)-degList_i-1);
     --dimList1 = {m_1..m_n} in the notation of our paper.
     --Now drop the terms where m_i = 0
     jumpList := positions(dimList1, i-> i>0);
     dL := dimList1_jumpList;
     t := product(dL, i->1+i);
     a := local a;
     kk := if p == 0 then QQ else ZZ/p;
     A := kk[a_0..a_(q*t-1)];
     M := genericMatrix(A,A_0,t,q);
     pureResolution(M, degList)
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
     HREF("https://macaulay2.com/Book/", "Sheaf Algorithms Using Exterior Algebra"), ".", 
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
	  Complex => {"a finite piece of the Tate resolution"}  
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
     " in ", HREF("https://macaulay2.com/Book/", "Sheaf Algorithms Using Exterior Algebra"),  ".",
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
          ff = freeResolution coker map(S^{1:0},S^{3:-1,2:-2},{{x_0..x_2,x_3^2,x_4^2}}); 
	  alpha = map(S^{1:-2},target ff.dd_3,{{1,4:0,x_0,2:0,x_1,0}})*ff.dd_3; 
	  beta = ff.dd_4//syz alpha; 
	  K = syz syz alpha|beta;
	  fK = freeResolution prune coker K;
	  s = random(target fK.dd_1,S^{1:-4,3:-5});
	  ftphi = freeResolution prune coker transpose (fK.dd_1|s);
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
	  betti freeResolution F
	  regularity F
	  cohomologyTable(presentation F,E,-6,6)
     	  ///,
     SeeAlso => {symExt}
     }

doc ///
   Key
     directImageComplex
     [directImageComplex, Regularity]
   Headline
     direct image complex
   Usage
     directImageComplex F
   Description
    Text
      Forms a minimal free complex representing the direct image complex of $F$ in the
      derived category, where $F$ is a module, chain complex or map of modules.
   Caveat
    The option "Regularity" is currently not supported.
///

doc ///
   Key
    Regularity
   Headline
    Option for directImageComplex
   Caveat
    Currently not supported
///
doc ///
   Key
    Exterior
   Headline
    dual exterior algebra cached in a polynomial ring
   Description
    Text
     checked for, and possibly installed, by "symmetricToExterior"
///

doc ///
   Key 
     (directImageComplex, Module)
   Headline
     Complex representing the direct image 
   Usage
     F = directImageComplex M
   Inputs 
     M: Module
       graded over a ring of the form S = A[y_0..y_n], representing a sheaf on $\PP^n_A$,
       where A is a polynomial ring.
     Regularity=>ZZ
       the Castelnuovo-Mumford regularity of {\tt M}.  If not provided, this value will be computed
   Outputs
     F: Complex
       complex of free modules over A. Homology in homological degree -i is 
       $R^i \pi_* {\mathcal M}$, where ${\mathcal M}$ is the sheaf on $\PP^n_A$ represented by M.
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
      as the direct image of a vector bundle on $\PP^n_A$.
      
      The following example can be used to study the loci in the family of extensions
      of a pair of vector bundles on $\PP^1$ where the extension bundle has a given
      splitting type: this type is calculated by the Fitting ideals of the
      matrices defining the direct image complexes of various twists of the bundle.
      See Section 5 in loc. cite. It is conjectured there that all the sums of
      these Fitting ideals for the universal extension of 
      $\mathcal O_{\PP^1}^{r-1}$ by $\mathcal O_{\PP^1}(d)$ are radical,
      as in the example below.
      
      First we examine the extensions of ${\mathcal O}_{\PP^1}(1)$ by ${\mathcal O}_{\PP^1}(-3)$.
      There is a 3-dimensional vector space 
      $$
      Ext^1({\mathcal O}_{\PP^1}(1),{\mathcal O}_{\PP^1}(-3))
      $$
      of extensions. The ``universal extension'' is thus a bundle on
      $\PP^1\times Ext$. The locus where the extension
      bundle splits as ${\mathcal O}_{\PP^1}(-2) \oplus {\mathcal O}_{\PP^1}$ is the
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
      ${\mathcal O}_{\PP^1}^2$ by ${\mathcal O}_{\PP^1}(6)$
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
   Headline
     map of direct image complexes
   Usage
     piF = directImageComplex F
   Inputs
     F:Matrix
       a homomorphism $F : M \rightarrow N$ of graded $S = A[y_0..y_n]$ modules, graded of degree 0,
       where we think of $M$ and $N$ as representing sheaves on $\PP^n_A$
   Outputs
     piF:ComplexMap
       the induced map on chain complexes {\tt piF : directImageComplex M --> directImageComplex N}
   Description
    Text
     We give an application of this function to create 
     pure free resolutions. A much more general tool for doing
     this is in the script pureResolution.
     
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
     betti freeResolution coker m
     (dual oo)[-3]
   SeeAlso
     directImageComplex
     pureResolution
     universalExtension
///

doc ///
   Key
     (directImageComplex, Complex)
   Headline
     direct image of a chain complex
   Usage
     piC = directImageComplex C
   Inputs
     C:Complex
       over a bigraded ring $S = A[y_0..y_n]$, where $A$ is singly graded
   Outputs
     piC:Complex
       a minimal free complex representing the direct image complex in the derived category
   Description
    Text
     The method is an elaboration of the exterior algebra method for computing cohomology
     discovered by Eisenbud, Floeystad, Schreyer: Sheaf cohomology and free resolutions over
     exterior algebras. Trans. Amer. Math. Soc. (2003).

     We give an application of this function to create generalized Eagon-Northcott complexes,
     discovered by Buchsbaum, Eisenbud, and Kirby, and described in 
     Eisenbud, Commutative Algebra, 1995, section A2.6.  This method can be generalized 
     to produce pure resolutions of any degree sequence.

     These are the complexes associated to a generic 2 by 5 matrix.
    Example
      (p,q) = (2,5) -- number of rows and columns
      A=ZZ/101[a_(0,0)..a_(p-1,q-1)];
      S = A [x_0..x_(p-1)];
      M = sub(map(A^p, A^{q:-1},transpose genericMatrix(A,a_(0,0),q,p)), S)
      Y = map(S^1, S^{q:{-1,-1}}, (vars S)*M)
      F = koszulComplex Y
      L = for i from -1 to q-p+1 list directImageComplex(F**S^{{i,0}});
      L/betti
   Caveat
     This function is not yet functorial, i.e., there is no method to take a map of
     chain complexes and produce the induced map on direct image complexes.
     Additionally, the input ring must be a tower ring with exactly two gradings, and the
     variables must have degree $\{0,1\}$ and $\{1,0\}$
     A later version will remove these restrictions
   SeeAlso
     directImageComplex
///

doc ///
   Key 
     universalExtension
     (universalExtension, List, List)
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
     Every vector bundle E on $\PP^1$ splits as a sum of line bundles
     OO(a_i). If La is a list of integers, we write E(La) for the direct sum of the
     line bundle OO(La_i).  Given two such bundles specified by the lists
     La and Lb this script constructs a module representing the universal
     extension of E(Lb) by E(La). It is defined on the product variety
     Ext^1(E(La), E(Lb)) x $\PP^1$, and represented here by
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

doc ///
   Key
     pureResolution
     (pureResolution, Ring, List)
     (pureResolution, Matrix, List)     
     (pureResolution, ZZ, List)
     (pureResolution, ZZ, ZZ, List)     
   Headline
     creates a pure resolution as an iterated direct image
   Usage
     F = pureResolution(A, D)
     F = pureResolution(M, D)
     F = pureResolution(p, D)
     F = pureResolution(p,q,D)
   Inputs
     A:Ring
     D:List
     M:Matrix 
       D is a strictly increasing list of integers, the degree sequence
       A is a ring with at least #D-1 variables, the base ring.
       The pure resolution created is the sparse one defined in the
       paper of Eisenbud-Schreyer.
       If M, a matrix over A, is given, then M is used instead
       to construct the resolution (see below).
       If one integer p is given, the program constructs a base ring with the right number of
       variables for the sparse example. If two integers p,q are given, the program
       constructs a base ring of characteristic p and makes a generic system of parameters
       with q elements by taking a generic matrix for M.
   Outputs
     F:Complex
       F will be a pure resolution over A of a module of finite length
       over A, with the desired degree sequence.
       If there are more than #D-1 variables, then a longer resolution
       will be produced, padding the degree sequence at the end
       with consecutive integers.
   Description
    Text
     A "pure free resolution of type (d_0,d_1,..,d_n)" 
     is a resolution of a graded Cohen-Macaulay
     module M over a polynomial ring such that for each 
     i = 1,..,n, the module of i-th syzygies of M is generated by
     syzygies of degree d_i. Eisenbud and Schreyer constructed
     such free resolutions in all characteristics and for all
     degree sequences $d_0 < d_1 < \cdots < d_n$ by pushing forward
     appropriate twists of a Koszul complex. (The construction
     was known for the Eagon-Northcott complex since work of Kempf).

     The script allows several variations including a sparse version 
     and a generic version.
     
     Here is a simple example, where we produce
     one of the complexes in the family that included the
     Eagon-Northcott complex (see for example the appendix in
     "Commutative Algebra with a View toward Algebraic Geometry"
     by D. Eisenbud.) This way of producing the Eagon-Northcott 
     complex was certainly known to George Kempf, who may have invented it.
    Example
     kk = ZZ/101
     A = kk[u,v,w]
     T = A[x,y]
     params = matrix"ux,uy+vx,vy+wx,wy"
     kn = koszulComplex(params)
     directImageComplex kn
    Text
     If we twist the map kn a little before taking the direct image
     we get other complexes in a family that also includes
     the Buchsbaum-Rim complex (see Eisenbud, loc. cit.)
    Example
     for d from -1 to 3 do 
       (print betti directImageComplex (T^{{d,0}}**kn);print())
    Text
     For more complex examples, we use the function 
     {\tt pureResolution}, which creates a Koszul complex over a product of
     projective spaces over a ground ring A and (iteratively) forms the direct image over A.
     In the following
     we specify a ground ring A and a degree sequence.
    Example
     A = kk[a,b,c]
     betti (pureResolution(A,{1,3,4,6}))
    Text
     If one doesn't want to bother creating the ring, it suffices to give the characteristic.
    Example
     betti (F = pureResolution(11,{0,2,4}))
     describe ring F
    Text
     With the form {\tt pureResolution(M,D)}
     It is possible to specify a matrix M of linear forms in the ground ring A that
     defines the parameters used in the Koszul complex whose direct image is taken.
     The matrix M in pureResolution(M,D) 
     should have size product(m_i+1) x q, where
     the m_i+1 are the successive differences of the entries of D that happen to be >1, and
     q >= #D-1+sum(m_i).(The m_i are the dimensions of the projective spaces from whose product
     we are projecting.)
    Example
     A = kk[a,b]
     M = random(A^4, A^{4:-1})
     time betti (F = pureResolution(M,{0,2,4}))
    Text
     With the form {\tt pureResolution(p,q,D)}
     we can directly create the situation of {\tt pureResolution(M,D)} where M is
     generic 
     product(m_i+1) x #D-1+sum(m_i) 
     matrix of linear forms defined over a ring with product(m_i+1) * #D-1+sum(m_i) 
     variables of characteristic p, created by the script. For a given number of
     variables in A this runs much faster than taking a random matrix M.
    Example
     time betti (F = pureResolution(11,4,{0,2,4}))
     ring F
   SeeAlso
     directImageComplex
     universalExtension
///



doc ///
   Key 
     projectiveProduct
     (projectiveProduct, Ring, List)
     (projectiveProduct, Matrix, List)
   Headline
     Makes a product of projective spaces and a system of parameters
   Usage
     (S, params) = projectiveProduct(A,dimList)
   Inputs 
     A: Ring
     dimList: List
       A the desired base ring. dimList a list of dimensions {d1..dn}
   Outputs
     S: Ring
     params: Matrix
       S is the iterated tower ring A[x_(0,0)..x_(0,d1)]..[..x_(n,dn)] representing the product
       P_A^{d1} x ..x P_A^{dn}, where the products are relative to A, and params is a system
       of multilinear forms in S. They are sparse if M is not present, or determined by M
       if it is.
   SeeAlso 
     pureResolution
///


TEST///
          S = ZZ/32003[x_0..x_2]; 
	  E = ZZ/32003[e_0..e_2, SkewCommutative=>true];
	  M = coker matrix {{x_0^2, x_1^2}};
	  m = presentation truncate(regularity M,M);
	  assert(symExt(m,E)==map(E^{{1}, {1}, {1}, {1}},E^4,{{e_2, 0, 0, 0}, {e_1, e_2, 0, 0}, {e_0, 0, e_2, 0}, {0, e_0, e_1, e_2}}))
///

TEST///
          S = ZZ/32003[x_0..x_2]; 
	  E = ZZ/32003[e_0..e_2, SkewCommutative=>true];
	  M = coker matrix {{x_0^2, x_1^2, x_2^2}};
	  assert(bgg(1,M,E)==map(E^{3:2},,{{e_1, e_0, 0}, {e_2, 0, e_0}, {0, e_2, e_1}}))
///
TEST///	  
	  S = ZZ/32003[x_0..x_2]; 
	  E = ZZ/32003[e_0..e_2, SkewCommutative=>true];
	  m = matrix{{x_0,x_1}};
	  regularity coker m
	  T = tateResolution(m,E,-2,4)
          assert(toList(7:1) == apply(length T+1, i-> rank T_i))
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
	  assert(matrix entries beilinson(alpha,S) ==  map(S^6,S^1,{{0}, {1}, {0}, {-1}, {0}, {0}}))
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
  kk = ZZ/101
  A = kk[a]
  S = A[x,y]
  M1 = S^{{-1,0},{0,0}}
  C1 = directImageComplex(id_M1)
  assert(C1_0 == 1)
  assert isWellDefined C1
  assert isCommutative C1

  M2 = S^{{-1,0}}
  C2 = directImageComplex(id_M2)
  assert isWellDefined C2
  assert isCommutative C2
  assert(C2_0 == 0)

  M3 = S^{{-2,0}}
  C3 = directImageComplex(id_M3)
  assert isWellDefined C3
  assert isCommutative C3
  assert(C3_0 == 0)
///

TEST///
  (p,q) = (2,5)
  kk = ZZ/101
  A=kk[a_(0,0)..a_(p-1,q-1)]
  S = A [x_0..x_(p-1)]
  M = sub(map(A^p, A^{q:-1},transpose genericMatrix(A,a_(0,0),q,p)), S)

  Y = map(S^1, S^{q:{-1,-1}}, (vars S)*M)
  F = koszulComplex Y
  L = directImageComplex(F**S^{{2,0}})
  ans = new BettiTally from {(0,{0},0) => 3, (3,{4},4) => 5, 
       (1,{1},1) => 10, (4,{5},5) => 2,
       (2,{2},2) => 10};
  assert(betti L == ans)
///

-*
--I don't see why the following doesn't work. The output of the left side sure LOOKS like that on the right
TEST///
A = ZZ/11[a,b]
(projectiveProduct(A,{1,1}))_0 === A[x_(0,0), x_(0,1)][x_(1,0), x_(1,1)]
///
*-

TEST///
A = QQ[a,b]
betti pureResolution(A,{0,2,4}) == new BettiTally from {(0,{0},0) => 3, (1,{2},2) => 6, (2,{4},4) => 3}
betti pureResolution(11,{0,2,4})== new BettiTally from {(0,{0},0) => 3, (1,{2},2) => 6, (2,{4},4) => 3}
betti pureResolution(2,{1,2,4})==new BettiTally from {(0,{1},1) => 2, (1,{2},2) => 3, (2,{4},4) => 1}
betti pureResolution(2,3,{1,2,4})
///
end--

restart
uninstallPackage "BGG"
installPackage "BGG"
check "BGG"
viewHelp BGG
