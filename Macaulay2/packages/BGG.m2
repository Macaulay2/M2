newPackage(
	"BGG",
    	Version => "1.0", 
    	Date => "September 26, 2006",
    	Authors => {
	     {Name => "David Eisenbud", Email => "de@msri.org", HomePage => "http://www.msri.org/~de/"},
	     {Name => "Frank Schreyer", Email => "", HomePage => ""},
	     {Name => "Sorin Popescu", Email => "", HomePage => ""},
	     {Name => "Mike Stillman", Email => "", HomePage => ""}
	     },
    	Headline => "Bernstein-Gelfand-Gelfand correspondence",
    	DebuggingMode => true
    	)

-- not exported:
--     sortedBasis
-- submatrixByDegrees takes matrix, two lists of ints, returns the submatrix
-- strands takes a complex and a range of degrees and returns the subquotient complex

export {
     bettiT,
     setupBGG,  -- written, BUT: caveat: insure that S is isom to A[x].
     degreeZeroPart, 
     symmetricToExterior, -- written and tested (but: works best if first grading is positive)
     symExt,
     bgg,
     tateResolution, -- written and tested (same caveat about degrees) WARNING: need to edit to fix regularity and truncation!!!
     directImageComplex,
     sheafCohomology,
     beilinson1,
     beilinson,
     RegularityBound}

setupBGG = method()
setupBGG(RingMap,List) :=
setupBGG(RingMap,Symbol) := (F,e) -> (
     -- F : A --> S isomorphic to A[x].
     -- We require that the last grading of S satisfies: it is of the form {0,...,0,1,...,1}
     -- where possibly there are no variables whose last degree is 0
     S := target F;
     A := source F;
     kk := coefficientRing S;
     ds := apply((options S).Degrees, last);
     xvars := positions(ds, x -> x === 1);
     avars := positions(ds, x -> x === 0);
     if #xvars + #avars =!= #ds then
       error "the last grading in the ring must take the value 0 or 1 for all variables";
     nexte := -1;
     gensE := apply(0..numgens S-1, i -> if ds#i === 0 then S_i else (nexte = nexte+1;e_nexte));
     E := kk[gensE, Degrees=>(options S).Degrees, SkewCommutative=>xvars];
     I = ideal presentation S;
     if I != 0 then (
	  -- We need to check that I does not involve the x variables
	  E = E/substitute(I,vars E);
	  );
     ee := (vars E)_xvars;
     xx := (vars S)_xvars;
     E.BGGcache = new HashTable from {dualVariables=>xx,variables=>ee,koszulDual=>S,baseRing=>A,twist=>degree(xx_0)};
     S.BGGcache = new HashTable from {variables=>xx,dualVariables=>ee,koszulDual=>E,baseRing=>A,twist=>degree(xx_0)};
     E)

selectComponent=(L,k)->(apply(L,c->c#k))

bettiT=method()
bettiT(ChainComplex):=(F)->(
--writes the betti table of a bigraded Tate resolution (with the maps 
--going from right to left, as in the usual betti command)
--using the SECOND degrees instead of the first.
     a:=min F;b:=max F;
     btt:=ZZ/3[tt];
     bT:=new ChainComplex;
     bT.ring=btt;
     apply(a..b,i->bT#i=btt^(-selectComponent(degrees (F_i),1)));
     betti bT
     )     

degreeZeroPart = method()
degreeZeroPart(ChainComplex) := (T)->(
--Takes a (doubly) graded free complex over E (the exterior algebra 
--over a ring A, where the variables of E have (last) degree 1 and
-- the other variables (coming from A) have (last) degree 0.
--and extracts the the (last) degree 0 part of T \tensor_E A, 
--a complex of free A-modules.
-- Current assumption: the degree zero part consists of free A-modules.
     a:=min T;b:=max T;
     A:=(ring T).BGGcache.baseRing;
     ndegsA:=degreeLength A;
     piT:=new ChainComplex;
     piT.ring=A;
     bj:=0;aj:=0;LLj:={};Lj:=LLj;co:={};ro:={};f:=null;
     apply(a..b,j->(bj=rank T_j;
	       LLj=select(degrees T_j,d->last d==0);
	       LLj=apply(LLj,d->-take(d,ndegsA));
	       piT#j=A^LLj));
     apply(a+1..b,j->(
	       aj=rank  T_(j-1);
	       bj=rank T_j;
	       Lj=degrees T_(j-1);
	       LLj=degrees T_(j);
	       co=toList select(0..aj-1,i->last (Lj#i)==0);
	       ro=toList select(0..bj-1,i->last (LLj#i)==0);
	       f=substitute(((T.dd_j)^co)_ro,A);
	       piT.dd#j=map(piT_(j-1),piT_j,f)));
     piT)

symmetricToExterior=method(Options=>{Minimize=>true})
symmetricToExterior(Matrix):=opt->m->(
--this routine can be used to move from
--a linear presentation matrix of a module over the symmetric algebra S=A[x]
--and a matrix over the Koszul dual algebra E=A<e>. If the original module has
--regularity 0, the matrix constructed is the 0th matrix in the Tate resolution.
--
--this function takes a  presentation matrix m with 
--entries m^i_j of (last) degrees 0 or 1 only 
--of a module over a symmetric or exterior algebra S over A
--and returns the map
--    Hom_A(E,(coker m)_0) -> Hom_A(E,(coker m)_1)
--over the  Koszul dual algebra E
--                     Minnesota, Sept 26 2006, David Eisenbud, Frank Schreyer, Mike Stillman
     S:= ring m; 
     M:=coker m;
     x:=S.BGGcache.variables; e:=S.BGGcache.dualVariables;   E:=S.BGGcache.koszulDual;
     a:=rank source m;
     La:=apply (degrees source m, last);
     co:=toList select(0..a-1,i-> La_i==0);
     M0:=coker substitute(m_co,vars E); -- (degree 0 part of M) tensor_A E
     m1:=presentation (ideal x * M); 
-- m1 is the presentation of M truncated in degrees >=1. The
-- script is written this way because the generators of ideal x* M are ordered
---as follows
-- x_0 generators of M,x_1*generators of M, ...
     b:=rank source m1;
     Lb:=apply(degrees source m1,last);     
     cob:=toList select(0..b-1,i-> Lb_i==1);
     M1:=coker substitute(m1_cob,vars E);-- (degree 1 part of M) tensor_A E
     F:=substitute(id_(target m),vars E);
     G:=e_{0}**F;
     n:=rank source e - 1;
     apply(1..n,j->G=G||(e_{j}**F));
     dL:=degreeLength S;
     twist:= 2 * S.BGGcache.twist;
     phi:=map(M1**E^{twist},M0,G);
     if opt.Minimize then prune phi else phi
     )

symExt = method()
symExt(Matrix,Ring) := (m,E) ->(
     ev := map(E,ring m,vars E);
     mt := transpose jacobian m;
     jn := gens kernel mt;
     q  := vars(ring m)**id_(target m);
     ans:= transpose ev(q*jn);
     --now correct the degrees:
     map(E^{(rank target ans):1}, E^{(rank source ans):0}, 
         ans));

bgg = method()
bgg(ZZ,Module,Ring) := (i,M,E) ->(
     S :=ring(M);
     numvarsE := rank source vars E;
     ev:=map(E,S,vars E);
     f0:=basis(i,M);
     f1:=basis(i+1,M);
     g :=((vars S)**f0)//f1;
     b:=(ev g)*((transpose vars E)**(ev source f0));
     --correct the degrees (which are otherwise
     --wrong in the transpose)
     map(E^{(rank target b):i+1},E^{(rank source b):i}, b));

truncateForLastDegree=(bnd,M)->(
     v:=gens ring M;
     s=ideal select(v, x->last degree x==1);
     trim sum(numgens M, i -> (
	       d := last degree M_i;
	       if d>=bnd then image M_{i} else
	         trim (s^(bnd-d)*image M_{i})))
	)
tateResolution = method()
tateResolution(Matrix,ZZ,ZZ) := (m,loDeg,hiDeg)->(
     --We assume that reg coker m <= hiDeg-1
     M := coker m;
--     reg := regularity M;
--     bnd := max(reg+1,hiDeg-1);
     S := ring m;
     twist := hiDeg * S.BGGcache.twist;
     mt := minimalPresentation truncateForLastDegree(hiDeg,M);
     mt = mt ** S^{twist};
     o := ker symmetricToExterior(presentation mt);
     c:=res(o, LengthLimit=>max(1,hiDeg-loDeg+1));
     glc = c;
     E := S.BGGcache.koszulDual;
     c[hiDeg-1] ** E^{twist})

directImageComplex = method(Options=>{RegularityBound=>null})
directImageComplex Module := o -> (M) -> (
     m := presentation M;
     --reg := max(1,regularity M);
     reg := if o.RegularityBound === null
       then (print "warning: using regularity 4"; 4)
       else o.RegularityBound;
     T := tateResolution(m,-2,reg);
     glT = T;
     degreeZeroPart T
     )

sheafCohomology = method()
sheafCohomology(Matrix,Ring,ZZ,ZZ) := (m,E,loDeg,hiDeg)->(
     T := tateResolution(m,E,loDeg,hiDeg);
     k := length T;
     d := k-hiDeg+loDeg;
     if d > 0 then 
        chainComplex apply(d+1 .. k, i->T.dd_(i))
     else T);

sortedBasis = (i,E) -> (
     m := basis(i,E);
     p := sortColumns(m,MonomialOrder=>Descending);
     m_p);

beilinson1 = method()
beilinson1(Matrix,List,ZZ,Ring) := (e,dege,i,S)->(
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

beilinson = method()
beilinson(Matrix,Ring) := (o,S) -> (
     coldegs := degrees source o;
     rowdegs := degrees target o;
     mats = table(numgens target o, numgens source o,
              (r,c) -> (
                   rdeg = first rowdegs#r;
                   cdeg = first coldegs#c;
                   overS = beilinson1(o_(r,c),cdeg-rdeg,cdeg,S);
                   -- overS = substitute(overE,S);
                   map(U(rdeg,S),U(cdeg,S),overS)));
     if #mats === 0 then matrix(S,{{}})
     else matrix(mats));

U = method()
U(ZZ,Ring) := (i,S) -> (
     if i < 0 or i >= numgens S then S^0
     else if i === 0 then S^1
     else cokernel koszul(i+2,vars S) ** S^{i});

beginDocumentation()

document { Key => BGG,
     Headline => "BGG (Bernstein-Gelfand-Gelfand) correspondence",
     EM "BGG", " is a package for using the Eisenbud-Floystad-Schreyer
     explicit form of the BGG correspondence.",
     PARA{
     "In order to use the functions of this package, it is necessary to
     call ", TO setupBGG, " to initialize the required rings and their relationship
     to each other."},
     EXAMPLE lines ///
--     	  loadPackage "BGG"
	  S = ZZ/101[x_0..x_4]
	  describe(E = setupBGG(S,e))
     ///,
     UL {
	  TO "example: direct image sheaf complexes"
	  }
     }

document { 
     Key => {setupBGG,(setupBGG,RingMap,Symbol),(setupBGG,RingMap,List)},
     Headline => "initialize the BGG package",
     Usage => "setupBGG(S,e)",
     Inputs => {
	  "S" => RingMap => "a polynomial ring",
	  "e" => null => {"either ", ofClass Symbol, " or ", ofClass List, ".  The
	       new variables chosen will be e_0, e_1, ..."}
	  },
     Outputs => {
	  Ring => "a ring where certain variables of S have been replaced with skew commuting
	  variables"
	  },
     Consequences => {
	  "The ring S is checked for consistency for use with the BGG package (see below),
	  and information is stored in S and in the returned value which is used
	  by the routines of this package."
	  },     
     "In order to use the ", TO "BGG", " package, it is necessary to first call this function before using
     any other functions in this package.",
     PARA{},
     "The ring S must be a polynomial ring such that the degree vector of each variable ends with
     a one (these are the x variables) or a zero (these are the coefficient variables).  
     Thus S is to be regarded as a polynomial ring over a coefficient A. The ring 
     S may be a quotient ring, but the quotient ideal should only involve the coefficient variables
     --that is, the relations are all on A.",
     PARA{},
     "An exterior algebra is created which replaces the x variables with e_0, e_1, etc, and where the 
     coefficient variables commute with everything.",
     EXAMPLE lines ///
--	  loadPackage "BGG"
	  S = ZZ/101[x,y,z]
	  A = coefficientRing S
	  F = map(S,A)
	  E = setupBGG(F,e)
	  describe E
	  E = setupBGG(F,{X,Y,Z})
     	  describe E
	  ///,
     EXAMPLE lines ///
	  S = ZZ/101[a,x,b,y,z, Degrees=>{{1,0},{2,1},{1,0},{-1,1},{3,1}}]/(a^2,a*b,b^2)
	  A = ZZ/101[a,b]/(a^2,a*b,b^2)
	  F = map(S,A)
	  E = setupBGG(F,{X,Y,Z})
	  describe E
	  ///,
     SeeAlso => {}
     }
document { 
     Key => "degreeZeroPart",
     Headline => "degree zero part of a (bigraded) complex",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => symmetricToExterior,
     Headline => "Convert to the Koszul dual",
     Usage => "symmetricToExterior m",
     Inputs => {
	  "m"=>Matrix=>"matrix over the symmetric algebra with (last) 
	  row degrees 0 and each (last) col degree either 0 or 1", 
	  Minimize=>Boolean=>{"if true the program prunes the output to
	  get a map between minimally generated modules"}
	  },
     Outputs => {
	  Matrix=>"the map (E tensor_A M_0 --> E tensor_A M_1) of modules over the exterior algebra E,
	  where M = coker m"
	  },
     EXAMPLE lines ///
     restart
     A = ZZ/101[a]
     S=ZZ/101[a,x,y, Degrees=>{{1,0},2:{1,1}}]
     F = map(S,A)
     m=matrix"ay,0;x,0;-y,x;0,-y"
     m = map(S^{{1,0},3:{0,0}}, , m)
     isHomogeneous m
     E=setupBGG(F,{e,f})
     describe E
     p = symmetricToExterior m
     pnonminimal = symmetricToExterior(m, Minimize=>false)
	  ///,
     EXAMPLE lines ///
     restart
     A=ZZ/101[a]/a^2
     S=A[x,y]
     F = map(S,A)
     m=matrix"ay,0;x,0;-y,x;0,-y"
     isHomogeneous m
     E=setupBGG(F,{e,f})
     describe E
     F = symmetricToExterior m
     Fnonminimal = symmetricToExterior(m, Minimize=>false)
     degrees F
	  ///,
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => symExt,
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => bgg,
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => tateResolution,
     Headline => "the Tate resolution over the exterior algebra of a module",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE lines///
          restart
     A = ZZ/101[a]
     S=ZZ/101[a,x,y, Degrees=>{{1,0},2:{1,1}}]
     F = map(S,A)
     m=matrix"ay,0;x,0;-y,x;0,-y"
     m = map(S^{{1,0},3:{0,0}}, , m)
     isHomogeneous m
     E=setupBGG(F,{e,f})
     C = tateResolution(m, -1,1)
     --###
	  ///,
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => {(directImageComplex,Module),directImageComplex},
     Headline => "the direct image complex",
     Usage => "directImageComplex M",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE lines///
          restart
     A = ZZ/101[a]
     S=ZZ/101[a,x,y, Degrees=>{{1,0},2:{1,1}}]
     F = map(S,A)
     m=matrix"-ay,0;x,0;-y,x;0,-y"
     m = map(S^{{1,0},3:{0,0}}, , m)
     isHomogeneous m
     E=setupBGG(F,{e,f})
     C = directImageComplex(coker m ** S^{-2*degree x})
     --###
	  ///,
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => sheafCohomology,
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => beilinson1,
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => beilinson,
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => U,
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
end

----------------------------------------------------
-- Example: push forward of OO_X, X is the blowup of 
-- an elliptic surface singularity
-- 
--
restart
loadPackage "Local"
loadPackage "BGG"
debug BGG
kk=ZZ/101
A1=kk[a,b,c,Degrees=>{3:{1,0}}]

--I=ideal{a^3+b^3+c^3+c^4}
--I=ideal{a^3+b^3+c^3}
I=ideal{a*b*c+a^4+b^4+c^4}

A=A1/I
setMaxIdeal(ideal(a,b,c))
S1=kk[a,b,c,x_0,x_1,x_2,Degrees=>{3:{1,0},3:{1,1}}]
S=S1/substitute(I,S1)
setMaxIdeal(ideal(a,b,c,x_0,x_1,x_2))

Itotal=minors(2,matrix{{a,x_0},{b,x_1},{c,x_2}})
Istrict= saturate(Itotal,ideal(a,b,c))

Mstrict = coker gens Istrict
Mtotal = coker gens Itotal
F = map(S,A)
E = setupBGG(F,e)
setMaxIdeal(ideal(a,b,c,e_0,e_1,e_2))

C = directImageComplex((Mtotal**S^{-2*{1,1}}), RegularityBound=>4)
C = directImageComplex((Mstrict**S^{-2*{1,1}}), RegularityBound=>4)

C = directImageComplex((Mtotal**S^{-3*{1,1}}), RegularityBound=>6)
C = directImageComplex((Mstrict**S^{-3*{1,1}}), RegularityBound=>5)

C = directImageComplex((Mtotal**S^{2*{1,1}}), RegularityBound=>2)
C = directImageComplex((Mstrict**S^{2*{1,1}}), RegularityBound=>2)

-------------------------------------------------------------------
-- The following lines are tests for Mike to speed this up --------
-- or fix localPrune...
restart
loadPackage "LocalRings"
loadPackage "BGG"
debug BGG
kk=ZZ/101
A1=kk[a,b,c,Degrees=>{3:{1,0}}]

I=ideal{a^3+b^3+c^3+c^4}
I=ideal{a*b*c+a^4+b^4+c^4}
I=ideal{a^3+b^3+c^3}

A=A1/I
setMaxIdeal(ideal(a,b,c))
S1=kk[a,b,c,x_0,x_1,x_2,Degrees=>{3:{1,0},3:{1,1}}]
S=S1/substitute(I,S1)
setMaxIdeal(ideal(a,b,c,x_0,x_1,x_2))

Itotal=minors(2,matrix{{a,x_0},{b,x_1},{c,x_2}})
Istrict= saturate(Itotal,ideal(a,b,c))

Mstrict = coker gens Istrict
Mtotal = coker gens Itotal
F = map(S,A)
E = setupBGG(F,e)
setMaxIdeal(ideal(a,b,c,e_0,e_1,e_2))
d=5
truncateForLastDegree(d,Mtotal)
M7 = localPrune oo
M7 = M7 ** S^{{d,d}}
N = ker symmetricToExterior(presentation M7)
N = N ** E^{{d,d}}
N = localPrune N
C = localResolution(N, LengthLimit=>8) -- This is bad...
C0 = degreeZeroPart C
prune HH((map(kk,A,{0,0,0})) C0)
prune HH((map(kk,E,{0,0,0,0,0,0})) C)
--------------------------------------------------------------------
E1 = (kk [a, b, c, e_0, e_1, e_2, Weights=>{-1,-1,-1,1,1,1},Global=>false,Degrees => {{1, 0}, {1, 0}, {1, 0}, {1, 1}, {1, 1}, {1, 1}}, SkewCommutative => {3, 4, 5}])/(c^4+a^3+b^3+c^3)
N1 = substitute(N,E1);
N2 = syz N1;
substitute(N2,0)

mingens image N2
C = localResolution(coker N,LengthLimit=>2*d)
fN=(localResolution(coker N,LengthLimit=>2*d))[d-1]**E^{{d-1,d}}
bettiT dual fN



----------------------------------------------------

document { Key => "example: direct image sheaf complexes",
     EXAMPLE {
"--Versal deformation of the extension of direct sum of line bundles on P1
--For O(a1)+O(a2)+...+O(ad), 1<=a1<=...<=ad.
--
sylvester=(n,x,y)->(R:=ring x;
     matrix(apply(n+1,i->apply(n,j->if i==j then x else 
		    if i==j+1 then y else 0_R))))",

"extensionMatrix=(a,b,LL)->(
     -- extension block with entries L
     matrix apply(a+1,i->apply(b,j->if i==0 and j<b-a-1 then LL_j else 0_S)))",

"setupDef=(L,kk)->(
-- input:
-- 	  L = list of twist 0 <L_0<=L_1<=...<=L_(k-1)
--        kk ground field     
-- creates:
--        the base ring A=kk[a's] for the versal deformation
--        O(L_0)+O(L_1)+...+O(L_(k-1)) on P^1,
--        the symmetric algebra S=A[x_0,x_1],
--        the extrerior E=A[e_0,e_1],
-- 	  xx=matrix{{x_0,x_1}}, ee=matrix{{e_0,e_1}}
-- output:
--        returns a presentation matrix of the versal deformation.
     k=#L;
     as:=toList join toSequence apply(k,i->join(
	       toSequence apply(i,j->apply(L_i-L_j-1,t->a_(j,i,t)))));
     degas:=toList join toSequence apply(k,i->join(
	       toSequence apply(i,j->L_i-L_j-1:{2*(i-j),0})));    
     A=kk[as,Degrees=>degas];
     degas1:=toList join(toSequence degas,(2:{1,1}));
     E=kk[join(as,{e_0,e_1}),Degrees=>degas1,SkewCommutative=>true];
     S=kk[join(as,{x_0,x_1}),Degrees=>degas1];
     ee=matrix{{e_0,e_1}};
     xx=matrix{{x_0,x_1}};
     M:=sylvester(L_0,x_0,x_1);
     use S;LL={};NN:=matrix{{0_S}};
     apply(1..#L-1,i->(LL=toList apply(L_i-L_0-1,t->x_1*a_(0,i,t));
	 M=M|extensionMatrix(L_0,L_i,LL)));
     apply(1..#L-1,j->(
	       NN=map(S^(L_j+1), S^(sum(0..j-1,t->L_t)),0);
	       NN=NN|sylvester(L_j,x_0,x_1);
	       apply(j+1..#L-1,i->(LL=toList apply(L_i-L_j-1,t->x_1*a_(j,i,t));
	NN=NN|extensionMatrix(L_j,L_i,LL)));
     	M=M||NN));
        da:=toList join toSequence apply(k,i->L_i+1:{-2*i,0}); 
     	db:=toList join toSequence apply(k,i->L_i:{-2*i-1,-1});
	map(S^da,S^db,M)
	-- Berkeley, 19.12.2004, Frank Schreyer
	)"
   },
   EXAMPLE lines ///
--         loadPackage "BGG"
	 L={1,3,5,7}
	 kk=ZZ/101
	 M=setupDef(L,kk)
	 N=symmetricToExterior(M,ee,xx)
	 fN=res(coker N,LengthLimit=>7)
	 bettiT dual fN
	 diags=apply(3..7,i->(
	  	   T=fN**E^{{0,i}};
  		   Rpi=degreeZeroPart(T,A);
	  	   Rpi.dd_(i-1)))
	 apply(diags,d->bettiT chainComplex(transpose d))	  
	 diags_2
	 T=fN**E^{{0,3}}[2];
	 Rpi= degreeZeroPart(T,A)
	 bettiT dual(Rpi)
	 bettiT dual(T)
     ///
     }


///
restart
loadPackage "BGG"
installPackage BGG
--installPackage "Style"
viewHelp

--Programs from The Book
load "BGG.m2"

S=ZZ/32003[x_0..x_2];
E=ZZ/32003[e_0..e_2,SkewCommutative=>true];
M=coker matrix{{x_0^2, x_1^2}};
m=presentation truncate(regularity M,M);
symExt(m,E)
M=cokernel matrix{{x_0^2, x_1^2, x_2^2}};
bgg(1,M,E)
m = matrix{{x_0,x_1}};
regularity coker m
T = tateResolution(m,E,-2,4)
betti T
T.dd_1
S=ZZ/32003[x_0..x_3];
E=ZZ/32003[e_0..e_3,SkewCommutative=>true];
m=koszul(3,vars S);
regularity coker m
betti tateResolution(m,E,-6,2)
betti sheafCohomology(m,E,-6,2)
M=sheaf coker m;
HH^1(M(>=0))
S = ZZ/32003[x_0..x_2];
U1 = coker koszul(3,vars S) ** S^{1};
k2 = koszul(2,vars S)
alpha = map(U1 ++ U1, S^{-1}, transpose{{0,-1,0,1,0,0}});
alphad = map(S^1, U1 ++ U1, matrix{{0,1,0,0,0,1}} * (k2 ++ k2));
F = prune homology(alphad, alpha);
betti  F
S=ZZ/32003[x_0..x_3];
E=ZZ/32003[e_0..e_3,SkewCommutative=>true];
koszul(2,vars S)
sortedBasis(2,E)
beilinson1(e_1,1,3,S)
beilinson1(e_1,1,2,S)
beilinson1(e_1,1,1,S)
S=ZZ/32003[x_0..x_2];
E = ZZ/32003[e_0..e_2,SkewCommutative=>true];
alphad = map(E^1,E^{-1,-1},{{e_1,e_2}})
alpha = map(E^{-1,-1},E^{-2},{{e_1},{e_2}})
alphad=beilinson(alphad,S);
alpha=beilinson(alpha,S);
F = prune homology(alphad,alpha);
betti  F
S = ZZ/32003[x_0..x_4];
E = ZZ/32003[e_0..e_4,SkewCommutative=>true];
beta=map(E^1,E^{-2,-1},{{e_0*e_2+e_1*e_3,-e_4}})
alpha=map(E^{-2,-1},E^{-3},{{e_4},{e_0*e_2+e_1*e_3}})
beta=beilinson(beta,S);
alpha=beilinson(alpha,S);
G = prune homology(beta,alpha);
betti res G
foursect = random(S^4, S^10) * presentation G;
IX = trim minors(4,foursect);
codim IX
degree IX
codim singularLocus IX
alphad = matrix{{e_4*e_1, e_2*e_3},{e_0*e_2, e_3*e_4},
                {e_1*e_3, e_4*e_0},{e_2*e_4, e_0*e_1},
                {e_3*e_0, e_1*e_2}};
alphad=map(E^5,E^{-2,-2},alphad)
alpha=syz alphad
alphad=beilinson(alphad,S);
alpha=beilinson(alpha,S);
FHM = prune homology(alphad,alpha);
betti res FHM
regularity FHM
betti sheafCohomology(presentation FHM,E,-6,6)
sect =  map(S^1,S^15,0) | random(S^1, S^4);
mapcone = sect || transpose presentation FHM;
fmapcone = res coker mapcone;
IX =  trim ideal fmapcone.dd_2;
codim IX
degree IX
codim singularLocus IX
///

end

randomMap = (F,G) -> (
     R := ring F;
     H := Hom(F,G);
     Hdeg0 := basis(0,H);
     randomf := Hdeg0 * random(source Hdeg0, R^1);
     homomorphism randomf)

randomVanishingIdeal = (F,G) -> (
     randomf := randomMap(F,G);
     presentIX := presentation cokernel randomf;
     sz := syz transpose presentIX;
     if numgens source sz =!= 1 then
       << "warning: expected syzygy to be a (twisted) ideal" << endl;
     trim ideal sz)

loadPackage "BGG"
KK=ZZ/32003
E=KK[e_0..e_4,SkewCommutative=>true];
S=KK[x_0..x_4]; 

-- veronese surface in P^4
F = random(E^{-1},E^{-4,-4,-4})
G = beilinson(F,S)
Z = syz transpose presentation cokernel G
J = trim ideal Z
res J
betti oo

-- rat.d7.g4.new
F = random(E^{-1,0},E^{-4,-4,-4,-4})
G = beilinson(F,S)
Z = syz transpose presentation cokernel G
J = trim ideal Z
res J
betti oo
degree J
genera comodule J

-- rat.d8.g5.new
F = random(E^{-1,-1,0,0,0,0,0},E^{-2,-2})
G = beilinson(F,S)
Z = syz transpose presentation cokernel G
J = trim ideal Z
res J
betti oo
degree J
genera comodule J

-- rat.d10.g9.quart1.new
F = random(E^{-1,-1,0},E^{-3,-3})
G = beilinson(F,S)
Z = syz transpose presentation cokernel G
J = trim ideal Z
res J
betti oo
degree J
genera comodule J

-- K3 surface
F = random(E^{-2,-2,-2},E^{-3,-3,-3,-3,-4})
G = beilinson(F,S)
Z = syz transpose presentation cokernel G;
J = trim ideal Z;
res J
betti oo
degree J
genera comodule J
J = randomVanishingIdeal((U(3,S))^4 ++ U(4,S), (U(2,S))^3)
betti J
res J

-- k3.d8.g6
F = S^{-2,-1,-1}
G = U(1,S)
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J
--       total: 1 8 11 5 1
--           0: 1 .  . . .
--           1: . .  . . .
--           2: . .  . . .
--           3: . 8 11 5 1

-- k3.d9.g8
F = U(4,S) ++ U(3,S)
G = S^6
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J
--       total: 1 6 6 1
--           0: 1 . . .
4--           1: . . . .
--           2: . . . .
--           3: . 6 6 1

-- k3.d11.g12
F = S^{-1} ++ (U(3,S))^3
G = (U(2,S))^2 ++ S^2
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J

-- k3.d12.g14
F = S^{-1} ++ (U(3,S))^4
G = (U(2,S))^3
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J

-- ell.d7.g6
F = S^{-2,-2}
G = S^{-1,-1,1}
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J

-- ell.d8.g7
F = S^{-1,-1}
G = S^{0,1,1}
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J

-- ell.d9.g7
F = S^{-1} ++ (U(2,S))^2
G = (U(1,S))^3 ++ S^2
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J

-- ell.d10.g10
F = S^{-1,-1} ++ U(3,S)
G = U(1,S) ++ S^3
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J

-- ell.d11.g12
F = S^{-1,-1} ++ (U(3,S))^2
G = U(2,S) ++ U(1,S) ++ S^1
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J

-- HP of U
S = ZZ/32003[a..e]
P0 = hilbertPolynomial(S^1,Projective=>true)
P1 = hilbertPolynomial(U(1,S) ** S^{-4},Projective=>true)
P2 = hilbertPolynomial(U(2,S) ** S^{-4},Projective=>true)
P3 = hilbertPolynomial(U(3,S) ** S^{-4},Projective=>true)
P4 = hilbertPolynomial(U(4,S) ** S^{-4},Projective=>true)

P0+3*P4-P1
P0-3*P2+4*P3+P4

matrix{{17,27,19,5}}
LLL syz oo
ker ooo
LLL gens oo

-P1+P2-2*P4
-P1-2*P2+4*P3-P4
-2*P1+3*P2-3*P3+2*P4

-- doesn't work (a=b=0, d=11, g=11)
F = U(1,S) ++ (U(2,S))^2 ++ U(4,S)
G = (U(3,S))^4
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J

gg = (a,b) -> 
  {-1,-2,4,-1} + a * {-1, 1, 0, -2} + b * {-2, 3, -3, 2}
gg(1,1) -- {-4,2,1,-1}
randomVanishingIdeal((U(2,S))^2 ++ U(3,S), (U(1,S))^4 ++ U(4,S))
gg(1,2) -- {-6, 5, -2, 1}
randomVanishingIdeal((U(2,S))^5 ++ U(4,S), (U(1,S))^6 ++ (U(3,S))^2)
random(E^{-1,-1,-1,-1,-1,-1,-3},E^{-2,-2,-2,-2,-2,-4})
beilinson(oo,S)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages NAMEOFPACKAGE=BGG install-one"
-- End:

