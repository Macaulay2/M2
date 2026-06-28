newPackage(
    "NumericalSemigroups",
    Version => "1.0",
    Date => "June 8, 2026",
    Headline => "Compute the Apery set and invariants of a numerical semigroup ring",
    Authors => {{ Name => "David Eisenbud",
	         Email => "de@berkeley.edu",
	         HomePage => "https://eisenbud.github.io"},
	        { Name => "Frank-Olaf Schreyer",
		  Email => "schreyer@math.uni-sb.de",
		  HomePage => "https://www.math.uni-sb.de/ag/schreyer/index.php/publications/publications-frank-olaf-schreyer"},
		{ Name => "Theodore Wittmer Lysek", Email => "tlysek44@gmail.com"}},
    AuxiliaryFiles => false,
    DebuggingMode => false,

    PackageExports => {
	"Complexes",
	"Posets",
	"FourierMotzkin",
	"Normaliz",
	"IntegralClosure",
	},
    Keywords => {"Commutative Algebra", "Algebraic Geometry", "Combinatorics"}

    )
///
restart
--TODO: fix, re-add syzDecomp; add tests for the arf stuff. change debug mode to false
loadPackage("NumericalSemigroups", Reload => true)
///
export { 
    "apery", --TEST created FOS
    "aperySet", --TEST created FOS
    "semigroup", -- TEST created FOS
    "isSymmetric",-- TEST created FOS
    "mu", --TEST created
    "semigroupFromMu",
    "semigroupsFromMatrix",
    "socle",--TEST created last
    "type", -- TEST created
    "kunzMatrix", --TEST created FOS
    "semigroupIdeal", --TEST created
    "semigroupRing", -- FOS: take a look
    "aperySemigroupRing", -- TEST created
    "gaps", -- TEST created
    "isGapSequence", --TEST created
    "findSemigroups", --TEST created
    "sums", --TEST created
    "aperyConeEquations",
    "muConeEquations",
    "coneRays",
    "aperyFacetRays", -- returns a list
    "muFacetRays", -- returns a matrix
    "allSemigroups", -- list semigroups with given genus etc
    "randomSemigroup",
    "def1", --TEST created -- degrees of first order deformations, or the generic first-order deformation itself.
    "weight",
    "ewt", --effective weight
    "effectiveWeight", -- synonym for ewt
    "kunzRing",--done FOSthe semigroup ring of R mod the multiplicity element
    "burchIndex", --done
    "buchweitz", --done
    "buchweitzCriterion", --done
    "buchweitzSemigroups", --done
    "fractionalIdeal", --done
    "infinitelyNearSemigroups",
    "infinitelyNearModules",
    "isArf",
    "arfClosure",
    "isMinimalMultiplicity",
    "arfIndex",
    "reduceByList",
    "kunzPoset",
    "syzFormat",
    "isKnownExample",
    ------seems to need the following, whose code I've brought from badNumericalSemigroups
    "facetRays",
    ----------Testing smoothability
--    "flatteningRelations", --TEST created. EXAMPLES USE THIS ONE AND THERE'S CODE FOR IT, SO THEO IS INCLUDING FOR NOW
}

-* Code section *-

shape = method()
shape Complex := F -> (
    apply(1+length F, i-> rank F_i))
shape Module := M ->(
    n := numgens ring M;
    F := res(M, LengthLimit => n+1);
    shape F)
    
    

aperyConeEquations = method()
aperyConeEquations ZZ := Matrix => m -> (
    --Returns a matrix whose columns
    --are the coefficients of the
    --inequalities defining the (homogeneous)
    --Kunz cone of Apery sets for multiplicity m
    --the column indices are the lexicographically ordered pairs {i,j}
    --such that with 1<= i<=j<=m-1 and i+j != m.
    --The output is an matrix M with m-1 rows
    --such that if E*matrix{aperySet L}
    --is a row of non-negative integers for
    --any semigroup L of multiplicity m.
    cols :=  flatten for i from 1 to m-1 list
        for j from i to m-1 list {i,j};
    cols = select(cols, p -> sum p != m);
    Eq := transpose matrix apply(cols, p -> (
	    eqj := new MutableList from toList (m-1:0);
	    eqj#(p_0-1) = 1+ eqj#(p_0-1);
	    eqj#(p_1-1) = 1+ eqj#(p_1 -1);
	    eqj#((p_0+p_1)%m -1) = -1;
	    toList eqj));
    Eq
)        

aperyConeEquations List := Matrix => L ->(
    -- If
    -- M = aperyConeEquations (m = mult L),
    -- then aperyConeEquations L
    --returns M|M', whose columns
    --represent the inequalities
    --defining the face on which L lies.

    --The output is an matrix M with m-1 rows
    --such that if E*matrix{aperySet L}
    --is a row of non-negative integers for
    --any semigroup L of multiplicity m
    --that satisfies the same equalities
    --that L does.
    A := aperySet L;
    m := min L;
    M := aperyConeEquations m;
    cols :=  flatten for i from 1 to m-1 list
        for j from i to m-1 list {i,j};
    cols = select(cols, p -> sum p != m and
	A_(p_0-1)+A_(p_1-1) == A_((p_0+p_1)%m-1));
    if cols == {} then return M;
    ML := transpose matrix apply (cols, p -> (
	    eqj := new MutableList from toList (m-1:0);
	    eqj#(p_0-1) = -1+ eqj#(p_0-1);
	    eqj#(p_1-1) = -1+ eqj#(p_1 -1);
	    eqj#((p_0+p_1)%m -1) = 1;
	    toList eqj));
    M|ML)

muConeEquations = method()
muConeEquations ZZ := List => m ->(
    -- Returns M, where
    -- M is a matrix with m rows.
    -- The first m-1 rows are the
    -- aperyConeEquations
    -- and the last row gives the
    -- constants in the inHomogeneous
    -- inequalities satisfied by
    -- mu L for all semigroups L
    -- of multiplicity m
    c := matrix{flatten for i from 1 to m-1 list (
	        for j from i to m-1 list(
		    if i+j == m then continue;
		    if i+j < m then 0 else 1
		    ))};
    E := aperyConeEquations m;
    E||c
    )
muConeEquations List := L -> (
    m := min L;
    Ec := muConeEquations m;
    cols :=  flatten for i from 1 to m-1 list
        for j from i to m-1 list {i,j};
    A := aperySet L;
    cols = select(cols, p -> sum p != m and
	A_(p_0-1)+A_(p_1-1) == A_((p_0+p_1)%m-1));
    if cols == {} then return Ec;
    ML := transpose matrix apply (cols, p -> (
	    eqj := new MutableList from toList (m-1:0);
	    eqj#(p_0-1) = -1+ eqj#(p_0-1);
	    eqj#(p_1-1) = -1+ eqj#(p_1 -1);
	    eqj#((p_0+p_1)%m -1) = 1;
	    toList eqj));
    n := numcols ML;
    cL:= map(ZZ^1, ZZ^n, 0);
    Ec | (ML||cL)
    )
///

restart
loadPackage("NumericalSemigroups", Reload => true)
A1 = aperySet {3,4,5}
A2 = aperySet {3,5}
A3 = aperySet {3,7}
matrix{A1}* aperyConeEquations 3 
matrix{A1}* aperyConeEquations {3,4,5}
matrix{A1}*aperyConeEquations {3,5}
matrix{A2}*aperyConeEquations {3,7}
aperyConeEquations {3,7}
matrix{A3}*aperyConeEquations {3,7}

mu1 =matrix {mu {3,4,5}|{1}}
mu2 = matrix {mu {3,5}|{1}}
mu3 = matrix{mu {3,7}|{1}}
mu3*muConeEquations 3
mu2*muConeEquations {3,5}
mu3*muConeEquations {3,5}
mu3*muConeEquations {3,7}
///

isOnSameFace = method()
isOnSameFace(List, List) := (L,L') ->(
    m := min L;
    if m != min L' then
        error"semigroups must have the same multiplicity";
    E := (aperyConeEquations m);
    t := matrix{aperySet L} * E;
    t' := matrix{aperySet L'} * E;
    positions(t, tt -> tt === 0) ==
    positions(t', tt -> tt === 0)
    )
///
restart
loadPackage("NumericalSemigroups", Reload => true)
aperyConeEquations 4
L = {4,9,14,7}
allSemigroups 3
aperyConeEquations 3
coneRays 3
allSemigroups 5
aperyConeEquations 5
muConeEquations L
betti((allSemigroups 5)_1)
///


allSemigroups = method()
allSemigroups ZZ := Sequence => m -> (
    -*
    Returns a Sequence of two matrices:
    The rows of the first form a
    Hilbert basis for the homogeneous Kunz cone
    of semigroups of multiplicity m, and
    a set of module generators for the inhomogeneous
    Kunz cone.
    *-
ineq := transpose aperyConeEquations m;
cong:=id_(ZZ^(m-1))|(-1)*transpose matrix{toList(1..m-1)}|transpose matrix{toList(m-1:m)};
trunc:=id_(ZZ^(m-1))|(-1)*transpose matrix{toList(m+1..2*m-1)};
NL:={(ineq,"inequalities"),(cong, "inhom_congruences"),(trunc,"inhom_inequalities")};
RatCone:=normaliz NL;
M:=RatCone#"gen";
pos:=positions(toList(0..rank target M-1),i->M_(i,m-1)==0);
hilbertBasis:=M^pos_{0..m-2};
pos=positions(toList(0..rank target M-1),i->M_(i,m-1)==1);
moduleGenerators:=M^pos_{0..m-2};
(hilbertBasis,moduleGenerators)
)

--FIXME
allSemigroups List := Sequence => L -> (
-*  Returns a matrix whose rows form a
    Hilbert basis for the face containing L
    of the homogeneous Kunz cone
    of semigroups of multiplicity m; and
    a set of module generators for
    the corresponding face of the inhomogeneous
    Kunz cone.
*-
m:=min L;
ineq0 := (aperyConeEquations m);
ap:=matrix{aperySet L};
pos:=positions(toList(0..rank source ineq0-1),i->(ap*ineq0)_(0,i)==0);
eq:=transpose ineq0_pos;
pos=positions(toList(0..rank source ineq0-1),i->(ap*ineq0)_(0,i)=!=0);
ineq:=transpose ineq0_pos;
cong:=id_(ZZ^(m-1))|(-1)*transpose matrix{toList(1..m-1)}|transpose matrix{toList(m-1:m)};
trunc:=id_(ZZ^(m-1))|(-1)*transpose matrix{toList(m+1..2*m-1)};
NL:={(eq, "equations"),(ineq,"inequalities"),(cong, "inhom_congruences"),(trunc,"inhom_inequalities")};
RatCone:=normaliz NL;
M:=RatCone#"gen";
pos=positions(toList(0..rank target M-1),i->M_(i,m-1)==0);
hilbertBasis:=M^pos_{0..m-2};
pos=positions(toList(0..rank target M-1),i->M_(i,m-1)==1);
moduleGenerators:=M^pos_{0..m-2};
(hilbertBasis,moduleGenerators)
)

///
restart
loadPackage("NumericalSemigroups", Reload => true)
L = {4,7,9}
allSemigroups L
///

randomSemigroup = method()
-*
Find the Apery set of a random semigroup L' on the same open face
of the Kunz cone as L.
After (H,M) = allSemigroups L, the Apery set of L'
L' is obtained by prepending m to a random linear combination of the rows of H
(using random integers 0..b-1) plus a random row of M.
*-
randomSemigroup (List, ZZ) := (L,b) -> (
    (H,M) := allSemigroups L;
    h := numRows H;
    m := numRows M;
    M1 := M^{random m}+sum apply(h, i-> (random b) * H^{i});
    prepend(min L, flatten entries M1)
    )
randomSemigroup(ZZ,ZZ) := (mult,b) -> (
    (H,M) := allSemigroups mult;
    h := numRows H;
    m := numRows M;
    M1 := M^{random m}+sum apply(h, i-> (random b) * H^{i});
    prepend(mult, flatten entries M1)
    )
    

buchweitzCriterion = method()
buchweitzCriterion(List) := L -> (
    G:=gaps L;
    3*(#G-1)-#sums(G,G))

buchweitzCriterion(ZZ,List) := (m,L) -> (
    assert(#L==m-1);
    buchweitzCriterion(prepend(m,L)))


-*
--what succeeded:



restart
loadPackage("NumericalSemigroups", Reload => true)
L = {4,9,14,7}

(B,G) = allSemigroups 4
(BL,GL) = allSemigroups L
(B,G) =allSemigroups {4,5}
L={6,8,10,13}
mingens L
elapsedTime (H,M)=allSemigroups L
aperySet L==first entries H+first entries M
L={10,13,15,16,18}, type L
elapsedTime (H,M)=allSemigroups L
tally flatten apply(entries M,L->buchweitzCriterion (10,L))
tally flatten apply(entries M,L->apply(entries H,h->buchweitzCriterion(10,L+h)))


L=buchweitz 0
elapsedTime (H,M)=allSemigroups L
betti H, betti M
H
showNmzOptions()
elapsedTime (H,M)=allSemigroups 8;
betti H, betti M

i14 : elapsedTime (H,M)=allSemigroups 9;
betti H, betti M

m=9
elapsedTime (H,M)=allSemigroups m;
betti H, betti M
-- 268.992 seconds elapsed

i15 : 
                 0 1             0 1
o15 = (total: 6943 8, total: 35886 8)
          -1:    . 8     -1:     . 8
           0: 6943 .      0: 35886 .


elapsedTime tally flatten apply(entries M,L->buchweitzCriterion (L))

 -- 43.7384 seconds elapsed

o16 = Tally{0 => 3093}
            1 => 3803
            2 => 4864
            3 => 5310
            4 => 5211
            5 => 3852
            6 => 3575
            7 => 2474
            8 => 1517
            9 => 1086
            10 => 575
            11 => 308
            12 => 150
            13 => 42
            14 => 21
            15 => 3
            16 => 2




elapsedTime tally flatten apply(entries M,L->buchweitzCriterion (m,L))
tally flatten apply(entries M,L->apply(entries H,h->buchweitzCriterion (m,L+h)))



elapsedTime LB=buchweitzSemigroups(13,17)|buchweitzSemigroups(14,17)
buchweitzSemigroups(15,17)
flatten apply(LB,L->buchweitzCriterion (L))
netList apply(LB,L->((H,M)=allSemigroups L;tally flatten apply(entries H,g->
	    apply(entries M,b->buchweitzCriterion(min L,b+g)))))
netList apply(LB,L->((H,M)=allSemigroups L;(L,M^{0})))
b=toList(14..25)
netList apply(LB_{0..3},L->((H,M)=allSemigroups L; g= first entries H;apply(16,i->genus({13}|(b+i*g)))))
netList apply(LB_{0..3},L->((H,M)=allSemigroups L; g= first entries H;apply(16,i->buchweitzCriterion(13,b+i*g))))


b=toList(15..27)
apply(LB_{4..5},L->((H,M)=allSemigroups L; g= first entries H;apply(16,i->buchweitzCriterion(14,b+i*g))))

elapsedTime LB=buchweitzSemigroups(12,18)
elapsedTime LB=buchweitzSemigroups(13,18)  -- 47.2556 seconds elapsed
elapsedTime LB=buchweitzSemigroups(14,18)  -- 14.8381 seconds elapsed
elapsedTime LB=buchweitzSemigroups(15,18)  -- 4.35668 seconds elapsed
elapsedTime LB=buchweitzSemigroups(16,18)=={}  -- 1.19864 seconds elapsed

elapsedTime LB=buchweitzSemigroups(16,19) 
netList
N=matrix apply(LB_{0},L->((H,M)=allSemigroups L; (g= first entries H//16)))
fourierMotzkin transpose N
viewHelp "fourierMotzkin"

restart
debug loadPackage("NumericalSemigroups", Reload => true)
*-


aperyFacetRays=method()

aperyFacetRays(List) := Matrix => L -> (
    --If L is the list of generators of a semigroup, this returns
    --a matrix whose columns are the rays of the homogeneous
    --Kunz cone that contain L. Note that this makes
    --no particular sense, since L is really an element of
    --the inhomogeneous Kunz cone.
    L1:=if L_0==0 then drop(L,1) else L; 
    A:=apery L1;
    m:=A#"multiplicity";
    halfSpaces := aperyConeEquations m; -- homogeneous cone eqns
    a:=matrix{aperySet A};
    eqpos:=positions(toList(0..rank source halfSpaces-1),
	i->(a*halfSpaces)_{i}==0);
    linEq:=halfSpaces_eqpos;
    halfFacet:=halfSpaces%linEq;
    fRays:=-(fourierMotzkin(halfFacet,linEq))_0;
    fRays
    )
///
restart
loadPackage("NumericalSemigroups", Reload => true)
L = {5,6,7,8,9}
aperySet L
A = aperyFacetRays {5,6,7,8,9}
m = 6
AA = coneRays m
for i from 0 to numcols AA -1 list(
    A = flatten entries AA_{i};
    A' = set apply(A, a -> a%m);
    A' == set toList (1..m-1)
    )
aperyFacetRays {6,9,13,16}
rank oo
M = (allSemigroups {6,9,13,16})_1
LL =  apply(numrows M, i -> mingens({6}|(flatten entries M^{i})))
netList apply(LL, L -> syzFormat L)
netList apply(LL, L -> kunzMatrix L)
code methods kunzMatrix

///
isAperySet = method()
isAperySet(ZZ, List) := Boolean => (m, A) ->(
--test whether the list A is the Apery set of
--a semigroup of multiplicity m
    if #A != m-1 then return false;
    all apply(m-1, i -> A_i % m == i+1)
    )

-*
///rather than computing the mucone separately, just translate to
the homogeneous Apery cone
///
muFacetRays=method()
muFacetRays(List) := Matrix => muL -> (
    --matrix whose columns are the rays of the in homogeneous
    --Kunz cone that contain muL
    mmu := muL;
    m :=#mmu+1; --multiplicity
    halfSpaces := aperyConeEquations m; -- homogeneous cone eqns
--    halfSpaces = aperyConeEquations L;
    --note that the mu's satisfy the INhomogeneous cone eqns
    eqpos:=positions(toList(0..numcols halfSpaces-1),
	i->(matrix{mmu}*halfSpaces)_{i}==0);
    linEq:=halfSpaces_eqpos;
    halfFacet:=halfSpaces%linEq;
    fRaysMu := -(fourierMotzkin(halfFacet,linEq))_0;
    fRaysMu
    )
///
needsPackage"NumericalSemigroups"
L = {3,5,7}
muL = mu L
assert(isMember (L, semigroupsFromMatrix muFacetRays L))
semigroupsFromMatrix muFacetRays L
 ///
*-

facetRays = method()
facetRays(List) := L -> (
    L1:=if L_0==0 then drop(L,1) else L; 
    A:=apery L1;
    m:=A#"multiplicity";
    halfSpaces := coneEquations m;
    a:=matrix{aperySet A};
    eqpos:=positions(toList(0..rank source halfSpaces-1),i->(a*halfSpaces)_{i}==0);
    linEq:=halfSpaces_eqpos;
    halfFacet:=halfSpaces%linEq;
    fRays:=-(fourierMotzkin(halfFacet,linEq))_0;
    fRays
    )


coneEquations = method(Options => {"Inhomogeneous" => false})
coneEquations ZZ := Matrix => o -> m -> (
    -- Forms the equations of the homogeneous or inhomogeneous
    --Kunz cone for multiplicity m
    --the columns are the lexicographically ordered pairs {i,j}
    --such that with 1<= i<=j<=m-1
    --i+j != m.
    --if o#"Inhomogeneous" == true, then the equations
    --for the inhomogeneous Kunz cone are returned
    cols :=  flatten for i from 1 to m-1 list
        for j from i to m-1 list {i,j};
    cols = select(cols, p -> sum p != m);
    n := #cols;
    eqns := map(ZZ^(m-1), ZZ^n, (i,j) -> (
	if (sum cols_j -(i+1)) % m == 0 then -1 else if
	    cols_j == {i+1,i+1} then 2  else if
	       isMember(i+1, cols_j) and
	       cols_j !={i+1,i+1} then 1 else 0));
    if o#"Inhomogeneous" then
      eqns || matrix{apply(n, j->
	       if sum cols_j < m then 0 else -1)} else
       eqns
  )

coneEquations List := Matrix => o -> L ->(
    K := kunzMatrix L;
    m := 1 + numrows K;
    --get the column indices of E
    cols :=  flatten for i from 1 to m-1 list
        for j from i to m-1 list {i,j};
    cols = select(cols, p -> sum p % m !=0);
    --find the positions of the cone equations that are satisfied by L
    pos := positions(cols, c -> K_(c_0-1, c_1-1) != 0);
    coneEquations(m,o)|(-(coneEquations (m, o))_pos)
    )



semigroupFromMu = method()
semigroupFromMu List :=  mmu -> (
    mult := #mmu+1;
    mingens({mult}| apply(mult-1, i-> i+1+ mult*mmu_i))
    )

semigroupsFromMatrix = method()
semigroupsFromMatrix Matrix :=  M -> (
    --columns of M should represent the mu vectors
    --of semigroups of multiplicity = mult;
    --for example, the output of facetRaysMu L
    c := numcols M;
    apply(c, j -> (
	    v := flatten entries M_{j};
	    if min v == 0 then v = apply(#v, i-> 1+v_i);
	    semigroupFromMu v
	    )
	)
    )

TEST///-*semigroupFromMu, semigroupsFromMatrix*-
debug needsPackage "NumericalSemigroups"
L = {6,9,13,16}
assert(L == semigroupFromMu mu L)
assert(coneRays 4 == facetRays {4,5,6,7})

///

coneRays = method()
coneRays ZZ := Matrix => m -> -(fourierMotzkin(aperyConeEquations m))_0
///
fourierMotzkin aperyConeEquations 4
///

findSemigroups = method()
findSemigroups(ZZ,ZZ,ZZ) := (m,c,g) ->(
    --list all semigroups with multiplicity m, conductor c, and precisely g gaps
    if g<m-1 or (c-1)%m == 0 then return {};
    candidates := for i from m+1 to c-2 list(
	if i%m != 0 then i else continue);
    if m+#candidates < g then return{};
    GC := subsets(set candidates, g-m);
    GG := apply(GC, gc -> gc+set(1..m-1)+set{c-1});
      for G in GG list (
	if #G <g then return{};	  
        s := isGapSequence(toList G);
	if class s === List and gcd s == 1 then(
        mingens unique (s|apply(m, i->c+i))) else continue)
)

findSemigroups (ZZ,ZZ) := (m,g) -> (
    --list all semigroups of multiplicity m with g gaps. 
   L := {};
	 for c from m to 2*g do(
--			<<"c = "<<c<<endl<<flush;
			if (c-1)%m !=0 then (
			    ell := findSemigroups(m,c,g);
			    if ell !={} then L = L |ell));
    L
    )

findSemigroups ZZ := g -> (
    --list all semigroups with g gaps.
   L0 := mingens toList(g+1..2*g+1);
   L := {L0};
    for m from 2 to g+1 do (
	    for c from m to 2*g do(
			if (c-1)%m !=0 then (
			    ell := findSemigroups(m,c,g);
			    if ell !={} then L = L |ell)));
    L
    )

///
findSemigroups(3,8,4) == {{3,5}}
findSemigroups(5,14,8) == {{5,7,9}, {5, 9, 11, 12}} 
findSemigroups(5,14,8)
assert(findSemigroups(3,8,4) == {{3,5}})
assert(findSemigroups(5,14,8) == {
	{5, 7, 11},
	{5, 7, 9},
	{5, 6, 14},
	{5, 9, 11, 12}})

m = 12
elapsedTime ss = findSemigroups(m,17);
for L in ss list (if #sums(2,L)> 3*17-3 then L else continue)
///

///
loadPackage( "NumericalSemigroups", Reload => true)
elapsedTime ss = findSemigroups 17;
#ss
elapsedTime bss = for L in ss list (
    G = gaps L;
    g = #G;
    if #sums(2,G) > 2*(2*g-2)-g+1 or
    #sums(3,G) > 3*(2*g-2)-g+1 or
    #sums(4,G) > 4*(2*g-2)-g+1 then L else continue)

     netList apply(bss, L -> (LA = aperySet L;
	     m := min L;
     apply(#LA, i -> (LA_i-i-1)//min L))
)
L = {1,3}
sums(L,L)
sums(2,L)
///

isGapSequence = method()
isGapSequence List := List => G'-> (
    --if the complement in the integers defines a semigroup s then return s; else
    --return false
    G := sort G';
    c := 1+ max G;
    s := select(c+1, i ->i>0 and not isMember(i, G));
    m := min s;
    s = s|apply(m-1,i->c+i+1);
    if G == gaps (L := mingens s) then L else false
	)

///
restart
debug loadPackage("NumericalSemigroups", Reload => true)
ss = for g from 3 to 16 list elapsedTime findSemigroups g;
#flatten ss
ssplus = for g from 3 to 16 list ({g}|ss_(g-3));

g = ssplus_1_0
sg = drop(ssplus_1,1)
elapsedTime sse = for s in flatten ss list(
    t := false;
    g = #gaps s;
    for n from 2 to 4 list(
	if #sums(n, gaps s)>n*(2*g-2)-g+1 then t = true);
    if t == true then s else continue); #sse

    for n from 2 to 4 list
	#sums(n, gaps s)>(2*(n+1)-1)*(g-1)
	
	(2*nxzz1)-1)*(g-1)	
n = 4
	    
s = first oo
n = 3
#sums(n,gaps s)
(2*n-1)*(g-1)

weight toList(g+1..2*g+1)
gaps toList(g+1..2*g+1)

///


gaps = method()
gaps List := List => L -> (
    --list of gaps in the semigroup generated by L
    A := apery L;
    c := A#"conductor";
    s := A#"semigroup";
    m := min L;
    select(c, i -> not isMember(i, s))
    )

///
restart
loadPackage("NumericalSemigroups", Reload => true)
g = 7
weight toList(g+1..2*g+1)
gaps toList(g+1..2*g+1)
///

weight = method()
weight List := L ->(
    G := sort gaps L;
    sum apply (#G, i -> G_i-1 - i)
    )

sums = method()
--sums List := L -> sort unique flatten apply(L, i-> apply(L, j-> i+j))
sums (List, List) := (L,L') -> sort unique flatten apply(L, i-> apply(L', j-> i+j))

sums (ZZ,List) := (n,L) ->(
    L' := L;
    for i from 1 to n-1 do (L' = flatten apply(L', j -> apply(L, k -> j+k)));
    sort unique L')

///
restart
loadPackage("NumericalSemigroups", Reload => true)
apery({3,4}, "UpperBound" => 10)
semigroup({3,4}, "UpperBound" => 10)
///
apery = method(Options => {"UpperBound" => 0})
apery List := HashTable => o -> L ->(
    --require gcd = 1;
    if gcd L != 1 then error"requires relatively prime generating set";
    A := new MutableHashTable;
    --A will hold the apery set, and a few invariants of the semigroup derived along the way
    m := min L; -- the multiplicity
    a := 0; -- number of keys already filled in A
    S := set L + set{0}; -- S will hold all the semigroup elmeents found, including 0

    --now look for new Apery set elements and semigroup elements until
    --all the keys have been filled.
    s := m;
    while a < m-1 do(
	s = s+1;
	t := any(L, u -> isMember(s-u, S));
	if t then (
	    S = S + set {s};
	    s' := s % m;
	    if not A#?s' and not s'== 0 then (
		A#s' = s;
		a  = a+1)
	    )
	    );

    A#"multiplicity" = m;
    A#"semigroup" = sort toList S;
    A#"conductor" = max A#"semigroup" - m +1;
    if o#"UpperBound" > A#"conductor" then
         A#"semigroup" =
	 A#"semigroup"|
	     toList(A#"conductor"+m..o#"UpperBound");
    hashTable pairs A
    )

aperySet = method()
aperySet HashTable := List => A -> (
    K := select(keys A , k-> class k === ZZ);
    apply(sort K, k -> A#k)
    )
aperySet List := L -> aperySet apery L

semigroup = method(Options => {"UpperBound"=>0})
semigroup List := List => o -> L -> (apery (L,o))#"semigroup"

--conductor = method()
conductor List := ZZ => L -> (apery L)#"conductor"

isSymmetric = method()
isSymmetric List := Boolean => L -> (
    A := apery L;
    m := A#"multiplicity";
    c := A#"conductor";
    sgrp := #A#"semigroup";
    c == 2*(sgrp - m)
    )

mu = method()
mu List :=List => L -> (
    As := aperySet L;
    m := min L;
    for i from 0 to m-2 list (As_(i) - (i+1))//m
    )
mu HashTable := List => H -> (
    --H should be apery L
    As := aperySet H;
    m := H#"multiplicity";
    for i from 0 to m-2 list (As_(i) - (i+1))//m
    )
--genus = method()
genus List := ZZ => L -> sum mu L
--number of gaps

--positiveResidue = (p,q) -> if p<0 then p + (1+abs p//q)*q else p%q -- assumes q>0
--needs a version where c is known.
--mingensSemigroup = method()
--mingensSemigroup List := s -> (
mingens List := List => o-> s ->( --note that mingens, defined in the system, has options, so the o -> is necessary
    s' := select (s, a -> a != 0);
    g := gcd s';
    if g != 1 then s' = apply(s', a -> a//g);
    m := min s';
    s' = aperySet s';
    out :={};
    for i from 1 to m-1 do
         for j from 1 to m-1 do (
	a := s'_(i-1);
	b := s'_(j-1);
	if a<=b then continue;
	if a-b >= s'_(positiveResidue(i-j-1 , m)) then out = out | {i-1}
	);
    sort ({m}|toList (set s' - set(s'_out)))
     )

///
restart
loadPackage"NumericalSemigroups"
s = semigroup {3,7}
mingens s

s={8, 10, 31, 129, 130}
mingens s
///
socle = method()
socle List := List => L' -> (
    L := mingens L';
    A := apery L;
    K := select(keys A , k-> class k === ZZ);
    aS := apply(sort K, k -> A#k); -- the Apery set
    m := A#"multiplicity";
    select(aS, a -> all(L, ell ->
	            not isMember(a+ell, aS)))
    )

///
restart
loadPackage( "NumericalSemigroups", Reload => true)
L = {1,3}
sums(L,L)
sums(3,L)
isGapSequence(G = {1, 2, 4, 7})
G ={1, 2, 8, 11}
elapsedTime isGapSequence G
///

    

eMatrix = L ->(
    A := apery L;
    m := A#"multiplicity";
    map(ZZ^(m-1), ZZ^(m-1), (i,j) -> if i+j+2 == m then 0 else A#(i+1)+A#(j+1)-A#((i+j+2)%m))
    )


kunzMatrix = method()
kunzMatrix HashTable := Matrix => A -> (
    m := A#"multiplicity";
    map(ZZ^(m-1), ZZ^(m-1), (i,j) -> (
	    if i+j+2 == m then 0 else (
		a := A#(i+1)+A#(j+1)-A#((i+j+2)%m);
		if a == 0 then 1 else 0)
    )))
-- kunzTable HashTable := Table => A -> (
--     m := A#"multiplicity";
--     table(m-1, m-1, (i,j) -> (
-- 	    if i+j+2 == m then "-" else (
-- 		a := A#(i+1)+A#(j+1)-A#((i+j+2)%m);
-- 		if a == 0 then 1 else 0)
--     )))
    

kunzMatrix List := Matrix => L -> kunzMatrix apery L

type = method()
type List := ZZ => L -> #socle L

semigroupRing = method(Options => {"BaseField" => ZZ/10007,
	                           "VariableName" => getSymbol "x",
			           "MinimalGenerators" => true})
			   
semigroupRing List := Ring => o-> L -> (
    if L == {1} then return o#"BaseField"[o#"VariableName"];
    I := semigroupIdeal(L,o);
    R := (ring I)/I;
    R.cache#"sgrp" = L;
    R
    )
semigroupIdeal = method(Options => {"BaseField" => ZZ/10007,
	                           "VariableName" => getSymbol "x",
			           "MinimalGenerators" => true})

kunzRing = method(Options => {"BaseField" => ZZ/10007,
                           "VariableName" => getSymbol "x",
		           "MinimalGenerators" => true}) 
kunzRing List := Ring => o -> L -> (
    --returns the semigroup ring of R mod the multiplicity element
    R := semigroupRing(L,o);
    m := min(gens R/degree);
    newvars := select(gens R, x -> degree x > m);
    newdegs := select(gens R/degree, d -> d>m);
    S := coefficientRing R[newvars, Degrees => newdegs];
    S/trim sub(ideal R, S)
    )

semigroupIdeal List := Ideal => o -> L -> (
    --Here the variables correspond to the given semigroup generators.
    if o#"MinimalGenerators" == true then L':= mingens L else L' = L;
    m := min L';
    --t := #L;
    x := o#"VariableName";
    kk := o#"BaseField";
    R := kk[apply(L',i-> x_(i%m)),Degrees => L'];
    t := symbol t;
    R1 := kk[t];
    I := trim ker map(R1, R, apply(L', i -> t^i));
    I.cache#"sgrp" = L;
    I
        )


///
restart
loadPackage("NumericalSemigroups", Reload => true)
LL = {{3,5},{3,4,5},{3,4}}
L={4,5}
assert all(LL, L -> transpose facetRays L * aperyConeEquations L == 0)
LL = {{3,5},{3,4,5},{3,4}}
assert all(LL, L -> transpose aperyConeEquations L * facetRays L == 0)
netList apply(LL,L->ideal semigroupRing(L,symbol u))
netList apply(LL,L->ideal semigroupRing(L))

semigroupIdeal({3,4,5,6}, "VariableName"=> z, "BaseField" => ZZ/3, "MinimalGenerators"=>false)
ideal(z_0^5-z_1^4) == semigroupIdeal({4,5}, "VariableName"=> z, "BaseField" => ZZ/3, "MinimalGenerators"=>false)
///
aperySemigroupRing=method()
aperySemigroupRing List := L -> (
    A:=apery L;
    m:= A#"multiplicity";
    degs1 := apply(toList(1..m-1),i-> A#i);
    kk:= ZZ/101;
    x := symbol x;
    S1:=kk[x_0..x_(m-1),Degrees=>prepend(m,degs1)];
    t:= symbol t;
    T:= kk[t];
    phi :=map(T,S1,matrix {apply(m,i->t^((degree x_i)_0))});    
    I1 := ker phi;
    xs:=drop(sort(gens S1,y->degree y),1)|{x_0};
    S:=kk[xs, Degrees=>apply(xs,y->(degree y)_0)];
    I:=sub(I1,S);
    xs2:=flatten apply(m-1,i->apply(toList(i..m-2),j->S_i*S_j));
    xs2r:=apply(xs2,yy->yy%I);
    R := S/ideal(xs2-xs2r);
    R.cache#"sgrp" = L;
    R
    )
semigroupFromAperySet=method()
semigroupFromAperySet(List) := List => As -> (
   -- As := aperySet L;
   m:=#As+1;
    L:=prepend(m,As);       
    semigroup L)




-*
fix:we want to apply this to a basis of Hom(I/I^2, S^1/I)/Hom(Omega, S^1/I), not to the generators of
Hom(I/I^2, S/I).

In the case L = {3,4,5} the basis has 3 elements; Hom(I/I^2, S/I) has 6 generators,  isn't even 0-dimensional.
The deformation was NOT flat before factoring out (t)^2.
*-

def1 = method(Options => {"BaseField" => ZZ/101,"JustDegs" => true})--, "BaseField"})
def1 List := o -> L -> (
 --degrees of first-order deformations or the generic first-order deformation itself.
 B := semigroupRing (L, "BaseField" => o#"BaseField");
 I := ideal B;
 S := ring I;
 H := Hom(module I, S^1/I); 
 H' := Hom(S^(-L), S^1/I);
 Dmat := diff( transpose vars S, gens I);
 D := map(S^(-L),module I/I^2, Dmat);
 DI := map(S^(-L),module I, Dmat); 
 t1module := coker map(H,H',Hom(DI, id_(S^1/I))); 
 pt1 := prune t1module;
 pmap := (pt1.cache.pruningMap)^(-1);
 surj := pmap*inducedMap (t1module, H);
 bt1 := basis pt1//surj;
 if o#"JustDegs" == true then return (flatten sort degrees source bt1);
--Default: execution ends here. The rest can be slow, already for multiplicity 5. 

 h := rank source bt1;
 homs := apply(h, i-> homomorphism bt1_{i});
 degs :=  -(homs/degree)|(gens S/degree);
 t := symbol t;
 T := coefficientRing S[t_0..t_(h-1),gens S, Degrees => degs];
 Tbar := T/(ideal(t_0..t_(h-1)))^2;
 II := sub(gens I, Tbar) +sum(h, i-> Tbar_i*(map(Tbar^1, , sub(matrix homs_i, Tbar))));
 ideal(Tbar**II)
 --(h, betti res coker gens I, betti res coker (Tbar** II))
 )

t2 = method(Options => {"BaseField" => ZZ/101})
t2 List := o -> L -> (
 B := semigroupRing (L, "BaseField" => o#"BaseField");
 I := ideal B;
 S := ring I;
 prune Ext^1(I, S^1/I)
 )
    

///
restart
loadPackage ("NumericalSemigroups", Reload => true)
L = {2,3}
L = {3,4,5}
L = {5,7,11}
apery L
def1 L
B = semigroupRing L
def1 L
--Buchweitz' example
G=toList(1..12)|{19,21,24,25}
L=isGapSequence G
B = semigroupRing L
type L
S = ambient B
I = ideal B
res I
def1 L
apery L
///

///
restart
debug loadPackage("NumericalSemigroups", Reload => true)
G={1,2,3,5,6}
L=isGapSequence G
gaps L
--L = {3,5}
sum L
gaps L
R = semigroupRing (L,"a")
ideal R
I = semigroupIdeal L
isHomogeneous I
betti (F= res I)
F.dd
L = {3,4,5}
--eMatrix L
kunzMatrix L
kunzMatrix{3,5}
kunzMatrix{3,5,7}
type L    
apery L    
L = {3,7}
L = {27,35}

L = {7,8,9}
socle L
A = apery L
aperySet A
isSymmetric L
mu L
genus L
mingens L

(i,j) = (1,1)
aS_(i-1)+aS_(j-1) aS_((i+j)%m-1)
(i,j) = (0,0)
A#(i+1)+A#(j+1)-A#((i+j)%m+1)

///





ewt=method()
ewt(List):= L -> (
    G:=gaps L;
    L1:=mingens L;
    sum(L1,a->#select(G,b->a<b))
    )
effectiveWeight = method()
effectiveWeight List := sgrp -> ewt sgrp


burchIndex = method()
burchIndex List := ZZ => L -> (
    I := ideal kunzRing L;
    mm := ideal vars ring I;
    BI := (mm*I):(I:mm);
    numcols basis (mm/(BI))
    )
///
restart
loadPackage("NumericalSemigroups", Reload => true)
L = {3,4,5}
isHomogeneous kunzRing L
burchIndex {3,4,5}

///

buchweitz = method()
buchweitz ZZ := List => i -> (
    --for i>=0 this produces a semigroup B with genus 16+i, conductor 26+2i, and
    --#sums(2, gaps buchweitz i) = 3*(genus B -1)+1)
    G := toList(1..12+i)| {19+2*i, 21+2*i, 24+2*i, 25+2*i};
    isGapSequence G)

buchweitzSemigroups = method()
buchweitzSemigroups (ZZ, ZZ, ZZ) := (m,c,g) ->(
      LL := findSemigroups(m,c,g);
      if #LL !=0 then (
        LB := select(LL, L -> (
	   G := gaps L;
	   #sums(G,G) > 3*(#G -1)));
           --if #LB >0 then 
	   --     (<<LB<<endl<<flush);
	   LB) else {}
      )
  
buchweitzSemigroups (ZZ, ZZ) := (m,g) ->(
      LL := findSemigroups(m,g);
      if #LL !=0 then (
        LB := select(LL, L -> (
	   G := gaps L;
	   #sums(G,G) > 3*(#G -1)));
           --if #LB >0 then 
	   --     (<<LB<<endl<<flush);
	   LB) else {}
      )

  buchweitzSemigroups ZZ := g ->(
      LL := findSemigroups g;
      if #LL !=0 then (
        LB := select(LL, L -> (
	   G := gaps L;
	   #sums(G,G) > 3*(#G -1)));
           --if #LB >0 then 
	   --     (<<LB<<endl<<flush);
	   LB) else {}
      )

     numberToMonomial = (R, n) -> (
	 --n should be an element of the semigroup from which R is made.
	 --Thus if n \equiv i mod m, we can express m as x_0^p*x_i
	 b := basis(n, R);
	 if b==0 then error"No element of degree n in R";
	 (entries b)_0_0
	 )

fractionalIdeal = method(Options => {"Ideal" => true})
fractionalIdeal(List, List) := o -> (sgrp, mons) -> (
    --sgrp is a list of ZZ_{>0}, interpreted as generators of a semigroup;
    --mons is a list of ZZ interpreted as generating a module in the quotient field of sgrp
    --with the option "Ideal" => false we get a list of integers, otherwise an ideal.
     if not gcd sgrp == 1 then error"sgrp has wrong quotient field";
     A := apery sgrp;
     m := A#"multiplicity";
     c := A#"conductor";
     amp := max mons - min mons; --amplitude of mons
     
     sList := if amp < m then A#"semigroup"
              else A#"semigroup" |
	           toList(c+m..c+amp);
     mons0 := apply(mons, j-> j-min mons);--now starts at 0.
     mons1 := select(sums(sList,mons0),
		      i -> i <= c+amp);
--print mons0;
     shift := (i,L) -> select(sums(sList,
	               apply(L, j-> j+i)),i -> i <= c+amp) ;
     sSet := set sList;
     
     monsi := mons1;
     for i from m to c do(
	 monsi = select(apply(mons1, j -> j+i), j-> j <= c+amp);
         if isSubset(set monsi, sSet) then break);

     if not o#"Ideal" then monsi else(
     R := semigroupRing sgrp;
     --now make ans into an ideal of R.
     trim(ideal for n in monsi list numberToMonomial(R,n))
     ))


fractionalIdeal(Ring, List) := Ideal => o-> (R, mons) -> (
    --R should be a semigroup ring
    sgrp := flatten((gens R)/degree);
    I := fractionalIdeal(sgrp, mons, "Ideal" => true);
    sub(I, R)
    )

///
restart
loadPackage("NumericalSemigroups", Reload => true)
sgrp = {5,8}
R = semigroupRing {5,8}
mons = {0,5,3, 11}
fractionalIdeal(sgrp, mons, "Ideal" => false)
fractionalIdeal(sgrp, mons)
fractionalIdeal(R, mons)
fractionalIdeal(sgrp,
                semigroup({1},"UpperBound"=>32),
                "Ideal" => false)
fractionalIdeal(sgrp,
                semigroup({1},"UpperBound"=>32))

semigroup({1},"UpperBound"=> 32)
fractionalIdeal(sgrp,

semigroup sgrp
infinitelyNearSemigroups {5,8}

fractionalIdeal(sgrp, semigroup{1}, "Ideal" => false)
mons = semigroup{1}
///
infinitelyNearSemigroups = method()
infinitelyNearSemigroups List  := List => sgrp ->(
    s := sgrp;
    infNear := {s};
    while not isMember(1,s) do (
	m := min s;
	s = mingens ({m}|apply(drop(s,1), i -> i -m));
	infNear = infNear |{s}
	);
infNear 
    )

infinitelyNearModules = method()
infinitelyNearModules Ring := List => R -> (
        sgrp := flatten((gens R)/degree);
	A := apery sgrp;
	c := A#"conductor";
	m := A#"multiplicity";
	N := infinitelyNearSemigroups sgrp;
	for s in N list module(
	   fractionalIdeal(R,
	                   semigroup(s,
			             "UpperBound" => c-1)))
	)
	

///
restart
loadPackage("NumericalSemigroups", Reload => true)
   sgrp = {5,9}
   R = semigroupRing sgrp
netList (L = infinitelyNearModules R)
apply(L, I -> flatten((flatten entries gens I)/degree))
infinitelyNearSemigroups {5,9}
semigroup {4,5}
   apply(mons, n -> numberToMonomial(R,n))
   fractionalIdeal(sgrp, mons_{4..7})
   numberToMonomial(R,23)
sgrp = {4,7,10,13}
mons = {3,4} -- generators of first blowup
semigroup sgrp
R = semigroupRing sgrp
n = 7
numberToMonomial(R,7)
fractionalIdeal(sgrp, mons, "Ideal"=>false)
fractionalIdeal(sgrp, mons)

R = semigroupRing {5,8}
infinitelyNearModules R
infinitelyNearSemigroups{5,8}
///

isArf = method()
isArf List := sgrp ->(
    infNear := infinitelyNearSemigroups mingens sgrp;
    for s in infNear do
        if not #s == min s then return false;
    true)

--------------Old arfClosure --------------
-* to fix
arfClosure = method()
arfClosure List :=  L -> (
    Ls := sort L;
    arfL := {Ls_0};
    apply (#L-1,i -> arfL = (arfL |{last arfL+Ls_(i+1)}));
    arfL)
*-


--------------New arfClosure and arfIndex--------------
-* slower version
arfClosure = method()
arfClosure List := sgrp -> (
    S := semigroup sgrp;
    Snext := A(S);
    while Snext != S do (
        S = Snext;
        Snext = A(S);
    );
    mingens S
)
*-

----faster version----
arfClosure = method()
arfClosure List := sgrp -> (
    infNear := infinitelyNearSemigroups mingens sgrp;
    mults := apply(drop(infNear, -1), s -> min s);
    if #mults == 0 then return {1};
    partialSums := apply(#mults, i -> sum take(mults, i+1));
    m0 := first mults;
    sLambda := last partialSums;
    mingens (partialSums | apply(m0 - 1, i -> sLambda + i + 1)))



--as far as I can tell, there's not such an easy shortcut for the arf index, so I'm keeping this for now
arfIndex = method()
arfIndex List := sgrp -> (
    S := semigroup sgrp;
    Snext := A(S);
    count := 0;
    while Snext != S do (
        S = Snext;
        Snext = A(S);
        count = count + 1;
    );
    count
)

A = method()
A List := S -> (
    T := drop(S, 1);
    n := #T;
    maxT := T#(n - 1);
    result := new MutableHashTable;
    result#0 = true;
    for i from 0 to n - 1 do
        for j from 0 to i do
            for k from 0 to j do (
                v := T#i + T#j - T#k;
                if v <= maxT then result#v = true;
            );
    sort keys result
)

///
restart
loadPackage("NumericalSemigroups", Reload => true)
arfClosure {5,7,8}
///

isMinimalMultiplicity = t -> min t == #mingens t;
-*
syzDecomps = L -> (
netList for s in L list(
R := semigroupRing (s, "BaseField" => ZZ/32003);
(infinitelyNearSemigroups s)/(ss-> min ss);
inds := drop(infinitelyNearModules R, 1);
{s,for M in inds list reduceByList(inds,
	image((res M).dd_1))}
))
*-
///
L1 = findSemigroups 7;#L1
L2 = select(L1, s -> isMinimalMultiplicity s);#L2
L = select(L2, s -> not isArf s);#L
syzDecomps L
///
----------Old arfIndex --------------
-- arfIndex = s ->(
--     si := infinitelyNearSemigroups s;
--     ss := si_0;
--     count := 0;
--     while isMinimalMultiplicity (si_count) and count < # si-1
--            do count = count+1;
-- 	   if count == #ss-1 then count = #si;
-- count)

   needsPackage "Posets";
kunzPoset = method(Options =>
    {Jitter => true, "Display" => true})
kunzPoset List := o -> L ->(
   m:= min L;
   A := prepend(0, aperySet L);
   A' := apply(A, a-> a%m);
   c := flatten(
     for i in A list for j in A list(
	if member (j-i, L) then {i%m,j%m} else continue));
P := poset (A', c);
if o#"Display" then
displayPoset (P, SuppressLabels => false, Jitter => o.Jitter);
P
)

positiveResidue = (p,q) -> if p<0 then p + (1+abs p//q)*q else p%q -- assumes q>0

mingens List := List => o ->  s ->(
    -- the o-> is necessary because
    --mingens is defined in the system with options
    s' := select (s, a -> a != 0);
    g := gcd s';
    if g != 1 then s' = apply(s', a -> a//g);
    m := min s';
    s' = aperySet s';
    out :={};
    for i from 1 to m-1 do
         for j from 1 to m-1 do (
	a := s'_(i-1);
	b := s'_(j-1);
	if a<=b then continue;
	if a-b >= s'_(positiveResidue(i-j-1 , m)) then out = out | {i-1}
	);
    sort ({m}|toList (set s' - set(s'_out)))
     )

 syzFormat=method()
syzFormat(List) := L -> (
    I := semigroupIdeal(L,"BaseField"=>ZZ/10007);
    fI := res I;
    apply(length fI+1,i->rank fI_i)
    )

isKnownExample=method()
isKnownExample(List) := L-> (
    if #L<=3 then return true;--determinantal
    if #L == 4 and type L == 1 then return true;--pfaffian
    --if L is a generalized Arithmeti cProgression L
    --then in some cases the paper of Oneto and Tamone shows smoothness
    --if min L < 6 then return true; -- see Komeda,
    --if genus L <8 then return true; --see Komeda, J. Pure Appl. Algebra 97 (1994), no. 1, 51–71
    --if ewt L <g --Pflueger...
    g :=genus L;
    if g<8 or ewt L < g or min L < 6 then return true else false)

-* Documentation section *-

beginDocumentation()


document {
Key => NumericalSemigroups,
Headline => "Invariants of numerical semigroups",
   "Numerical semigroups are cofinite subsets of the natural numbers that are closed under sums.
   We generally refer to these simply as semigroups.
   
   A semigroup S thus includes the empty sum, 0, but we input semigroups by giving generators, all nonzero.
   The smallest nonzero element of S is the multiplicity. The Apery set (really sequence) of a semigroup S is the
   the list {a_1..a_m-1} where a_i is the smallest element in S such that a_i = i mod m.
   The conductor is 1 plus the largest element not in S. We generally specify a semigroup by giving
   a list of positive integers L with gcd = 1, representing the semigroup of all sums of
   elements of L.",
   PARA{},

     SUBSECTION "Combinatorics of Semigroups",
     UL{
 	TO apery,
	TO gaps,
	TO sums,
	TO isGapSequence,
 	TO isSymmetric,
	TO weight,
	TO effectiveWeight,
       },

     SUBSECTION "Working with the Kunz cone",
     UL{
	TO aperyConeEquations,
	TO muConeEquations,
	TO mu,
	TO semigroupFromMu,
	TO facetRays,
	TO coneRays,
	TO allSemigroups,
	TO semigroupsFromMatrix,
	TO randomSemigroup,
	TO findSemigroups,
	TO buchweitzCriterion,
	TO buchweitz,
	TO buchweitzSemigroups,
        },

     SUBSECTION "Properties of semigroup rings",
     UL{
        TO burchIndex,
	TO semigroupRing,
	TO semigroupIdeal,	
	TO socle,
	TO type,
	TO genus,
	TO kunzRing,
	TO kunzPoset,
	TO syzFormat,
	TO reduceByList,
        },
     SUBSECTION "Arf Semigroups",
     UL{
 	TO isArf,
	TO arfIndex,
	TO arfClosure,
        TO fractionalIdeal,
	TO infinitelyNearSemigroups,
	TO infinitelyNearModules,
	TO isMinimalMultiplicity,
      }
}


doc ///
Key
 buchweitzCriterion
 (buchweitzCriterion, List)
 (buchweitzCriterion,ZZ,List)
Headline
 Does L satisfies the Buchweitz criterion?
Usage
 d = buchweitzCriterion L
 d = buchweitzCriterion(m,L)
Inputs
 L:List
  generators of a semigroup or the aperyset of a semigroup
 m:ZZ
  the multiplicity of the semigroup
Outputs
 d:ZZ
Description
  Text
   A Weierstrass semigroups L satisfies

       3*(genus L-1) -#sums(G,G) >=0.

   The function returns this difference.  

  Example
   L={7,12,13}
   buchweitzCriterion L
   L= buchweitz 0
   buchweitzCriterion L
   (H,M)=allSemigroups L
   b=(entries M)_0
   g=(entries H)_0
   apply(3,i->buchweitzCriterion(13,b+i*g))

SeeAlso
  buchweitz
///

doc ///
Key
 apery
 (apery, List)
Headline
 Compute the apery set, multiplicity and conductor 
Usage
 A = apery L
Inputs
 L: List
  of positive integers
Outputs
 A:HashTable
Description
  Text
   The smallest nonzero element of s is the \emph{multiplicity}. The Apery set (really sequence) of a semigroup S is the
   the list {a_1..a_m-1} where a_i is the smallest element in s such that a_i = i mod m.
   The \emph{conductor} is 1 plus the largest element \emph{not} in S. We generally specify a semigroup by giving
   a list of positive integers L with gcd = 1, representing the semigroup of all sums of
   elements of L.
  Example
   L = {3,5}
   apery L
SeeAlso
  aperySet
///

doc ///
Key
 aperySet
 (aperySet, List)
 (aperySet, HashTable)
Headline
 Compute the apery set of a numerical semigroup
Usage
 as = aperySet L
 aS = aperySet HL 
Inputs
 L: List
  of positive integers
 H: HashTable
  as computed by H = apery L
Outputs
 aS: List
Description
  Text
   L is taken as generators of a numerical semigroup S; should have gcd = 1.
   The apery set is then the list aS = {a_1..a_(m-1)} where m is the smallest
   element of L, and a_i is the smallest element of S with a_i = i mod m.
  Example
   aperySet {3,5}
   semigroup {3,5}
SeeAlso
 apery
///

doc ///
Key
 semigroup
 (semigroup, List)
Headline
 Compute the semigroup generated by a list of positive integers
Usage
 L = semigroup L0
Inputs
 L0: List
Outputs
 L: List
Description
  Text
   The semigroup is actually computed by the function apery, and
   semigroup L = (apery L)#"semigroup"
  Example
   semigroup {5,4}
SeeAlso
   apery
///

doc ///
Key
 isSymmetric
 (isSymmetric, List)
Headline
 test whether the semigroup generated by L is symmetric
Usage
 t = isSymmetric L
Inputs
 L: List
Outputs
 t: Boolean
Description
  Text
   Suppose that c = conductor S, so that c-1 is the last gap. If x in S, and x<c then
   c-1-x must be a gap, so the number g of gaps <c is at least the number of
   semigroup elements e < c. The semigroup is called \emph{symmetric} if g = e,
   or equivalently if the semigroup ring is Gorenstein. Thus for example any
   semigroup generated by just 2 elements is symmetric.
  Example
   isSymmetric{3,4,5}
   isSymmetric{3, 5}
   gaps {3,5}
   mu {3,5}
SeeAlso
   apery
   semigroup
   gaps
   mu
///


doc ///
Key
 mu
 (mu, List)
 (mu, HashTable)
Headline
 Compute the point representing a semigroup in the Kunz cone 
Usage
 K = mu L
 K = mu H
Inputs
 L: List
 H: HashTable
  produced by apery L
Outputs
 K: List
 
Description
  Text
   The apery set A of a semigroup s with multiplicity m has the form
   A_i = i+ mu_i*m. The point with coordinates mu(L) represents semigroup L
   in the Kunz cone P_m.
  Example
   m = 3
   L = {3,7}
   a = aperySet L
   b = mu L
   apply (#b, i -> (i+1)+(b_i*m))
SeeAlso
 aperySet
///


doc ///
Key
 (genus, List)
Headline
 Compute the number of gaps (genus) of a semigroup
Usage
 g = genus L
Inputs
 L: List
Outputs
 g: ZZ
Description
  Text
   The gaps S is the list of the finitely many positive integers not in S
  Example
   genus {4,7}
   G = gaps {4,7}
   #G
   S = semigroup{4,7}
   set G + set S  == set(0..21)
SeeAlso
 gaps
 semigroup
///

doc ///
Key
 kunzMatrix
 (kunzMatrix, List)
 (kunzMatrix, HashTable) 
Headline
 determine the set of facet equations satisfied by a semigroup
Usage
 M = kunzMatrix L
 M = kunzMatrix H 
Inputs
 L: List
 H: HashTable
   such as produced by apery
Outputs
 M: Matrix
  of 0s and 1s
Description
  Text
   The equations defining the facets of the homogeneous Kunz cone P_m^*
   are
   E_(i,j): a_i+a_j = a_(i+j mod m) for those (i,j) such that  i+j != 0 mod m.
   
   Given a list L generating the semigroups s
   with Apery set a = {a_1..a_i}, M = kunzMatrix L has a 1 in the (i,j) position
   if and only if a satisfies equation E_(i,j). Thus M = kunzMatrix L
   is a symmetric matrix of 0s and 1s that determines the face
   of the kunz cone P on which it lies.
  Example
   L = {4,7}
   aperySet L
   kunzMatrix L
SeeAlso
 aperySet
///
doc ///
Key
 findSemigroups
 (findSemigroups, ZZ, ZZ, ZZ)
 (findSemigroups, ZZ, ZZ)
 (findSemigroups, ZZ) 
Headline
 Find all semigroups with a given number of gaps, multiplicity and/or conductor 

Usage
 LL = findSemigroups(mult, cond, numgaps)
 LL = findSemigroups(mult, numgaps)
 LL = findSemigroups(numgaps)  
Inputs
 mult: ZZ
  multiplicity
 cond: ZZ
  conductor
 numgaps: ZZ
  number of gaps
Outputs
 LL: List
  of sequences of generators of semigroups with the given invariants
Description
  Text
   If S is the Weierstrass semigroup of a point p on a Riemann surface X -- that
   is, the semigroup of pole orders of rational functions at p,
   then the genus of X is the number of gaps of S and there is a 
   differential on X vanishing to order exactly d iff d+1 is a gap.
  Example
   findSemigroups(5,14,8)
   G = gaps {5,7,9}
   #G
  Text
   The number of vanishing orders of quadratic differentials
   on is h^0(2K) = (4g-4) - g + 1 = 3g-3,
   so if s is the semigroup of pole orders of a point on X
   and G is the set of gaps, then there can be at most 3g-3 distinct
   sums of pairs of elements of G. This gives a nontrivial obstruction
   to the smoothability of the semigroup ring of S and thus to the
   existence of a Weierstrass point with semigroup s.

   The following example, discovered by Ragnar Buchweitz (Thesis)
   was the first known example of a non-Weierstrass semigroup.
  Example
   G=toList(1..12)|{19,21,24,25}
  Text
   The function @TO isGapSequence@ returns generators for
   the semigroups with given gap sequence or returns false if there is
   no such semigroup
  Example
   L=isGapSequence G
   g = #G
   3*g-3
   #sums(G,G)
SeeAlso
 isGapSequence
 gaps
///

///
restart
debug loadPackage("NumericalSemigroups",Reload=>true)
G=toList(1..12)|{19,21,24,25}
L=isGapSequence G
I=semigroupIdeal L
S=ring I
inI=trim ideal(gens I%S_0)
fI=res I
degrees fI_2
elapsedTime (A,unfolding)=makeUnfolding I; -- 14.1952 seconds elapsed
numgens A
tally degrees A
///




doc ///
Key
 (mingens, List)
Headline
 Find a mininmal set of semigroup generators
Usage
 L' = mingens L
Inputs
 L: List
  generators of a semigroup
Outputs
 L': List
  minimal generators of the same semigroup
Description
  Text
   The set of generators is minimal if it has empty
   intersection with the set of sums of non-zero generators.

   It would have been nicer to overload @TO mingens@ to
   accept a list.
  Example
   L = semigroup {3,7}
   mingens L
SeeAlso
 semigroup
///

doc ///
Key
 socle
 (socle, List)
Headline
 elements of the semigroup that are in the socle mod the multiplicity
Usage
   socle L
Inputs
 L:List
  generators of a semigroup
Outputs
 L:List
  subset of the Apery set representing the socle mod the multiplicity
Description
  Text
   Let S = semigroup L
   be a numerical semigroup with minimal non-zero element m,
   and consider the artinian ring
   A(s) = k[{t^i : i in s]/(t^m).
   The socle of A(s) is the sum of the minimal nonzero ideals,
   and socle L is a set of generators of the socle
  Example
   L = {3,7}
   s = semigroup L
   socle L
   L = {3,4,5}
   s = semigroup L
   socle L
SeeAlso
 semigroup
///

 doc ///
Key
 type
 (type, List)
Headline
 type of the local semigroup ring
Usage
 r = type L
Inputs
 L:List
  of semigroup generators
Outputs
 r: ZZ
  the type
Description
  Text
   The type of a local Cohen-Macaulay ring is the number
   of generators of the canonical module, or equivalently the
   dimension of the socle of an zero-dimensional reduction.

   For example, the type of a complete intersection such as
   the semigroup ring of a semigroup generated by 2 elements.
  Example
   type {3,5}
SeeAlso
 socle
///
doc ///
Key
 gaps
 (gaps, List)
Headline
 The gap sequence of a semigroup
Usage
 G = gaps L
Inputs
 L: List
  of semigroup generators
Outputs
 G: List
  the gap sequence
Description
  Text
   If semigroup L is the Weierstrass semigroup of a Riemann surface
   C at a point, then #gaps L = g = h^0(omega_C), the genus of C. Furthermore,
   the number of elements of sums(n, G) is bounded by the dimension of
   h^0(omega_C^n) = n*(2g-2)-g+1 = (2n-1)g-2n+1. However, for
   an arbitrary semigroup the number #sums(n,G) may be larger;
   the first such example was found by Ragnar Buchweitz, and
   is given below.

   The function isGapSequence returns either false or generators
   of the semigroup of which the sequence is the gap sequence.

  Example
   G=toList(1..12)|{19,21,24,25}
   g = #G
   for n from 1 to 3 list (#sums(n,G),n*(2*g-2) - g + 1)
   L=isGapSequence G
   G ==gaps L
Caveat
SeeAlso
 isGapSequence
 findSemigroups
///
doc ///
Key
 sums
 (sums, List, List)
 (sums, ZZ, List)
Headline
 sum of two sequences
Usage
 L = sums(L1, L2)
 L = sums(n,L1)
Inputs
 L1: List
 L2: List
 n:ZZ
Outputs
 L:List
Description
  Text
   sums(L1,L2) returns a sorted list of the unique
   sums of nonzero elements of L1 with L2;
   sums(n, L1) returns the sorted list of unique sums
   of n nonzero elements of L1.
  Example
   L1 = {2,3}
   L2 = {4,5}
   sums(L1, L2)
   sums(1, L1) == L1
   sums(L1, L1)
   sums(2,L1)
  Text
   Of course the sequence of arbitrary sums of elements including 0
   is essentially semigroup L. To get just the sums of n elements,
   one could write:
  Example
   n = 3
   {0}|sort unique flatten (for i from 1 to n list sums(i,L1))
///

doc ///
Key
 def1
 (def1, List)
Headline
 degrees of a basis of T^1
Usage
 D = def1 L
Inputs
 L: List
  generators of a semigroup
Outputs
 D: List
  degrees of a basis of T^1(semigroupRing L)
Description
  Text
   T^1(B) is the tangent space to the versal deformaion of
   the ring B, and is finite dimensional when B has isolated
   singularity. If B = S/I is a Cohen presentation, then
   T^1(B) = coker Hom(Omega_S, B) -> Hom(I/I^2, B).
   When B is a semigroup ring, then Henry Pinkham proved that
   an open subset of the space of elements of T1 of
   negative degree correspond to smoothings of the projective cone
   of the semigroup ring to Riemann surfaces
  Example
   def1{2,3}
///

doc ///
Key
 (conductor, List)
Headline
 conductor of a semigroup
Usage
 c = conductor L
Inputs
 L:List
  of generators of a semigroup
Outputs
 c:ZZ
  conductor of the semigroups
Description
  Text
   Semigroups in this package are additively closed
   cofinite subsets of ZZ_{>= 0}. The conductor
   is the smallest element c such that c+i is
   in the semigroup for all i >= 0. For a semigroup
   generated by two elements a,b, the conductor
   is (a-1)(b-1), but for semigroups with more
   generators there is no known formula.
  Example
   conductor {3,5}
   conductor {5, 7, 9, 13}
///

doc ///
Key
 facetRays
 (facetRays, List)
Headline
 computes the rays spanning the face in which a semigroup lies
Usage
 R = facetRays L
Inputs
 L:List
  of generators of a semigroup
Outputs
 R:Matrix
  of integers; the columns are the rays
  of the face on which semigroup L lies
Description
  Text
   Note that this function computes the rays of the
   closed face *not facet* on which L lies.
   Uses the Fourier-Motzkin algorithm to go from
   the @TO aperyConeEquations@ satisfied by the semigroup
   to the rays. For example, in multiplicity 3,
   the cone has two rays, occupied by the
   semigroups semigroup{3,4} and semigroup{3,5},
   with semigroup{3,4,5} in the interior.
   The rays are given in reduced form (a vector
   of positive integers with gcd 1), and appear
   as the columns of the output matrix.
  Example
   aperySet{3,4}
   facetRays{3,4}
   facetRays{3,5}
   facetRays{3,4,5}
  Text
   On the facet with the @TO buchweitz@ example there are two facet rays:
  Example
   F = facetRays buchweitz 0
  Text
   The second column is mu buchweitz 0, the mu vector of the Buchweitz example.
   Adding multiples of it to the Weierstrass semigroups ordinary point
   of genus 12, we eventually reach a semigroup that fails the Buchweitz
   test to be a Weierstrass semigroup:
  Example
   --this example is nonsense ; replace.
   b =flatten( {0}| entries(F)_0 ) -- edited for minimal change STILL TO BE FIXED
   L = toList (13..25)
   #L,#b
  Text
    We conjecture that the same phenomen  for any semigroup L0
    of multiplicity 13 in place of L. Here is a "random" example:
  -- Example
  --   setRandomSeed 0
  --   L0 = {13}|aperySet ({13}|apply(1 + random 10, i->13+random 10))
  --   i = 1
  --   for i from 0 to 20 list (
  --      L' = L0+i*13*b;
  --      G = gaps L';
  --      #sums(G, G) - 3*(genus L' -1)
  --      )
SeeAlso
 aperySet
 aperyConeEquations
 coneRays
///


doc ///
Key
 allSemigroups
 (allSemigroups, List)
 (allSemigroups, ZZ)
Headline
 Compute the Hilbert basis and module generators of a cone of semigroups
Usage
 (H,M) = allSemigroups L
 (H,M) = allSemigroups m
Inputs
 L:List
  of generators of a semigroup
 m: ZZ
  the multiplicity of the semigroups
Outputs
 H:Matrix
  of integers; the rows form the Hilbert basis  of the cone;
 M:Matrix
  of module generators of the cone 
Description
  Text
   Using Normaliz we compute the face of the Kunz cone containing L
   (or of the cone of semigroups of multiplicity m).
   
   Every semigroup of multiplicity m has Apery set of the form
   m, mu_1*m+1, mu_2*m+2..mu_(m-1)*m+(m-1).
   The sequence of mu_i for a semigroup L
   is the value of mu(L).
  Example
   (H,M) = allSemigroups 3
   H == matrix{
             {3,3},
	     {3,6},
	     {6,3}
	     }
   M == matrix{
             {4,5},
	     {4,8},
	     {7,5},
	     {10,5}
	     }
  Text
   Here the rows of the
   first matrix, H, form a Hilbert basis, that is, a set
   of semigroup generators, of
   the semigroup of the possible mu(L) as L runs over the
   semigroups of multiplicity 3. The rows of the the
   second matrix, M, when prepened by the multiplicity 3,
   are Apery sets of some semigroups such that every semigroup
   of multiplicity 3 has Apery set equal to a linear combination
   of the rows of H added to one of the rows of M.
  Example
   allSemigroups {4,7,9}
  Text
   This means that all the semigroups on the same (open) face of the
   Kunz cone as {4,7,9} can be obtained by taking a
   linear combination of the rows of H, adding one of
   the rows of M, and prepending the multiplicity 4.
   
   On the face with the @TO buchweitz@ example
   there are two facet rays:
  Example
   (H,M) = allSemigroups buchweitz 0
  Text
   The first row of H is 13*(mu buchweitz 0), the mu vector of the Buchweitz example.
   Adding multiples of the first row to the Weierstrass semigroups of an ordinary point
   on a curve of genus 12, we eventually reach a semigroup that fails the Buchweitz
   test to be a Weierstrass semigroup:
  Example
   b = {0}|flatten (entries H)_0
   L = toList (13..25)
   for i from 0 to 15 list (
       L' = L+i*b;
       G = gaps L';
       3*(genus L' -1)-#sums(G,G)
       )
  Text
    By Riemann-Roch the quantity 3*(genus L' -1)-#sums(G,G) is non-negative
    for Weierstrass semigroups. 
    We conjecture that the same thing is true for any semigroup L0
    of multiplicity 13 in place of L. Here is a "random" example:
  Example
    setRandomSeed 0
    L0 = {13}|aperySet ({13}|apply(1 + random 10, i->13+random 10))
    for i from 0 to 20 list (
       L' = L0+i*b;
       G = gaps L';
       3*(genus L' -1)-#sums(G,G)
       )
SeeAlso
 aperySet
 aperyConeEquations
 buchweitzCriterion
///

doc ///
Node
  Key
   randomSemigroup
   (randomSemigroup, List, ZZ)
   (randomSemigroup, ZZ, ZZ)
  Headline
   Random semingroup on a given face of the Kunz cone
  Usage
   L' = randomSemigroup(L,b)
   L' = randomSemigroup(m,b)   
  Inputs
   L:List
    of generators of a semigroup
   b:ZZ
    bound for the random numbers to be used
   m:ZZ
    multiplicity
  Outputs
   L':List
    Apery set of a semigroup
  Description
    Text
     Find a random semigroup within a bounded portiion
     of the open face of the Kunz cone containing L
     (or of the interior of the Kunz cone, if a multiplicity m
     is specified instead of a list.

     After (H,M) = allSemigroups L, the Apery set of L'
     is obtained by prepending m to
     the sum of a random linear combination of the rows of H
     (using random integers 0..b-1)
     and a random row of M.

     Since L' is on the same face as L, it shares many
     homological properties, such as the total betti
     numbers of the resolution of its semigroup ideal.
    Example
     L = {6,9,13,16}
     L' = randomSemigroup({6,9,13,16}, 5)
     mingens L'
     F = res semigroupIdeal L;
     F'= res semigroupIdeal L';
     apply(1+length F, i-> rank F_i)
     apply(1+length F', i-> rank F'_i)
  Caveat
   The list L' is an Apery set, so
   may not be a minimal set of semigroup generators.
  SeeAlso
   allSemigroups
   mingens
///

doc ///
Key
 semigroupRing
 (semigroupRing, List)
 [semigroupRing, "BaseField"]
Headline
 forms the semigroup ring over "BaseField"
Usage
 A = semigroupRing L
Inputs
 L:List
  of semigroup generators
Outputs
 A:Ring
  algebra over "BaseField"
Description
  Text
   If the basering is kk, the semigroup ring
   is A = kk[x^S] where x^S denotes the set of
   monomials in variables x_i with exponent
   vectors in S, and kk is the field that is the value
   of the option "BaseField" (ZZ/101 by default).

   If m is the  multiplicity of S,
   the semigroup ring depends up to an
   isomorphism that may change the degrees,
   only on the face of the Kunz cone
   in which the semigroup lies.

   Semigroup rings are interesting as
   examples, and arise as Weierstrass semigroups
   of points on algebraic curves: if p in C
   is such a point, then the Weierstrass semigroup
   of C at p is the set of pole orders of rational
   functions with poles only at p, and the
   semigroupRing is the associated graded ring of the filtered ring
   $\bigcup_{n >= 0} H^0(O_C(np))$.
   For non-Weierstrass
   points the semigroup is 0,g+1,g+2.., and there are
   finitely many "Weierstrass" point p
   whose semigroup has weight >=2.

   For example if C is a smooth plane quartic,
   then at each ordinary flex point, the semigroup is
   0,3,5,6,7,..
   
  Example
   semigroupRing {3,4,5}
   weight {4,5,6,7}, gaps {4,5,6,7}
   semigroupRing {4,5,6,7}
   weight {3,5,7}, gaps {3,5,7}
   semigroupRing {3,5,7}
   weight {3,4}, gaps {3,4}
   semigroupRing({3,4}, "BaseField" => QQ)
SeeAlso
 semigroupIdeal
 weight
 ///


doc ///
Key
 buchweitz
 (buchweitz, ZZ)
Headline
 An example of a semigroup that is not a Weierstrass semigroup
Usage
 L = buchweitz i
Inputs
 i:ZZ
Outputs
 L:List
  generators of a semigroup
Description
  Text
   For i>=0 this produces a semigroup B with genus 16+i, conductor 26+2i, and
   #sums(2, gaps buchweitz i) = 3*(genus B -1)+1). This implies
   that these semigroups are NOT Weierstrass semigroups by the
   following argument, first employed by Buchweitz:

   If L generates the Weierstrass semigroup of a point x
   on a Riemann surface C, then the gaps L is the
   set {1+v | v is the order at p of vanishing of a global
   section of \omega_C}. Thus
   sums(d, #gaps L) <= dim H^0(\omega_C^{d) = d*(2g-1) - g + 1.
  Example
   B = buchweitz 0
   g = #gaps B
   m = 3
   2*(2*g-2) - g + 1
   #sums(2, gaps B)
  Text
   More such semigroups can be found with @TO buchweitzSemigroups@
Acknowledgement
 The result was written in Ragnar Buchweitz' These d'Etat,
 but never otherwise published by Buchweitz. In the meantime it became
 famous anyway.
///

doc ///
Key
 buchweitzSemigroups
 (buchweitzSemigroups, ZZ)
 (buchweitzSemigroups, ZZ, ZZ)
 (buchweitzSemigroups, ZZ, ZZ, ZZ)  
Headline
 Finds semigroups that are not Weierstrass semigroups by the Buchweitz test
Usage
 LL = buchweitzSemigroups g
 LL = buchweitzSemigroups (m,g)
 LL = buchweitzSemigroups (m,c,g)
Inputs
 g:ZZ
  genus
 m:ZZ
  multiplicity
 c:ZZ
  conductor
Outputs
 LL:List
  list of semigroups
Description
  Text
   Uses findSemigroups to produce lists of semigroups with the given
   invariants, and then uses the Buchweitz test: the following
   inequality holds for Weierstrass semigroups:
   sums(2, #gaps L) <= dim H^0(\omega_C^{2) = 2*(2g-1) - g + 1.
  Example
   B = buchweitzSemigroups (13,26,16)
   buchweitz 0
   B = buchweitzSemigroups (14,28,17)
  Text
   The second example in these two cases are part of the sequence defined in @TO buchweitz@. As g increases
   there are many more.
  Example
   #buchweitzSemigroups (15,30,18)
SeeAlso
 buchweitz
 findSemigroups
///


doc ///
Key
 isGapSequence
 (isGapSequence, List)
Headline
 test whether a list of integers can be the list of gaps of a semigroup
Usage
 L = isGapSequence G
Inputs
 G: List
Outputs
 L:Boolean
 L:List
Description
  Text
   The function isGapSequence returns either false or
   a list of generators
   of the semigroup of which the sequence is the gap sequence.
   Note that the gap sequence of a semigroup of multiplicity m
   begins with 1..m-1, and ends with the Frobenius number c-1,
   where c is the conductor.
  Example
   G = {1,2,3,4,6,9,11,14}
   isGapSequence G
   G = {1,2,3,4,6,9,11}
   S = isGapSequence G
   G == gaps S
SeeAlso
 gaps
///

doc ///
Key
 ewt
 effectiveWeight
 (ewt, List)
 (effectiveWeight, List)
Headline
 Effective weight of a semigroup (Pflueger)
Usage
 w = ewt L
Inputs
 L: List
  generators of a semigroup
Outputs
 w: ZZ
  the effective weight
Description
  Text
    The effective weight of a semigroup S is defined as the number
   of pairs (a,b) such that a is a minimal generator of S and b is a gap of S with a<b.

   By contrast, the @TO weight@ of S (the sum of the ramification indices of the corresponding Weierstrass
   point) may be defined as the number of pairs (a,b) such that a is in S and b is a gap with a<b.

   Improving on work of Eisenbud-Harris (who proved that primitive semigroups S are Weierstrass),
   and occur in codimension equal to the @TO weight@ of S),
   Nathan Pflueger introduced the "effective weight" and showed that all semigroups with
   genus g and effective weight w<g are Weierstrass, and occur on a subvariety of M_(g,1) with
   codimension w.

   For example, semigroups generated by two elements are always Weierstrass since
   complete intersections are smoothable; they are almost never primitive, 
  Example
   L = {6,7}
   genus L
   weight L
   ewt L
References
 Pflueger, Nathan . On nonprimitive Weierstrass points.
 Algebra Number Theory  12  (2018),  no. 8, 1923--1947.
SeeAlso
 weight
 genus
///

doc ///
Key
 weight
 (weight, List)
Headline
 weight of a semigroup
Usage
 w = weight L
Inputs
 L:List
Outputs
 w: ZZ
Description
  Text
   If S is the Weierstrass semigroup S of a point p on a Riemann surface C,
   then the vanishing sequence (v_0,\dots, v_(g-1)) of the canonical series at p
   is the list of orders of vanishing of differential forms at p, and the
   ramification sequence at p is (v_0 - 0, v_1 - 1, .. ,v_(g-1) - (g-1)).
   The weight of the Weierstrass point p is the sum of the ramification sequence at p.

   The vanishing sequence can be computed from the set G of gaps in S as
   v_i = G_i - 1, so the weight is sum(G_i - 1 - i) or as the number of pairs
   (a,b) such that a is in S, b is a gap, and a < b.

  Example
   weight {5,7}
   semigroup{5,7}
   gaps{5,7}
  Text
   The effective weight ewt is the number of such pairs where a is a minimal generator
   of S; this may be a better measure.
  Example
   mingens{5,7}
   ewt {5,7}
SeeAlso
 gaps
 ewt
 mingens
///

doc ///
Key
 semigroupIdeal
 (semigroupIdeal, List)
 [semigroupIdeal, "VariableName"=> "x"]
 [semigroupIdeal, "MinimalGenerators"=>true]
 [semigroupIdeal, "BaseField" => ZZ/101]
Headline
 The ideal defining the semigroup ring
Usage
 I =  semigroupIdeal L
Inputs
 L: List
Outputs
 I: Ideal
Description
  Text
   The semigroup ideal of the semigroup generated by L
   is the kernel of the map kk[x_0..x_(#L)] -> kk[t]
   sending x_i to t^(L_i), where kk is the specified BaseField,
   defaulting to ZZ/101 and x is the specified VariableName.
   If the option "MinimalGenerators" is set to true, the default, then
   the program first computes a minimal set of generators from L;
   if it is set to false, the program uses L itself.
  Example
   semigroupIdeal {5,7}
   semigroupIdeal({5,7,10}, "MinimalGenerators" => false)
SeeAlso
 mingens
 semigroupRing
///
doc ///
Key
 aperySemigroupRing
 (aperySemigroupRing, List)
Headline
 computes the semigroup ring using both the multiplicity and the full Apery set
Usage
 R = aperySemigroupRing L
Inputs
 L: List
Outputs
 R: Ring
Description
  Text
   While the function semigroupRing L uses just a minimal set of generators,
   the function aperySemigroupRing L uses the larger Apery set, and puts
   the generator corresponding to the multiplicity at the end.
  Example
   L = {5,6}
   aperySet L
   gens aperySemigroupRing L
   gens semigroupRing L
SeeAlso
 semigroupRing
 aperySet
 mingens
///

///
restart
loadPackage ("NumericalSemigroups", Reload => true)
uninstallPackage "NumericalSemigroups"
restart
installPackage "NumericalSemigroups"
check "NumericalSemigroups"
--<<docTemplate
///

doc ///
Key
 burchIndex
 (burchIndex, List)
Headline
 Compute the burchIndex of the Burch ring of a semigroup
Usage
 b = burchIndex L
Inputs
 L:List
  of generators of a semigroup
Outputs
 b:ZZ
Description
  Text
   The Burch index of an artinian local ring A = S/I,
   where (S,n,k) is regular local, is
   dim_k (n/(nI:(I:n))). The number returned by

   burchIndex L

   where L is a list of integers, is the burch index of the
   Kunz ring of L, that is, the
   semigroup ring of L modulo the lowest degree element.
  Example
   burchIndex {3,4,5}
References
 Dao, HL and Eisenbud, D,
 Burch index, summands of syzygies and linearity in resolutions.
 Bull. Iranian Math. Soc. 49 (2023).
SeeAlso
 kunzRing
///

doc ///
Key
 kunzRing
 (kunzRing, List)
 [kunzRing, "BaseField"]
 [kunzRing, "VariableName"]
 [kunzRing, "MinimalGenerators"]
Headline
 artinian reduction of a semigroup ring
Usage
 R = kunzRing L
Inputs
 L:List
  of ZZ
Outputs
 R:Ring
  semigroup ring mod multiplicity element
Description
  Text
   Returns the semigroup ring modulo the element of least degree.
   The kunzRing shares many properties with the semigroup ring;
   see @TO semigroupRing@ for explanations of the options.
  Example
   semigroupRing {3,5}
   kunzRing {3,5}
   kunzRing ({3,5,6}, "BaseField" => ZZ/32003, "VariableName" => y, "MinimalGenerators" => false)
  Text
   The Kunz ring is an invariant of the face of the Kunz cone which contains L.
   For all L in the interior of the corresponding face have isomorphic Kunz rings.
  Example
   L=semigroup {4,6,7}
   (H,M)=allSemigroups {4,6,7}
   L1={4}|flatten (entries(M^{2}+3*H^{1}))
   #gaps L1, socle L1
   kunzRing {4,6,7}
   kunzRing L1
///
doc ///
Key
 fractionalIdeal
 (fractionalIdeal, List, List)
 [fractionalIdeal, "Ideal"]
 
Headline
 turn a fractional ideal into a proper ideal
Usage
 I = fractionalIdeal(sgrp,mons)
 L = fractionalIdeal(sgrp,mons)
 
Inputs
 sgrp:List
  of ZZ, representing a semigroup
 mons:List
  of ZZ representing the degrees of generators of a fractional ideal
Outputs
 I:Ideal
  ideal in semigroupRing sgrp
Description
  Text
   sgrp is a list of ZZ_{>0}, interpreted as generators of a semigroup;
   mons is a list of ZZ interpreted as generating a module in the quotient field of sgrp
   with the option "Ideal" => false we get a list of integers belonging to the semigroup,
   otherwise a proper ideal of the semigroup ring.
   of the ring semigroupRing sgrp. In both cases, the program chooses the
   generators of least possible degree.

   This is perhaps most useful when regarding a blowup or iterated blowup as a module
   over the original ring. For example, the sequence of blowups of the semigroupRing {5,9}
   is given by semigroupRing{4,5}, semigroupRing{1}:
  Example
   sgrp = {5,9}
   sgrp1 = {4,5}
   sgrp2 = {1}
   fractionalIdeal(sgrp, sgrp1, "Ideal"=>false)
   fractionalIdeal(sgrp, sgrp2)
SeeAlso
 semigroupRing
///

doc ///
Key
 infinitelyNearSemigroups
 (infinitelyNearSemigroups, List)
Headline
 The sequence of blowup semigroups of a numerical semigroup
Usage
 N = infinitelyNearSemigroups L
Inputs
 L: List
  of positive integers, generating a numerical semigroup S
Outputs
 N: List
  of lists of minimal generators of the successive blowups of S,
  starting with S itself and ending with the trivial semigroup {1}
Description
  Text
   Let R be the semigroup ring of a numerical semigroup S with
   multiplicity m, and let \mathfrak{m} = (t^s : s \in S, s > 0) be
   its maximal ideal. The \emph{blowup} of R at \mathfrak{m} is the
   subring R[\mathfrak{m}/t^m] = R[t^{s-m} : s \in S, s > 0] of the
   fraction field frac R; this blowup is again a semigroup ring,
   namely the semigroup ring of the \emph{blowup semigroup} S',
   generated by m together with s-m for every nonzero s in S.

   Iterating this construction produces an ascending sequence of
   semigroup rings R = R_0 \subseteq R_1 \subseteq R_2 \subseteq \dots,
   each obtained from the previous by blowing up at its maximal ideal,
   and a corresponding ascending sequence of semigroups
   S = S_0 \subseteq S_1 \subseteq S_2 \subseteq \dots.
   The sequence stabilizes after finitely many steps when S_r becomes
   the trivial semigroup N (generated by 1), at which point R_r is the
   integral closure of R in frac R.

   The function infinitelyNearSemigroups returns this sequence as a
   list of minimal generating sets, beginning with mingens S and
   ending with {1}.
  Example
   infinitelyNearSemigroups {3,5,7}
   infinitelyNearSemigroups {5,8}
  Text
   For an Arf semigroup, every blowup in the sequence has minimal
   multiplicity (see @TO isArf@), and the sequence of multiplicities
   determines the Arf closure (see @TO arfClosure@):
  Example
   N = infinitelyNearSemigroups {5,8,9,11,12}
   apply(N, min)
SeeAlso
 infinitelyNearModules
 isArf
 arfClosure
 semigroup
///

doc ///
Key
 infinitelyNearModules
 (infinitelyNearModules, Ring)
Headline
 The sequence of blowups of a semigroup ring as fractional ideals
Usage
 MM = infinitelyNearModules R
Inputs
 R: Ring
  a semigroup ring, as produced by @TO semigroupRing@
Outputs
 MM: List
  of R-modules, one for each semigroup in
  @TO infinitelyNearSemigroups@ applied to the underlying semigroup of R
Description
  Text
   Let R be the semigroup ring of a numerical semigroup S, and let
   S = S_0 \subseteq S_1 \subseteq \dots \subseteq S_r = N
   be the sequence of blowups of S returned by
   @TO infinitelyNearSemigroups@. Each S_i is a finitely generated
   R-submodule of the fraction field frac R: namely, the R-module
   generated (in frac R) by the monomials t^s for s in a generating
   set of S_i.
   
   The function infinitelyNearModules returns the sequence of these
   R-modules. The first entry is (an R-module isomorphic to) R itself.
  Example
   R = semigroupRing {3,5,7}
   MM = infinitelyNearModules R
   MM/numgens
Caveat
   The construction goes through @TO fractionalIdeal@, which clears
   denominators by multiplying through by a high enough power of
   the multiplicity element so as to produce an honest ideal of R.
   As a result, the modules returned by infinitelyNearModules are
   isomorphic to the actual infinitely near modules sitting inside
   frac R, but their internal grading is shifted by the chosen
   denominator. In other words, what these modules represent is
   correct \emph{up to a degree shift}; if you need the natural
   grading inherited from frac R, you must compensate for that shift
   by hand.
SeeAlso
 infinitelyNearSemigroups
 fractionalIdeal
 semigroupRing
 isArf
///

doc ///
Key
 aperyConeEquations
 (aperyConeEquations, ZZ)
 (aperyConeEquations, List)
 muConeEquations
 (muConeEquations, ZZ)
 (muConeEquations, List)
Headline
 Inequalities defining the Kunz cones
Usage
 M =  aperyConeEquations m
 M = aperyConeEquations sgrp
 M =  muConeEquations m
 M = muConeEquations sgrp
Inputs
 m:ZZ
  multiplicity
 sgrp:List
  generators of a semigroup
Outputs
 M:Matrix
  m-1 x d matrix whose columns represent
    inequalities defining the Kunz cone
Description
  Text
   Let S be the numerical semigroup defined by a list sgrp,
   have multiplicity m and apery set a_1,\dots,a_(m-1) and
   set mu_i = (a_i -i)//m. The homogeneous Kunz cone of
   semigroups of multiplity m is the convex polyhedral cone
   defined by the inequalities of the form

   a_i + a_j - a_(i+j) \geq 0.

   where 1\leq i,j\leq m-1 and i+j\neq m is interpreted mod m.   

   On the other hand, the inHomogeneous Kunz cone is
   given by the equations

   mu_i + m_j - mu_(i+j) \geq 0 if i+j<m
   mu_i + m_j - mu_(i+j) -1  \geq 0 if i+j> m

   The output of the function is the pair (E,c),
   where, if L is a semigroup, then

   matrix {Apery L} * E

   is a non-negative matrix with entries divisible by m, and

   matrix {mu L} *E - c

   is a non-negative matrix.
   The columns of E and c are indexed by the lexicographically
   ordered pairs {i,j} with 1<= i<=j<=m-1 and i+j != m.
   
   

   The function aperyConeEquations m returns an m-1 x d matrix of ZZ
   whose columns are the coefficients of the left hand sides of these inequalities.
   The function aperyConeEquations sgrp does the same, with additional columns representing
   the additional inequalities of this type that are satisfied by
   the Apery set apery(sgrp). For m = 3, the semigroup {3,4,5} is interior (and thus
   satisfies no further equations), while the semigroups {3,4} and {3,5} are on
   the two extremal rays of the cone.
  Example
   aperyConeEquations 3
   aperyConeEquations {3,4,5}
   aperyConeEquations {3,4}
   aperyConeEquations {3,5}
   allSemigroups 3
  Text
   The inhomogeneous Kunz cone does the same, but for the numbers mu_i instead of
   a_i. Thus when i+j > m the inequality mu_i+mu_j-mu_(i+j) \geq 0 is replaced by the inequality

   mu_i+mu_j - mu_(i+j) -1.

   The function muConeEquations m,
   returns the matrix
   aperyConeEquations m
   with one more row, where the last row represents the
   constant terms of this inquality:
  Example
   eq=muConeEquations 3
   muConeEquations {3,4,5}
   muConeEquations {3,4}
   muConeEquations {3,5}
   (H,M)=allSemigroups 3
   (H,M)=allSemigroups 4
   M1=(M|matrix apply(rank target M,i->{1}))
   eqInh=muConeEquations 4
   eqh=aperyConeEquations 4
   M1*eqInh
   H*eqh
  Text
   All entries of M1*eqInh and H*eqh are non-negative as desired.
References
 Kunz, Ernst: Klassification numerische Halbgruppen
Caveat
SeeAlso
 apery
 coneRays
///


doc ///
Key
 coneRays
 (coneRays, ZZ)
Headline
 All the rays of the (homogeneous) Kunz cone
Usage
 M = coneRays m
Inputs
 m:ZZ
  multiplicity
Outputs
 M:Matrix
  of ZZ -- columns are vectors on the rays of the cone
Description
  Text
   Uses the Fourier-Motzkin algorithm to compute the rays from the list of supporting hyperplanes,
   which is given by @TO aperyConeEquations@. The number of rays grows rather quickly with m;
   the actual number is unknown. Use @TO facetRays@ to determine the rays bounding the face
   on which a given semigroup lies.
  Example
   coneRays 3
   coneRays 4
   facetRays {4,5,6}
SeeAlso
 aperyConeEquations
 facetRays
///

doc ///
Node
  Key
   semigroupFromMu
   (semigroupFromMu, List)
  Headline
   Inverse of the function mu
  Usage
   L = semigroupFromMu mm
  Inputs
   mm:List
    of m-1 positive integers
  Outputs
   L:List
    minimal generators of a semigroup
  Description
    Text
     if A = {a_1..a_(m-1)}
     is the Apery set of a semigroup L,
     then mu L is the sequence of numbers (a_i -i)/m.
     semigroupFromMu is the inverse operation.
    Example
     L = {6,9,13,16}
     mm = mu L
     semigroupFromMu mm
  SeeAlso
   mu
///
doc ///
Node
  Key
   semigroupsFromMatrix
   (semigroupsFromMatrix, Matrix)
  Headline
   applies semigroupFromMu to the columns of a matrix
  Usage
   ss = semigroupsFromMatrix M
  Inputs
   M:Matrix
    of non-negative integers
  Outputs
   ss:List
    of minimal generating sets of semigroups
  Description
    Text
     If M = facetRays L then the columns of M
     are the values of mu applied to the first
     point on each ray of the minimal closed face
     containing L.
     In this case semigroupsFrom M returns the list
     of semigroups corresponding to the rays,
    Example
     M1 = coneRays 4
     netList semigroupsFromMatrix M1
     (H,M) = allSemigroups 4
  SeeAlso
   coneRays
   allSemigroups
///

doc ///
Key
 isArf
 (isArf, List)
Headline
 test whether a numerical semigroup is Arf
Usage
 t = isArf L
Inputs
 L: List
  of positive integers, generating a numerical semigroup S
Outputs
 t: Boolean
  true if S is Arf
Description
  Text
   A numerical semigroup S is \emph{Arf} if for every triple of elements
   x \geq y \geq z in S, the element x+y-z also lies in S. Equivalently,
   S is Arf if and only if every semigroup in the sequence of blowups
   (the infinitely near semigroups of S) has minimal multiplicity, that is,
   has multiplicity equal to its embedding dimension. The latter
   characterization is what isArf actually checks.

   The input L is interpreted as a generating set of a semigroup; it
   need not be the minimal generating set, and isArf works with the
   semigroup it generates.
  Example
   isArf {5,8,9,11,12}
   isArf {5,8,9,12}
   isArf {3,5,7}
  Text
   The trivial semigroup (all of N) is Arf:
  Example
   isArf {1}
SeeAlso
 arfClosure
 arfIndex
 infinitelyNearSemigroups
 semigroup
///

doc ///
Key
 arfClosure
 (arfClosure, List)
Headline
 Compute the Arf closure of a numerical semigroup
Usage
 A = arfClosure L
Inputs
 L: List
  of positive integers, generating a numerical semigroup S
Outputs
 A: List
  of minimal generators of the Arf closure of S
Description
  Text
   A numerical semigroup S is \emph{Arf} if for every triple of elements
   x \geq y \geq z in S, the element x+y-z also lies in S. Equivalently,
   S is Arf if and only if every semigroup in the sequence of blowups
   (the infinitely near semigroups of S) has minimal multiplicity, that is,
   has multiplicity equal to its embedding dimension.

   The \emph{Arf closure} Arf(S) is the smallest Arf semigroup containing S.
   The function arfClosure returns a list of minimal generators of Arf(S).

   The Arf closure is computed from the multiplicities of the infinitely
   near semigroups of S: if those multiplicities are m_0, m_1, ..., m_(r-1),
   then the partial sums m_0, m_0+m_1, ..., m_0+...+m_(r-1) lie in Arf(S),
   and together with the integers immediately above the largest partial sum
   (up to m_0 - 1 of them) they generate Arf(S).
  Example
   arfClosure {3,5,7}
   arfClosure {5,8,9,12}
   arfClosure {5,8,9,11,12}
  Text
   The third example is already Arf, so the Arf closure equals the input
   semigroup (up to choice of minimal generators). The trivial semigroup
   N (generated by 1) is its own Arf closure:
  Example
   arfClosure {1}
SeeAlso
 isArf
 arfIndex
 infinitelyNearSemigroups
 semigroup
 aperySet
///

doc ///
Key
 arfIndex
 (arfIndex, List)
Headline
 Computes the Arf index of a numerical semigroup
Usage
 n = arfIndex L
Inputs
 L: List
  of positive integers, generating a numerical semigroup S
Outputs
 n: ZZ
  the Arf index of S
Description
  Text
   Let A be the operator that sends a numerical semigroup S to the
   semigroup obtained by adjoining all elements of the form x+y-z with
   x \geq y \geq z in S (and x+y-z still bounded by the conductor region
   under consideration). Iterating A eventually stabilizes at the Arf
   closure of S; see @TO arfClosure@.

   The function arfIndex returns the number of times A must be applied
   to reach this fixed point. In particular, arfIndex L is 0 exactly when
   the input semigroup is already Arf.
  Example
   arfIndex {5,8,9,11,12}
   arfIndex {5,8,9,12}
   arfIndex {3,5,7}
SeeAlso
 isArf
 arfClosure
 infinitelyNearSemigroups
 semigroup
///
doc ///
Key
 isKnownExample
 (isKnownExample, List)
Headline
 Is L a known Weierstrass semigroup?
Usage
 b = isKnownExample L
Inputs
 L:List
  generators of the semigroup
Outputs
 b:Boolean
  true if L is a known example of a Weierstrass semigroup
Description
  Text
   Certain semigroups are known to be Weierstrass.
   For example L has 2  or 3 generators only, by work of Pinkham and Sally. 
   Eisenbud-Harris proved that semigroups L of @TO weight@(L)<@TO genus@(L) are smoothable, and
   Pflueger extended this to show that semigroups L of @TO ewt@(L)<@TO genus@(L) are smoothable.
   Komeda proved that anysemigroup with min L < 6 is Weierstrass.
   In "A minimal non-Weierstrass Semigroup" by Eisenbud and Schreyer it is shown that all
   semigroups of genus <13 are Weierstrass but that the semigroup {6,9,13,16}, of genus 13,
   is not.
  Example
   L={7,12,13}
   isKnownExample L
   L={7,8,9,11,13}
   ewt L, genus L
   isKnownExample L
   LL=findSemigroups(9,10);#LL
   LL = flatten apply (11, g->findSemigroups g);#LL
   Lknown = select(LL, s -> isKnownExample s);#Lknown
   tally apply(Lknown, L->(
	if #L <= 3 then "Hilbert-Burch" else
	if #L == 4 and type L == 1 then "Buchsbaum-Eisenbud" else
	if weight L < genus L then "Eisenbud-Harris" else
	if ewt L < genus L then "Plueger" else
	if min L <6 then "Komeda-Low Multiplicity"))


SeeAlso
  findSemigroups
///

--<<docTemplate
 -* Test section *-
TEST///-* randomSemigroup *-
     L = {6,9,13,16}
     L' = randomSemigroup({6,9,13,16}, 5)
     F = res semigroupIdeal L;
     F'= res semigroupIdeal L';
     assert(
	 apply(1+length F, i-> rank F_i)==
	 apply(1+length F', i-> rank F'_i)
	 )
///

--Not sure about this test
-- TEST///-* isArf*-
-- assert(isArf {5,8,9,12} == false)
-- assert(isArf {5,8,9,11,12}==true)
-- assert(isArf {5,8,9,10,11}==false) -- the given gens aren't minimal
--

TEST///-* arfClosure and arfIndex*-
-- the Arf closure of an Arf semigroup is the semigroup itself
assert(isArf arfClosure {5,8,9,12})
assert(isArf arfClosure {3,5,7})
-- arfIndex is 0 exactly when the input is already Arf
assert(arfIndex {5,8,9,11,12} == 0)
assert(arfIndex {5,8,9,12} > 0)
-- Arf closure is idempotent
assert(arfClosure arfClosure {5,8,9,12} == arfClosure {5,8,9,12})
assert(arfClosure {5,8,9,12} == {5, 8, 9, 11, 12})
///

 TEST/// -*fractionalIdeal and numberToMonomial*-
--debug loadPackage("NumericalSemigroups", Reload => true)
debug needsPackage "NumericalSemigroups"
sgrp = {3,5}
   R = semigroupRing sgrp
   mons = semigroup sgrp
   assert(apply(mons, n -> numberToMonomial(R,n)) == {1, R_0, R_1, R_0^2, R_0*R_1, R_0^3, R_1^2})
   assert(
    I := fractionalIdeal(sgrp, {2,3});
    R = ring I;
    I == ideal(R_1,R_0^2)
    )
///
 
 TEST ///-*buchweitzSemigroups*-
assert(buchweitzSemigroups 6 == {})
--elapsedTime buchweitzSemigroups(13,16)
///



TEST/// -*aperySemigroupRing *-
L = {5,6}
assert(aperySet L == {6, 12, 18, 24})
assert(numgens aperySemigroupRing L == 1+ #aperySet L)
///

TEST/// -*semigroupIdeal*-
I = semigroupIdeal({4,5},
    "VariableName"=> z,
    "BaseField" => ZZ/3,
    "MinimalGenerators"=>false)
assert(char ring I == 3 and 
toString I == toString ideal(z_0^5-z_1^4))
///

TEST/// -*mu*-
assert(mu{5,7} == {4, 1, 5, 2})
assert(mu apery{5,7} == {4, 1, 5, 2})
///


TEST/// -*buchweitz*-
i = 2
gaps (B =  buchweitz i)
assert(#sums(2, gaps buchweitz i) == 3*(genus B -1)+1)
///

TEST/// -*def1*-
assert(def1{2,3}  == {-6, -4})
///

TEST/// -*burchIndex*-
assert(burchIndex {6,7,8,9,10,11} == 5 and
burchIndex (L = {3,4,5}) == 2 and
burchIndex {6,19,22, 35} == 0 and  -- this is Gorenstein and
burchIndex {14, 19, 21, 29, 36} == 3
)
///


TEST/// -*sums*-
   L1 = {2,3}
   L2 = {4,5}
   assert (sums(L1, L2) == {6,7,8})
   assert(sums(1, L1) == L1)
   assert(sums(2,L1) == {4,5,6})
///

TEST///-*gaps*-
assert((G = gaps{3,5}) == {1, 2, 4, 7})
s = select (semigroup {3,5}, i -> i < conductor{3,5})
assert(#G==#s)
///

TEST///-*findSemigroups*-
assert(findSemigroups(3,8,4) == {{3,5}})
assert(findSemigroups(5,14,8) == {
	{5, 7, 11},
	{5, 7, 9},
	{5, 6, 14},
	{5, 9, 11, 12}})
///

TEST///-*type*-
assert(type {3,4} == 1 and type {5,6,7,8,9} == 4)
///


TEST ///-* Buchweitz example of nonsmoothable semigroup and isGapSequence*-
G=toList(1..12)|{19,21,24,25}
L=isGapSequence G
G1=gaps L
G1==G
assert(#sums(G1,G1)>3*(#G1-1))
///

TEST///-*mingens*-
assert ((L = mingens {5,6, 8,9, 10,12, 13, 17}) == {5, 6, 8, 9})
assert(mingens {3, 5, 8, 9, 10}== {3,5})
assert(mingens ({3, 4, 5, 6, 7, 9, 10, 12}) == {3,4,5})
assert(mingens {3,3,5,5} == {3,5})
assert(mingens semigroup {3,7} == {3,7})
assert(mingens {8, 10, 31, 129, 130} == {8, 10, 31})
///    


TEST///-*socle*-
L = {9, 23, 28, 31}
assert(socle L == {84, 79, 62})
assert(socle {3,7} == {14})
assert(type {3,4,5} == #socle {3,4,5})
///

TEST/// -*conductor*-
assert(conductor {7, 24} == 6*23)
///

TEST///-*test of option*-
   R = semigroupRing({3,5,7}, "BaseField" => QQ)
   assert(char R == 0)
   ///

TEST///-*test of allSemigroups*-
   (H,M)=allSemigroups 3
   assert(H==matrix{{3,3},{3,6},{6,3}})
   assert(M==matrix{{4,5},{4,8},{7,5},{10,5}})
///

TEST///-*test of facetRays*-
assert(facetRays{3,4} == matrix"1;2")
assert(facetRays{3,5} == matrix"2;1")
assert(facetRays{3,4,5} == matrix"1,2;2,1")
///

TEST/// -*buchweitzCriterion*-
assert(buchweitzCriterion {7,12,13} == 0)
assert(buchweitzCriterion buchweitz 0 == -1)
assert(buchweitzCriterion buchweitz 2 == -1)
assert(buchweitzCriterion(3,{4,5}) == buchweitzCriterion {3,4,5})
///

TEST/// -*isSymmetric*-
assert(isSymmetric {3,5})
assert(isSymmetric {7,9})
assert(not isSymmetric {3,4,5})
///

TEST/// -*kunzMatrix*-
assert(kunzMatrix {4,7} == matrix {{0,0,0},{0,0,1},{0,1,1}})
assert(kunzMatrix {4,7} == transpose kunzMatrix {4,7})
assert(kunzMatrix {4,7} == kunzMatrix apery {4,7})
///

TEST/// -*weight*-
assert(weight {5,7} == 36)
assert(weight {6,7} == 55)
///

TEST/// -*ewt and effectiveWeight*-
assert(ewt {5,7} == 15)
assert(ewt {6,7} == 20)
assert(effectiveWeight {5,7} == ewt {5,7})
assert(ewt {5,7} <= weight {5,7})
///

TEST/// -*kunzRing*-
R = kunzRing {3,5};
assert(dim R == 0)
assert(isHomogeneous R)
///

TEST/// -*coneRays*-
importFrom(NumericalSemigroups, "coneEquations")
assert(coneRays 3 == matrix {{1,2},{2,1}})
assert(all(flatten entries ((transpose coneEquations 4) * coneRays 4), e -> e >= 0))
///

TEST///-*test of aperyConeEquations, muConeEquations*-
A1 = aperySet {3,4,5}
A2 = aperySet {3,5}
A3 = aperySet {3,7}
assert(aperyConeEquations 3 == aperyConeEquations {3,4,5})
assert((matrix{A1}*aperyConeEquations {3,5})_(0,2) == -6)
assert(
    aperyConeEquations {3,7} ==
    matrix {{2, -1, -2}, {-1, 2, 1}})

mu1 =matrix {mu {3,4,5}|{1}}
mu2 = matrix {mu {3,5}|{1}}
mu3 = matrix{mu {3,7}|{1}}
mu3*muConeEquations 3
mu2*muConeEquations {3,5}
mu3*muConeEquations {3,5}
mu3*muConeEquations {3,7}
///


TEST/// -*isKnownExample and findSemigroups*-
elapsedTime LL = flatten apply (11, g->findSemigroups g);#LL
assert(#LL == 478)
elapsedTime Lknown = select(LL, s -> isKnownExample s);#Lknown
assert(#LL - #Lknown == 116)

tally apply(Lknown, L->(
	if #L <= 3 then "Hilbert-Burch" else
	if #L == 4 and type L == 1 then "Buchsbaum-Eisenbud" else
	if weight L < genus L then "Eisenbud-Harris" else
	if ewt L < genus L then "Plueger" else
	if min L <6 then "Komeda-Low Multiplicity"))
	
///
end--
restart
needsPackage "NumericalSemigroups"
loadPackage ("NumericalSemigroups", Reload => true)
check "NumericalSemigroups"

restart
uninstallPackage "NumericalSemigroups"
restart
installPackage "NumericalSemigroups"
check "NumericalSemigroups"
viewHelp "NumericalSemigroups"
path

prepend ("./", path)
uninstallPackage "NumericalSemigroups"
installPackage "NumericalSemigroups"
peek loadedFiles

L1={13,14,15,16,17,18,20,22}
(gL1,bL1)=allSemigroups L1
buchweitzCriterion L1
b=aperySet L1
g=(entries gL1)_2
apply(10,i->(L=prepend(13,b+i*g);G=gaps L;-#sums(G,G)+3*(#G-1)))
L=prepend(13,b+2*g)

G=gaps L;#sums(G,G)-3*(#G-1)
(gL,bL)=allSemigroups L
gL=
first entries gL1
#bL
netList apply(bL,b->tally flatten apply(5,i->apply(4,j->(Lbij=prepend(15,(b+i*gL 0+j*gL_1));
		G=gaps Lbij; (#sums(G,G)-3*(#G-1))))))
(g,b)=allSemigroups LL_1
g=7
elapsedTime LL=flatten apply(toList(2..g),m->findSemigroups(m,2*g,g))




L=buchweitz 20
L=mingens ({10}|apply(7,i->random((i+1)*11)+11))
m=min L
a=aperySet L
am=mu apery L
am
G=gaps L

Gm=apply(m-1,i->select(G,j->j%m==i+1))
apply(m,k->  #unique flatten apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
	(sums(Gm_(i-1),Gm_(j-1))) else {}))-max apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
	(am_(i-1)+am_(j-1)-1) else 0)))



--netList apply(m,k->   apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
--	(sums(Gm_(i-1),Gm_(j-1))) else {})))


sum(m,k-> (mk=max  apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
	(am_(i-1)+am_(j-1)-1)   else 0));
         ki=select(toList(1..m-1),i->(j=(k-i)%m;
		 j=!=0 and  am_(i-1)+am_(j-1)-1 ==mk));
	 max apply(ki,i->(j=(k-i)%m;mk+if i+j>m then 1 else 0))))-1
oo==#sums(G,G)

sum(m,k->max  apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
	(am_(i-1)+am_(j-1)-1+if i+j>m then 1 else 0) else 0)))-1
oo==#sums(G,G)
#G,3*(#G-1)
buchweitzCriterion L

L={13}|apply(9,i->i+1+13)|apply(3,i->i+1+11+2*13)
aperySet L
(B,M)=allSemigroups L
g=((entries B)_0)_({0..4}|{9,6,9,7,8,10,11})
b=(entries M)_0
Lis=apply(10,i->{13}|b+i*g)
apply(Lis,L->buchweitzCriterion L)







apply(m,k-> max apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
	(#sums(Gm_(i-1),Gm_(j-1))) else 0)))

#sums(G,G)

apply(m,k->  unique flatten apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
	(sums(Gm_(i-1),Gm_(j-1))) else {})))==
apply(m,k->select(sums(G,G),j->j%m==k))

apply(m,k-> max apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
	(#sums(Gm_(i-1),Gm_(j-1))) else 0)))==
apply(m,k->  max apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
	(am_(i-1)+am_(j-1)-1) else 0)))

apply(m,k-> all apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
	(#sums(Gm_(i-1),Gm_(j-1)))==(am_(i-1)+am_(j-1)-1)  else true)))


sort flatten apply(m,k->  unique flatten apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
	(sums(Gm_(i-1),Gm_(j-1))) else {})))==
sums(G,G)

sum(m,k->  max apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
	(am_(i-1)+am_(j-1)-1) else 0)))
==
#sums(G,G)


)))
apply(m,k->  unique flatten apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
	(sums(Gm_(i-1),Gm_(j-1))) else {})))
netList apply(m,k->   apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
	(sums(Gm_(i-1),Gm_(j-1))) else {})))


apply(m,k->  unique flatten apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
	(sums(Gm_(i-1),Gm_(j-1))) else {})))==
apply(m,j->select(sums(G,G),i->i%m==j))
apply(m,k->  #unique flatten apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
	(sums(Gm_(i-1),Gm_(j-1))) else {}))==max apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
	(am_(i-1)+am_(j-1)-1) else 0)))

sum(m,k->#select(sums(G,G),i->i%m==k))
#sums(G,G)
sum(m,k->  max apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
	(am_(i-1)+am_(j-1)-1) else 0)))

tally apply(m,k->  #unique flatten apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
	(sums(Gm_(i-1),Gm_(j-1))) else {}))==max apply(toList(1..m-1),i->(j=(k-i)%m; if j=!=0 then 
	(am_(i-1)+am_(j-1)-1) else 0)))

18, 24, 25, 26, 28, 30, 33

--resolutions over Arf rings and minimal multiplicity, not Arf, rings.
restart
loadPackage ("NumericalSemigroups", Reload => true)
needsPackage "DirectSummands"
needsPackage "Indecomposables"

arfSequence = sgrp ->(
R = semigroupRing (sgrp, "BaseField" => ZZ/32003);
--infinitelyNearSemigroups sgrp;
MM = drop(infinitelyNearModules R, 1);
FF = apply(MM, M -> res (M, LengthLimit =>1));
netList apply(#MM, i -> reduceByList(MM, image ((FF_i).dd_1))))

testAtDepth = method()
testAtDepth Ideal := I -> (
dep = #gens ring I -length res I; -- depth of ring I, assuming I subset mm^2
F = res (coker vars ((ring I)/I), LengthLimit => dep+1);
reduceByList({image F.dd_dep}, image F.dd_(dep+1))
)


testAtDepth List := s -> testAtDepth semigroupIdeal s
testAtDepth Ring := R -> testAtDepth ideal R

isArf(sgrp = {5,8,9,11,12})
arfSequence sgrp
-- the syz of MM_0 is MM_0^4
reduceByList(MM, image ((FF_0).dd_1))
reduceByList(MM, image ((FF_1).dd_1))
--the syz of MM_1 is MM_0^2++MM_1^2
R = semigroupRing({5,9}, "BaseField" => ZZ/32003)
(findSummands R)_1
isArf(sgrp = {4,7,9,10})
arfSequence sgrp
isArf (sgrp = {4,11,13,14})
arfSequence sgrp
isArf (sgrp = {4,15,17,18})
arfSequence sgrp

--examples of minimal degree, not Arf:
g = 9
ss = findSemigroups g;#ss
ss1 = select(ss, s-> not isArf s);#ss1
ss2 = select(ss1, s-> #s == min s);#ss2
netList ss2
elapsedTime netList(ss2/(s -> testAtDepth s))--.25 sec for g = 7, 17sec for g=9

--elapsedTime for s in ss2 list (findSummands semigroupRing s)_1--10 sec for g = 7

s = {3,7,11}
isArf s
testAtDepth s
R = semigroupRing s
ideal R
findSummands R

needsPackage "Points"
needsPackage "Indecomposables"
I = randomPoints(3, 9)
R = ring I/I
L = findSummands R
inds = {coker vars R, image vars R}
F = res coker vars R
elapsedTime reduceByList(inds, image F.dd_2)

I = randomPoints(2, 5)
R = ring I/I
L = findSummands R
L_1
L_0_2
inds = {coker vars R, image vars R}
F = res coker vars R
elapsedTime reduceByList(inds, image F.dd_2)
ss = summands image F.dd_2
isIsomorphic(ss_0,ss_1)
netList ss
(uniqueUpToIsomorphism summands image F.dd_2)_0_0 -- this is the ideal one of the points
G = res oo 
uniqueUpToIsomorphism summands image G.dd_2 -- coordinate rings of all the points, with different multiplities

-- 5 points
I = randomPoints(3, 5)
R = ring I/I
F = res coker vars R
(uniqueUpToIsomorphism summands image F.dd_1)_0_0 -- first syz is the direct sum of the ideals of one of the  points
G = res oo 
numberOfSummands image G.dd_1 -- 1 irreducible
numberOfSummands image G.dd_2 -- 1 irreducible (larger); this is the third syzygy

-----
--rational normal quartic
restart
loadPackage ("Indecomposables", Reload => true)
loadPackage ("NumericalSemigroups", Reload => true)

kk = ZZ/32003
U = kk[s,t]
mm = ideal(s,t)
n=3
S = kk[x_0..x_n]
I = ker map(U,S,gens (mm^n))
R = S/I
findSummands R
netList toList oo

--three lines
I = intersect(ideal(x_0,x_1), ideal(x_1,x_2), ideal(x_2, x_3))
R = (ring I)/I
findSummands R
netList toList oo

--cone over 3 points
I = intersect(ideal(x_0,x_1), ideal(x_1,x_2), ideal(x_2, x_0))
R = (ring I)/I
findSummands R
netList toList oo

--
Kobayashi and Takahashi example
kk = ZZ/32003
S = kk[x,y,z,t, Degrees => {3,5,7,3}]
m = matrix"x,y,z;
y,z,x3-t3"
I = minors(2, m)
R = S/I
findSummands R
--
testAtDepth randomPoints(2,5)

testAtDepth semigroupIdeal{3,8, 10} --min deg, not arf. passes test
testAtDepth semigroupIdeal{4,5,6} --not min deg, not arf. fails test--it's CI!
testAtDepth semigroupIdeal{4,5,7} --not min deg, not arf. fails test, is Golod
semigroupRing{4,5,7}
(findSummands semigroupRing{4,5,6,7})_1
testAtDepth semigroupIdeal{4,9,10,11} -- min deg, not arf.  is Golod passes


needsPackage "EagonResolution"
viewHelp EagonResolution
R = semigroupRing{3,8,13}
F = eagonResolution eagon(R,3)
picture (F, Display => "DisplayBlocks")
summands coker F.dd_2
transpose F.dd_3
findSummands R
summands image F.dd_2
F.dd_2
K = koszul vars R
uniqueUpToIsomorphism summands ((ideal vars R)*image K.dd_2)

---
--3 lines in PP^3
S = ZZ/32003[a,b,c,d]
I =  intersect(ideal(a,b),ideal(b,c), ideal(a,c))
I =  intersect(ideal(a,b),ideal(b,c), ideal(a,d))
Q = ideal"ad-bc"
I = intersect(Q+ (ideal(a,b))^2, ideal(a,c))
betti res I
testAtDepth I
E = eagon(S/I,3)
F = eagonResolution(E)
picture(F, Display => "DisplayBlocks")
findSummands(S/I)
summands image F.dd_2
K = koszul vars (R = S/I)
summands prune image K.dd_2

M = matrix"a2,b3,c5,d2;
bc, acd, a2b2c, bd"
isHomogeneous (m = map(S^2,,M))
I = minors(2, m)
isGolod(S/I)
findSummands (S/I)
F = res coker vars (R=S/I)
summands image F.dd_3
numberOfSummands image F.dd_3
inds = {image F.dd_1, image F.dd_2, image F.dd_3}
elapsedTime reduceByList(inds, image F.dd_4)





--Simplest min deg non-Arf semigroup?
restart
loadPackage ( "NumericalSemigroups", Reload => true)
needsPackage "NumericalSemigroups"
needsPackage "Indecomposables"
L = findSemigroups 8
mmNotArf = select(L, s-> isMinimalMultiplicity s and not isArf s);#mmNotArf
arf8 = select(L, s-> isArf s)

--twisted cubic
S = ZZ/32003[a,b,c,d]
m = matrix"a,b,c;
b,c,d"
I = minors(2,m)
R =ring I/I
F = res coker vars R
F.dd_2
findSummands R
betti F

---
s = arf8_6
conductors = s -> (
    slist = infinitelyNearSemigroups s;
    RList = apply(#slist, i->semigroupRing slist_i);
    R = Rlist_0
    for S in Rlist list S
semigroupRing slist_1
slist_2
for ss in slist list semigroupRing ss

s = {3,5,7}
R = semigroupRing( s, "BaseField" => ZZ/32003)
MM = infinitelyNearModules R
F =res   MM_2
isIsomorphic(image F.dd_1, MM_1++MM_1)
F.dd_1

R1 = semigroupRing({2,3}, "BaseField" => ZZ/32003)
F = res 
NN =summands image F.dd_1
isIsomorphic(NN_0, NN_1)
isIsomorphic(NN_0, MM_1)
isIsomorphic(NN_1, MM_2)

PP = summands image (res  MM_1).dd_1
(res  MM_1).dd_1
degrees source oo



s = {4,6,9,11}
S6a

data = g ->(
S = findSemigroups g;
Sa = select(S, ss-> isArf ss);
netList for s in Sa list(
R = semigroupRing (s, "BaseField" => ZZ/32003);
(infinitelyNearSemigroups s)/(ss-> min ss);
inds = drop(infinitelyNearModules R, 1);
{s,for M in inds list reduceByList(inds, image((res M).dd_1))}
))

elapsedTime data 7
g=3
dS6a

mingens s
isArf s
infinitelyNearModules semigroupRing s
conductor s

--is ann Ri_R iso to Ri
S6a
--s = {4,6,11,13}
for s in S6a list(
R = semigroupRing (s, "BaseField" => ZZ/32003);
(infinitelyNearSemigroups s)/(ss-> min ss);
inds = drop(infinitelyNearModules R, 1);
inds
for M in inds list(
    e := (entries gens M)_0;
    mine := min (e/degree);
    p := positions(e, ee-> degree ee == mine);
    x := e_(min p);
    (isIsomorphic (module ann(M/ideal x) , M))_0
    )
)

s = {3,5}
prune((ideal gens R)/(ideal R_0))
ann oo

needsPackage "NumericalSemigroups"
#arf for g<=7:2g-4 but 8->13, 9-> 17, 10->21
g = 10
elapsedTime L = findSemigroups g
elapsedTime # unique select(L, s -> isArf s)

r = 4;d=11
L= toList(d-r+1..d)
g = #gaps L
m = (d-1)//(r-1)
eps = (d-1)%(r-1)
--ground truth:
di = g+2*m+eps -2
cod = 3*g-3-di
--versus:
ewt L
weight L

-----
Confusion: facetRays buchweitz 0
returns two rays, and one of the rays corresponds to
the apery set of  buchweitz 0.

But if a point on a cone lies on a ray, then
(in dimension >2) it should lie on two facets
(every face is an intersection of facets)
so what is facetRays actually doing?

???

restart
needsPackage "NumericalSemigroups"
F = facetRays buchweitz 0
mm = flatten entries F_{1}

A1 = for i from 1 to 12 list 13*mm_(i-1)+i
A2 = aperySet buchweitz 0
assert(A1 == A2)

This would make sense if "facetRays L" actually computes
the rays of the closed face -- not facet -- on which L lies.

facetRays {4,5,6}
mu{4,5,6} is half of the sum of the facet rays

----
semigroupFromAperySet = a -> mingens prepend (#a+1, a)

L = {6,9,13,16}
LL = apply(numcols facetRays L, i ->(
            (facetRays L)_{i} + transpose matrix {aperySet L}))
ss = for M in LL list semigroupFromAperySet flatten entries M
FF = apply(#ss, i-> res semigroupIdeal ss_i);
(FF_0).dd_3
degrees FF_0_3

F = res semigroupIdeal L;
F.dd_3
degrees F_3

 L in Lknown list (
      if L =!= null then continue else L)

---------
LL = flatten apply (13, g -> findSemigroups g);#LL -- 1413
Lknown = select(LL, L->isKnownExample L);#Lknown -- 781

tally apply(Lknown, L->(
	if #L == 2 then "Pinkham" else
      	if #L== 3 then "Hilbert-Burch" else -- Waldi 1981 124
      	if #L == 4 and type L == 1 then "Buchsbaum-Eisenbud" else -- Waldi 1981 16
      	if weight L < genus L then "Eisenbud-Harris" else -- 1987 455
      	if min L <6 then "Komeda-Low Multiplicity" else --1992 111
	if genus L < 8 then "Komeda-Low genus" else --1994 -- 3
      	if ewt L < genus L then "Plueger") --2018 -- 72
    )

netList flatten for i from 3 to 7 list (for j from i+1 to 8 list(
          if gcd{i,j} == 1 then (i,j, genus {i,j}) else continue))

netList  for i from 0 to 12 list ((1.618)^(i+1)/#findSemigroups i)
24+99+16+455+112+72+3

Haure's example (p. 165)
: genus 11
Gaps = {1,2,3,5,6,7,9,10,13,17,21}
peek loadedFiles
