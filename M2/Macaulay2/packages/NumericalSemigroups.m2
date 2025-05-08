newPackage(
    "NumericalSemigroups",
    Version => "1.0",
    Date => "October 25, 2024",
    Headline => "Compute the Apery set and invariants of a numerical semigroup ring",
    Authors => {{ Name => "David Eisenbud", Email => "de@berkeley.edu", HomePage => "http://eisenbud.github.io"},
	        { Name => "Frank-Olaf Schreyer", Email => "schreyer@math.uni-sb.de", HomePage => "https://www.math.uni-sb.de/ag/schreyer/index.php/publications/publications-frank-olaf-schreyer"}},
    AuxiliaryFiles => false,
    DebuggingMode => false,
    PackageExports => {"FourierMotzkin","Normaliz", "IntegralClosure", "FastMinors",  "RandomPoints"},
    Keywords => {"Commutative Algebra", "Algebraic Geometry", "Combinatorics"}
    )
///
restart

loadPackage ("NumericalSemigroups", Reload=>true)
check "NumericalSemigroups"
uninstallPackage "NumericalSemigroups"
restart
installPackage "NumericalSemigroups"

viewHelp "NumericalSemigroups"
viewHelp Normaliz

///
--"done" indicates that there is a TEST installed for that function.
export { 
    "apery", --done FOS
    "aperySet", --done FOS
    "semigroup", -- done FOS
    "isSymmetric",-- done FOS
    "mu", --done
    "socle",--done last
    "type", -- done
    "kunzMatrix", --done FOS
    "semigroupIdeal", --done
    "semigroupRing", -- FOS: take a look
    "aperySemigroupRing", -- done
    "gaps", -- done
    "isGapSequence", --done
    "findSemigroups", --done
    "sums", --done
    "coneEquations",
    "coneRays",
    "facetRays",
    "allSemigroups",
    "def1", --done -- degrees of first order deformations, or the generic first-order deformation itself.
    "weight",
    "makeUnfolding", -- done
    "flatteningRelations", --done
    "isSmoothableSemigroup", --done
    "ewt",
    "effectiveWeight", -- synonym for ewt
    "kunzRing",--done FOSthe semigroup ring of R mod the multiplicity element
    "burchIndex", --done
    "buchweitz", --done
    "buchweitzCriterion", --done
    "buchweitzSemigroups", --done
    "findPoint", --done
    "heuristicSmoothness", --done
    "knownExample", --done FOS
    "isARandomFiberSmooth", --done FOS
    "getFlatFamily", --done FOS
    "isWeierstrassSemigroup", -- done FOS
    "nonWeierstrassSemigroups", -- done FOS
    "LabBookProtocol", --done FOS
    "fractionalIdeal", --done
    }

-* Code section *-
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
///
restart
loadPackage("NumericalSemigroups", Reload => true)
coneEquations(4, "Inhomogeneous" => true)
L = {4,9,14,7}
allSemigroups L
coneEquations(L, "Inhomogeneous" => true)
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
ineq := transpose coneEquations m;
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
ineq0 := coneEquations m;
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

buchweitzCriterion = method()
buchweitzCriterion(List) := L -> (
    G:=gaps L;
    3*(#G-1)-#sums(G,G))

buchweitzCriterion(ZZ,List) := (m,L) -> (
    assert(#L==m-1);
    buchweitzCriterion(prepend(m,L)))

///

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
-*
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



*-
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

///

facetRays=method()
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

coneRays = method()
coneRays ZZ := Matrix => m -> -(fourierMotzkin coneEquations m)_0


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
   L := {};
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


apery = method()
apery List := HashTable => L -> (
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
    hashTable pairs A
    )

aperySet = method()
aperySet HashTable := List => A -> (
    K := select(keys A , k-> class k === ZZ);
    apply(sort K, k -> A#k)
    )
aperySet List := L -> aperySet apery L

semigroup = method()
semigroup List := List => L -> (apery L)#"semigroup"

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

positiveResidue = (p,q) -> if p<0 then p + (1+abs p//q)*q else p%q -- assumes q>0
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

kunzMatrix List := Matrix => L -> kunzMatrix apery L

type = method()
type List := ZZ => L -> #socle L

semigroupRing = method(Options => {"BaseField" => ZZ/101,
	                           "VariableName" => getSymbol "x",
			           "MinimalGenerators" => true})
			   
semigroupRing List := Ring => o-> L -> (
    I := semigroupIdeal(L,o);
    R := (ring I)/I;
    R.cache#"sgrp" = L;
    R
    )
semigroupIdeal = method(Options => {"BaseField" => ZZ/(nextPrime 10^3),
	                           "VariableName" => getSymbol "x",
			           "MinimalGenerators" => true})
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
assert all(LL, L -> transpose facetRays L * coneEquations L == 0)
LL = {{3,5},{3,4,5},{3,4}}
assert all(LL, L -> transpose coneEquations L * facetRays L == 0)
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

makeUnfolding=method(Options =>
      {Verbose => false,
      "BaseField" => ZZ/(nextPrime 10^3)})

makeUnfolding Ideal := o-> I ->(
    if not degreeLength ring I == 1 or
       not isHomogeneous I or
       I != trim I then
       error "expected N-graded homogeneous ideal
       given with minimal set of generators";
--    gbI := gb(I,ChangeMatrix =>true);
--    chMat := getChangeMatrix gbI;
    S := ring I;
    kk := coefficientRing S;
    degs := flatten degrees source gens I;
    unfoldingTerms := flatten for i from 0 to max degs-1 list (b:=basis(i,S/I); if b==0 then
	continue else (entries b))_0;
    unfoldingTerms2 := apply(degs,d->select(unfoldingTerms, t-> (degree t)_0 < d));
    a := symbol a;
    avars := flatten apply(#degs,i->apply(#unfoldingTerms2_i,j->a_{i,j}));
    adegs := flatten apply(#degs,i->apply(unfoldingTerms2_i,t->degs_i-(degree t)_0));
    A := kk[avars,Degrees=>adegs];
    avars= reverse sort(gens A,y->degree y);
    adegs=apply(avars,y->(degree y)_0);
    A = kk[avars,Degrees=>adegs];
    SA := kk[gens S|gens A,Degrees=>degrees S|degrees A];
    avars = apply (#degs,i->apply(#unfoldingTerms2_i,j->a_{i,j}));
    unfoldingTerms3 := matrix{apply(#degs,i->sum(#unfoldingTerms2_i,j->
	    sub(a_{i,j},SA)*sub((unfoldingTerms2_i)_j,SA)))}; 
    unfolding := sub(gens I,SA)+unfoldingTerms3;
    (A,unfolding)
    )

makeUnfolding List := o-> L -> (
        I:= trim ideal semigroupRing(L,"BaseField"=>o#"BaseField");
	makeUnfolding I)


-*
flatteningRelations1=method()
flatteningRelations1(Ideal,Ring, Matrix) := (I,A,unfolding) -> (
    gbI:=gb(I,ChangeMatrix=>true);
    S := ring I;
    SA := ring unfolding;
    chMat:=getChangeMatrix gbI;
    unfoldingGB := unfolding*sub(chMat,SA);
    ldT := flatten entries leadTerm unfoldingGB;
    s0:=syz sub(gens gbI,SA);s1:=null;
    --Now we compute the Buchberger test syzygies
    us:=null;testSyzygy1:=null;u1:=null;
    while (
	testSyzygy1=unfoldingGB*s0;
	us=apply(ldT,u->(u1=contract(u,testSyzygy1);
	   testSyzygy1=testSyzygy1-u*u1;
	   u1));
        s1=fold(us,{i,j}->i||j);
	not s1 == 0 ) do (
	s0 = s0-s1);
    --The flatteningRelations are the coefficients of testSyzygy2
    testSyzygy2:=unfoldingGB*s0;
    ma := max flatten degrees source syz leadTerm gens gbI;
    rems := reverse flatten for i from 0 to ma list (b:=basis(i,S^1/I); if b==0 then  continue else (entries b))_0;
    us = apply(rems,u->(u1:=contract(sub(u,SA),testSyzygy2);testSyzygy2-sub(u,SA)*u1;
	u1));
    relsA:=sub(ideal(flatten us),A);
    relsA
     )
*-

flatteningRelations=method()
flatteningRelations(Ideal,Ring, Matrix) := (I,A,unfolding) -> (
    gbI:=gb(I,ChangeMatrix=>true);
    S := ring I;
    SA := ring unfolding;
    chMat:=getChangeMatrix gbI;
    unfoldingGB := unfolding*sub(chMat,SA);
    -- can one use the build in gb algorithm to compute the
    -- flattening relations faster
    unfGBf:=forceGB unfoldingGB;
    ldT := flatten entries leadTerm unfoldingGB;
    s0:=syz sub(gens gbI,SA);
    testSyzygy1:=unfoldingGB*s0;
    testSyzygy2:=testSyzygy1%unfGBf;
    u1:=null;
    ma := max flatten degrees source syz leadTerm gens gbI;
    rems := reverse flatten for i from 0 to ma list (b:=basis(i,S^1/I); if b==0 then  continue else (entries b))_0;
    us := apply(rems,u->(u1=contract(sub(u,SA),testSyzygy2);testSyzygy2-sub(u,SA)*u1;
	u1));
    relsA:=sub(ideal(flatten us),A);
    relsA
     )




isSmoothableSemigroup=method(Options =>
      {Verbose => false,
      "BaseField" => ZZ/(nextPrime 10^3)})

isSmoothableSemigroup(List,RR,ZZ) := o-> (L,r,n) -> (
    (I,J1,family) := getFlatFamily(L,r,n,o);
    isARandomFiberSmooth(I,J1,family,o))


ewt=method()
ewt(List):= L -> (
    G:=gaps L;
    L1:=mingens L;
    sum(L1,a->#select(G,b->a<b))
    )
effectiveWeight = method()
effectiveWeight List := sgrp -> ewt sgrp

findPoint=method(Options => {Verbose => false})
    
findPoint(Ideal) := o -> (c) -> (
    c1 := prune c;  
    R := ring c;
    A1 := vars R % c;
    if c1==0 then return sub(A1,random(R^1,R^(numgens R)));
    if o.Verbose then << "has to solve" <<flush<< endl;
    kk:= coefficientRing R;
    leftOverVars := support c;
    R1:=kk[support c];
    cR1:=sub(c,R1);
    isHomogeneous cR1;
    if o.Verbose then (
    elapsedTime point := sub(matrix randomPoints(cR1,Homogeneous=>false),kk);) else (
                point = sub(matrix randomPoints(cR1,Homogeneous=>false),kk));
    subs1:=apply(#leftOverVars,i->leftOverVars_i => point_(0,i));
    assert(sub(cR1,subs1)==0);
    --ring c === ring A1;
    A2:=sub(sub(A1,subs1),R);
    return sub(A2,random(R^1,R^(numgens R)));
    )
    
    
 

getFlatFamily=method(Options =>
      {Verbose => false,
      "BaseField" => ZZ/(nextPrime 10^3)})

getFlatFamily(List,RR,ZZ) := o -> (L,r,n) -> (
    I := semigroupIdeal(L, "BaseField"=> o#"BaseField");
    if o.Verbose then (
        degsT1:=-def1 L;
	<< "unfolding" <<endl<<flush;
        elapsedTime (A,unfolding):= makeUnfolding I;) else (
	degsT1=-def1 L;
	(A,unfolding)= makeUnfolding I);
    ma:=max degsT1;
    S:=ring I;
    SA:=ring unfolding;
    restrict:=ideal select( gens A, aa->(degree aa)_0<=ma*r+n);
    runfolding:=unfolding%sub(restrict,SA);
    if o.Verbose then (
	 <<"flatteningRelations"<<endl<<flush;
          elapsedTime J:=flatteningRelations(I,A,runfolding);
	  ) else (
	 J=flatteningRelations(I,A,runfolding));
    mA:= max flatten degrees A;
    if o.Verbose then (
	<<"next gb" << endl<<flush;
	elapsedTime gbJ:=forceGB gens gb(J,DegreeLimit=>mA);) else (
	gbJ=forceGB gens gb(J,DegreeLimit=>mA););
	varsAmodJ:=vars A%gbJ;
    J1:=sub(J,varsAmodJ);
    family:=sub(runfolding,sub(vars S,SA)|sub(varsAmodJ,SA));
    if J1==0 then assert (betti syz family==betti syz gens I);
    (I,J1,family))

isWeierstrassSemigroup=method(Options =>
      {Verbose => false,
      "BaseField" => ZZ/(nextPrime 10^3)})

isWeierstrassSemigroup(List,RR) := o -> (L,r) -> (
    if o.Verbose then (elapsedTime degT1:=def1(L);) else degT1=def1(L);
    truncations:=-unique select(degT1,d->d<0);
    I:=semigroupIdeal L;
    S:=ring I;
    (A,unfolding):=makeUnfolding I;
    ma:=max flatten (gens A/degree);
    truncations=select(truncations,d->d<ma*r+1);
    J1:=null; family:= null;
    for t in truncations do ( if o.Verbose then (<<t<<endl<<flush);
	 (I,J1,family)=getFlatFamily(L,0.0,t-1);
                 if isARandomFiberSmooth(I,J1,family,o) then
		 ( break return true));
    return false)
-*
estimateTruncationRatio=method()
estimateTruncationRatio(List) := LL -> (
    I:=null;S:=null;A:=null;unfolding:=null;
    J1:=null; family:= null;ratio:=null;degT1:=null;
    truncations:=null;ma:=null;t:=null;answer:=null;
    ratios:=apply(LL,L -> (
	    <<L <<endl<<flush;
	    elapsedTime degT1=def1(L);
    truncations=-unique select(degT1,d->d<0);
    I=semigroupIdeal L;
    S=ring I;
    (A,unfolding)=makeUnfolding I;    
    ma=max flatten degrees A;
    for t in truncations do (
	elapsedTime (<<t<<endl<<flush;
	elapsedTime (I,J1,family)=getFlatFamily(L,0.0,t-1);
                 --<<(isARandomFiberSmooth(I,J1,family),t) <<flush<< endl;
		 answer= isARandomFiberSmooth(I,J1,family));
                 if answer then
		 ( << t-1<<endl<<flush; ratio=(ma,max truncations,t-1); break ));
	     ratio)
	 );
	 ratios)
*-
///
---- current test ---------
restart
debug loadPackage("NumericalSemigroups",Reload=>true)
L={6, 9, 14, 19, 22}
genus L

apply(LL,L->(I=semigroupIdeal L; (A,unfolding)=makeUnfolding I;
                mA= max flatten degrees A;degT1=-def1 L;(ma,max degT1)))



elapsedTime isWeierstrassSemigroup L



J1==0
J1
SA=ring family
ffam=res ideal family
ffam.dd_4
isSmoothableSemigroup(L,0.2,0)
///

isARandomFiberSmooth=method(Options =>
      {Verbose => false,
      "BaseField" => ZZ/(nextPrime 10^3)})
isARandomFiberSmooth(Ideal,Ideal,Matrix) := o -> (I,J1,family) -> (
    S := ring I;
    A := ring J1;
    fib:=null;answer:= null;
    if J1==0 then (fib = ideal sub(family,vars S|random(S^1,S^(numgens A)));
	answer = heuristicSmoothness(fib);
	if o.Verbose then <<answer <<endl<<flush;
	return answer);   
     kk := coefficientRing A;
     
--decompose here
-*   
     varsJ1:=support J1;
     R1:=kk[varsJ1];  
     elapsedTime J1ungraded:=trim ideal map(R1^1,,gens sub(J1,R1));
     << "in isARandomFiber decomposition, " <<  "(numgens,#support): " << (numgens J1ungraded,#support J1ungraded)<< endl;

     elapsedTime cJ1un:=decompose J1ungraded;
     cJ1:=apply(cJ1un,c->ideal map(A^1,,gens sub(c,matrix{ varsJ1})));
*-
    if o.Verbose then (<<"decompose" <<endl<<flush;
         elapsedTime cJ1:=decompose (J1); -- perhaps moved to a new ring
         <<"number of components: "<< #cJ1 <<endl<<flush;) else (
         cJ1=decompose J1);
    A2:=null;point:=null; 
    dims:=apply(cJ1,c->(
	    A2=findPoint(c);
	    --point=sub(sub(h,A2),coefficientRing A);
	    point=sub(A2,coefficientRing A);
	    assert(
		sub(J1,point)==0);
	    fib=ideal sub(family,vars S|point);
	    ---break return fib;
	    --assert(betti syz gens fib == betti syz gens I);
	    answer = heuristicSmoothness(fib);
	    --  singF=radical ideal gens gb (fib +minors(#mingens L-1,jacobian fib));
	    if answer then -1 else 0
	    --dim singF
	    ));
      if o.Verbose then (
	  << "support c, codim c: " <<apply(cJ1,c->(#support c,codim c)) <<endl<<flush;
	  <<dims <<endl<<flush;);
      return min dims ==-1 )

 
///
LL12d
L=LL12d_2
filterSmoothable(L,0.30,1)

///


  
heuristicSmoothness=method(Options =>
      {Verbose => false,
      "BaseField" => ZZ/(nextPrime 10^3)})
heuristicSmoothness(Ideal) := o -> fib -> (
    --VerifyNonRegular:= null;
    --regularInCodimension(1, (ring fib)/fib, VerifyNonRegular=>true, Verbose=>true) )
    jac := jacobian fib;
    kk:=coefficientRing ring fib;
    R1:=kk[support fib];
    points:= fib;N:=1;
    numberOfSingPoints:=null;dimPoints:=null;
    rad:=null;someMinors:=null;
    while (
	someMinors = chooseGoodMinors(20*N,numgens ring fib-1,jac);
	points = ideal gens gb (points+someMinors);
	dimPoints=dim points;
	if dimPoints>-1 then (
	    rad = radical points;
	    numberOfSingPoints=degree sub(rad,R1););
    dimPoints>-1 and numberOfSingPoints>1 and N<2) do (N=N+1);
    if dimPoints==-1 then return true;
    jacpt:=sub(jac,vars ring fib%rad);
    if numberOfSingPoints==1 then (
	return rank jacpt==codim fib) else (
	if o.Verbose then (
	<<"numberOfSingPoints "<<numberOfSingPoints <<endl<<flush;);
        return false)
	--return dim(rad+minors(codim fib,jacpt))==-1)
    )


knownExample=method()
knownExample(List) := L-> (
    if #L<=3 then return true;--determinantal
    if #L == 4 and type L == 1 then return true;--pfaffian
    --if L is a generalized Arithmeti cProgression L
    --then in some cases the paper of Oneto and Tamone shows smoothness
    g := genus L;
    if ewt L < g or min L <5 then return true else false)

nonWeierstrassSemigroups=method(Options =>
      {Verbose => false,
      "BaseField" => ZZ/(nextPrime 10^3)})
nonWeierstrassSemigroups(ZZ,ZZ) := o -> (m,g) -> (
    LL:= findSemigroups(m,g);
    LLa:=select(LL,L->not knownExample L);
    if o.Verbose then <<(#LL,#LLa) <<endl<<flush;
    r:=0.65;
    while ( LLa=select(LLa,L-> (
		if o.Verbose then (<<L<<endl<<flush;
		 elapsedTime not isSmoothableSemigroup(L,r,0,o)
		 ) else (not isSmoothableSemigroup(L,r,0,o))));
	     #LLa >6 ) do (r=r-0.05;if o.Verbose then (<<#LLa<<endl<<flush;<<endl;));
    LLa=select(LLa,L->(if o.Verbose then (<<L<<endl<<flush;
		elapsedTime not isWeierstrassSemigroup(L,r,o)) else (
		not isWeierstrassSemigroup(L,r,o))));
    if #LLa==0 then (<<(m,g," all semigroups are smoothable")<<flush<<endl);
    LLa)

nonWeierstrassSemigroups(ZZ,ZZ,List) := o -> (m,g,LLdifficult) -> (
    LL:= findSemigroups(m,g);
    LLa:=sort select(LL,L->not knownExample L);
    <<(#LL,#LLa) <<endl<<flush;
    LLa=select(LLa,L->not isMember(L,LLdifficult));
    r:=0.6;
    while ( elapsedTime LLa=select(LLa,L-> (<<L<<endl<<flush;
		 not elapsedTime isSmoothableSemigroup(L,r,0,o)));
	<<#LLa<<endl;<<endl<<flush;
	#LLa >6 ) do (r=r-0.1);
    <<LLa <<endl;
    n:= 0;
    elapsedTime LLa=select(LLa,L->(<<n<< L <<endl<<flush;n=n+1;
		elapsedTime not isWeierstrassSemigroup(L,r,o)));
    --if #LLa==0 then (<<(m,g," all but difficult semigroups are smoothable")<<flush<<endl);
    LLa|LLdifficult)


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
kunzRing = method()
kunzRing List := Ring => L -> (
    --returns the semigroup ring of R mod the multiplicity element
    R := semigroupRing L;
    m := min(gens R/degree);
    newvars := select(gens R, x -> degree x > m);
    newdegs := select(gens R/degree, d -> d>m);
    S := coefficientRing R[newvars, Degrees => newdegs];
    S/trim sub(ideal R, S)
    )

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

LabBookProtocol = method()
LabBookProtocol ZZ := String => g ->(

    if g == 7 then print("
    LL7=findSemigroups 7;#LL7
    LL7a=select(LL7,L->not knownExample L);#LL7a
    elapsedTime LL7b=select(LL7a,L->not isSmoothableSemigroup(L,0.25,0))
    LL7b=={} -- => every genus 7 semigroup is Weierstrass
    ");

    if g == 8 then print("
	LL8=findSemigroups 8;#LL8
	LL8a=select(LL8,L->not knownExample L);#LL8a
	elapsedTime LL8b=select(LL8a,L-> not isSmoothableSemigroup(L,0.40,0)) -- 16.7345 seconds elapsed
        LL8b=={{6,8,9,11}}
	elapsedTime LL8c=select(LL8b,L-> not isWeierstrassSemigroup(L,0.15)) --  1.88664 seconds elapsed
        LL8c=={} -- => every genus 8 semigroup is Weierstrass
    ");

    if g == 9 then print("
    LL9=findSemigroups 9;#LL9
    LL9a=select(LL9,L->not knownExample L);#LL9a
    elapsedTime LL9b=select(LL9a,L->(not isSmoothableSemigroup(L,0.5,0)));#LL9b -- 134.401 seconds elapsed
    LL9b
    elapsedTime LL9c=select(LL9b,L->(not isSmoothableSemigroup(L,0.4,0)));  -- 26.7357 seconds elapsed
    LL9c=={} -- => every genus 8 semigroup is Weierstrass
    ");

    if g == 10 then print("
    LL10=findSemigroups 10;#LL10
    LL10a=select(LL10,L->not knownExample L);#LL10a
    elapsedTime LL10b=select(LL10a,L-> elapsedTime not isSmoothableSemigroup(L,0.6,0)); -- 418.486 seconds elapsed
    #LL10b 
    elapsedTime LL10c=select(LL10b,L->(<<L<<endl<<flush;elapsedTime not isSmoothableSemigroup(L,0.5,0))); -- 173.422 seconds elapsed-
    #LL10c 
    elapsedTime LL10d=select(LL10c,L->(<<L<<endl<<flush;elapsedTime not isSmoothableSemigroup(L,0.45,0))); -- 156.571 seconds elapsed
    LL10d
    elapsedTime LL10e=select(LL10d,L->(<<L<<endl<<flush;elapsedTime not isWeierstrassSemigroup(L,0.4)));   -- 197.321 seconds elapsed 
    LL10e=={} -- => every genus 10 semigroup is Weierstrass
    ");

    if g == 11 then print("
    LL11=findSemigroups 11;#LL11
    LL11a=select(LL11,L->not knownExample L);#LL11a
    last LL11a, last LL11 -- shows that all examples of genus 11 not covered by Plueger have multiplicity <= 10.

    elapsedTime nonWeierstrassSemigroups(5,11) -- 117.422 seconds elapsed - 
    LLdifficult={{6, 8, 17, 19, 21},{6, 8, 10, 19, 21, 23},{6, 9, 11, 14}}
    elapsedTime nonWeierstrassSemigroups(6,11,LLdifficult,Verbose=>true)   -- 267.818 seconds elapsed
    --(6, 11,  all but difficult semigroups are smoothable)
    elapsedTime LL611=select(LLdifficult,L->(<<L<<endl<<flush;elapsedTime not isWeierstrassSemigroup(L,0.4,Verbose=>true)));   -- 197.l321 seconds elapsed 

    elapsedTime nonWeierstrassSemigroups(7,11, LLdifficult, Verbose => true) --257 sec
    LLdifficult={{8, 9, 11, 15, 21}}
    elapsedTime nonWeierstrassSemigroups(8,11, LLdifficult, Verbose => true)
    LLdifficult={}
    elapsedTime nonWeierstrassSemigroups(9,11, LLdifficult, Verbose => true)
    LLdifficult={}
    elapsedTime nonWeierstrassSemigroups(10,11, LLdifficult, Verbose => true)
    ");
    )


    
///
restart
debug loadPackage( "NumericalSemigroups", Reload => true)
LL11=findSemigroups 11;#LL11
    LL11a=select(LL11,L->not knownExample L);#LL11a
    last LL11a, last LL11 -- shows that all examples of genus 11 not covered by Plueger have multiplicity <= 10.

LabBookProtocol 7
    LL7=findSemigroups 7;#LL7
    LL7a=select(LL7,L->not knownExample L);#LL7a
    elapsedTime LL7b=select(LL7a,L->not isSmoothableSemigroup(L,0.25,0,Verbose=>true))
    elapsedTime LL7b=select(LL7a,L->not isSmoothableSemigroup(L,0.25,0))
    LL7b=={}
      
    LL8=findSemigroups 8;#LL8
    LL8a=select(LL8,L->not knownExample L);#LL8a
    elapsedTime LL8b=select(LL8a,L-> not isSmoothableSemigroup(L,0.40,0)) -- 16.7345 seconds elapsed
    LL8b=={{6,8,9,11}}
    elapsedTime LL8c=select(LL8b,L-> not isWeierstrassSemigroup(L,0.15)) -- 1.88664 seconds elapsed
    LL8c=={} -- => every genus 8 semigroup is Weierstrass
    
    LL9=findSemigroups 9;#LL9
    LL9a=select(LL9,L->not knownExample L);#LL9a
    elapsedTime LL9b=select(LL9a,L->(not isSmoothableSemigroup(L,0.5,0)));#LL9b -- 134.401 seconds elapsed
    LL9b
    elapsedTime LL9c=select(LL9b,L->(not isSmoothableSemigroup(L,0.4,0)));  -- 26.7357 seconds elapsed
    LL9c=={} -- => every genus 9 semigroup is Weierstrass


    LL10=findSemigroups 10;#LL10
    LL10a=select(LL10,L->not knownExample L);#LL10a
    elapsedTime LL10b=select(LL10a,L-> elapsedTime not isSmoothableSemigroup(L,0.6,0)); -- 418.486 seconds elapsed
    #LL10b 
    elapsedTime LL10c=select(LL10b,L->(<<L<<endl<<flush;elapsedTime not isSmoothableSemigroup(L,0.5,0))); -- 173.422 seconds elapsed-
    #LL10c 
    elapsedTime LL10d=select(LL10c,L->(<<L<<endl<<flush;elapsedTime not isSmoothableSemigroup(L,0.45,0))); -- 156.571 seconds elapsed
    LL10d
    elapsedTime LL10e=select(LL10d,L->(<<L<<endl<<flush;elapsedTime not isWeierstrassSemigroup(L,0.4)));   -- 197.321 seconds elapsed 
    LL10e=={} -- => every genus 10 semigroup is Weierstrass

    elapsedTime nonWeierstrassSemigroups(5,11,Verbose =>true) -- 117.422 seconds elapsed - 

    LLdifficult={{6, 8, 17, 19, 21},{6, 8, 10, 19, 21, 23},{6, 9, 11, 14}}
    elapsedTime nonWeierstrassSemigroups(6,11,LLdifficult,Verbose=>true)   -- 267.818 seconds elapsed
    --(6, 11,  all but difficult semigroups are smoothable)
    elapsedTime LL611=select(LLdifficult,L->(<<L<<endl<<flush;elapsedTime not isWeierstrassSemigroup(L,0.4,Verbose=>true)));   -- 197.321 seconds elapsed 


    LLdifficult ={}
    elapsedTime nonWeierstrassSemigroups(7,11,LLdifficult,Verbose=>true)   -- 267.818 seconds elapsed
    elapsedTime LL711=select(LLdifficult,L->(<<L<<endl<<flush;elapsedTime not isWeierstrassSemigroup(L,0.4,Verbose=>true)));   -- 197.321 seconds elapsed 
    -- 951.724 seconds elapsed
    --o3 == {}

    LLdifficult ={{8, 9, 11, 15, 21}}
    elapsedTime nonWeierstrassSemigroups(8,11,LLdifficult,Verbose=>true)   
    elapsedTime LL811=select(LLdifficult,L->(<<L<<endl<<flush;elapsedTime not isWeierstrassSemigroup(L,0.4,Verbose=>true)));   -- 197.321 seconds elapsed 
    

    LLdifficult ={}
    elapsedTime nonWeierstrassSemigroups(9,11,LLdifficult,Verbose=>true)   
    elapsedTime LL911=select(LLdifficult,L->(<<L<<endl<<flush;elapsedTime not isWeierstrassSemigroup(L,0.4,Verbose=>true)));   -- 197.321 seconds elapsed 
    -- 998.636 seconds elapsed
    o4 = {}
    
        LLdifficult ={}
    elapsedTime nonWeierstrassSemigroups(10,11,LLdifficult,Verbose=>true)   
    elapsedTime LL1011=select(LLdifficult,L->(<<L<<endl<<flush;elapsedTime not isWeierstrassSemigroup(L,0.4,Verbose=>true)));   -- 197.321 seconds elapsed
    -- 2104.78 seconds elapsed
    --o5 == {}

    
     LLdifficult = {{6, 9, 17, 19, 20, 22},}
     elapsedTime nonWeierstrassSemigroups(6,12,LLdifficult,Verbose=>true)   
    elapsedTime LL612=select(LLdifficult,L->(<<L<<endl<<flush;elapsedTime not isWeierstrassSemigroup(L,0.4,Verbose=>true)));   -- 197.321 seconds elapsed 
   



 
    LLdifficult={}
    elapsedTime nonWeierstrassSemigroups(5,12,LLdifficult,Verbose=>true)  -- 152.485 seconds elapsed
    oo == {}
 
    LLdifficult={}
    elapsedTime nonWeierstrassSemigroups(7,12,LLdifficult,Verbose=>true)  -- 152.485 seconds elapsed
    oo == {}

   


///    
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
     c := conductor sgrp;
     mons0 := min mons;
     ans := {c}; -- a default value.
     ss := semigroup sgrp;
     nonGens :=sums(sgrp, mons);
     newMons := sort (mons | nonGens);
     for i from -mons0 to c do(
        newMonsPlus := sums({i}, newMons);
        m := select(newMonsPlus, j -> j<c);
	if isSubset(m, ss) then(
	        ans = sums({i}, newMons);
	        break));
     if not o#"Ideal" then ans else(
     R := semigroupRing sgrp;
     --now make ans into an ideal of R.
     trim(ideal for n in ans list numberToMonomial(R,n))
     ))


     
///
restart
debug loadPackage("NumericalSemigroups", Reload => true)
   sgrp = {5,9}
   R = semigroupRing sgrp
   mons = semigroup sgrp
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

///
-* Documentation section *-

beginDocumentation()


document {
Key => NumericalSemigroups,
Headline => "Compute invariants of a numerical semigroup",
   "In this package we consider numerical semigroups: that is, cofinite subsets of the natural numbers that are closed under sums.
   We generally refer to these simply as semigroups.
   
   A semigroup S thus includes the empty sum, 0, but we input semigroups by giving generators, all nonzero.
   The smallest nonzero element of S is the multiplicity. The Apery set (really sequence) of a semigroup S is the
   the list {a_1..a_m-1} where a_i is the smallest element in S such that a_i = i mod m.
   The conductor is 1 plus the largest element not in S. We generally specify a semigroup by giving
   a list of positive integers L with gcd = 1, representing the semigroup of all sums of
   elements of L.",

   PARA{},
     SUBSECTION "Combinatorial properties of the Kunz cone",
     UL{
	TO coneEquations,
	TO mu,
	TO facetRays,
	TO coneRays,
	TO allSemigroups,
	TO findSemigroups,
	TO buchweitzCriterion,
	TO buchweitz,
	TO buchweitzSemigroups,
        },
     SUBSECTION "Properties of semigroup rings",
     UL{
        TO burchIndex,
	TO semigroupRing,
	TO socle,
	TO kunzRing,
	TO isSymmetric
        },
     SUBSECTION "Weierstrass semigroups",
     "The question whether every semigroup is a Weierstrass semigroup was answered negatively 
     by Buchweitz: the semigroup generated by {13, 14, 15, 16, 17, 18, 20, 22, 23} is not a Weierstrass semigroup,
     as demonstrated in ", TO buchweitz,".
     On the other hand Pinkham gave a positive criterion with deformation theory.
     A semigroup is a Weierstrass semigroup if and only if the graded semigroup ring of L 
     has a smoothing deformation with strictly positive deformation parameters.",
     PARA{},

     "In this section we implemented Pinkham's approach in POSITIVE CHARACTERISTIC. We plan
     to extend the smoothing results to characteristic 0 in the future.",
     UL{
	TO makeUnfolding,
	TO flatteningRelations,
        TO getFlatFamily,
	TO findPoint,
	TO isARandomFiberSmooth,
	TO heuristicSmoothness,
--	TO isSmoothableSemigroup,
	TO isWeierstrassSemigroup,
	TO nonWeierstrassSemigroups,
	TO LabBookProtocol,
        }     
}

doc ///
Key
 LabBookProtocol
 (LabBookProtocol, ZZ)
Headline
 Weierstrass Semigroups in Low genus
Usage
 s = LabBookProtocol g
Inputs
 g:ZZ
  genus
Outputs
 s:String
  commands to study semigroups of genus g
Description
  Text
   This function prints a series of commands that check that most semigroups of genus g (up to g = 10) are Weierstrass.
   It outputs a short list of "difficult" examples that currently take too long to check.
  Example
   LabBookProtocol 7
   LL7=findSemigroups 7;#LL7
   LL7a=select(LL7,L->not knownExample L);#LL7a
   elapsedTime LL7b=select(LL7a,L->not isSmoothableSemigroup(L,0.25,0,Verbose=>true))
   elapsedTime LL7b=select(LL7a,L->not isSmoothableSemigroup(L,0.25,0))
   LL7b=={}
  Text
    With the option Verbose=>true one gets timings on various parts of the computation.
    To check all semigroups of genus g=8,9 and 10 takes about

    18.2, 161.1 and 945.6 seconds respectively.

  Example
   LabBookProtocol 11
  Text
   Since the number of cases gets rather large, we break up the list of all semigroups
   into sublists of semigroups of given multiplicity and call the function nonWeierstrassSemigroups:
  Example
   m=5,g=8
   elapsedTime nonWeierstrassSemigroups(m,g,Verbose=>true)
  Text
   In the verbose mode we get timings of various computation steps and further information.
   The first line,
   (13,1),
   indicates that there 13 semigroups of multiplicity 5 and genus 8 of which only 1 is not flagged
   as smoothable by the function knownExample.
   The second line,
   {5,8,11,12},
   gives the current semigroup.
   The timing under various  headers tells how much time was used in each of the steps.
  Example
   L={6,8,9,11}
   genus L
   isWeierstrassSemigroup(L,0.2,Verbose=>true)
  Text
   The first integer, 6, tells that in this attempt deformation parameters of degree >= 6 were used and no smooth fiber was found.
   Finally with all parameters of degree >= 4, the flatteningRelations define a scheme that decomposes into 2 components,
   both affine spaces. If we encounter non affine components we print  "has to solve", and find a point in each such component.
   We then print the number of singular points in the fiber.
   Finally the output "{0,-1}" is the dimension of the singular loci of a random fiber over each component.
   Thus the entry "-1" indicates that a general fiber of the second component is smooth.
   
SeeAlso
  isSmoothableSemigroup
  isWeierstrassSemigroup
  nonWeierstrassSemigroups
  knownExample
///

doc ///
Key
 knownExample
 (knownExample, List)
Headline
 Is L a known Weierstrass semigroup?
Usage
 b = knownExample L
Inputs
 g:ZZ
  genus
Outputs
 b:Boolean
  true if L is a known example of a Weierstrass semigroup
Description
  Text
   Certain semigroups are known to be Weierstrass. For example L has 2  or 3 generators only, by work of Pinkham and Sally. 
   Another sort examples are semigroup with small weight ewt(L) < genus L by the work Nathan Pflueger extending
   work of Eisenbud-Harris.
  Example
   L={7,12,13}
   knownExample L
   L={7,8,9,11,13}
   ewt L, genus L
   knownExample L
   LL=findSemigroups(9,10);#LL
   select(LL,L->not knownExample L)
   #oo
SeeAlso
  LabBookProtocol
  findSemigroups
///

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

elapsedTime J=flatteningRelations(I,A,unfolding);
elapsedTime gb J; dim J;
elapsedTime (A1=vars A%J)

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
   T^1(B) is the tangent space to the versal deformation of
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
   Uses the Fourier-Motzkin algorithm to go from
   the @TO coneEquations@ satisfied by the semigroup
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
   On the face with the @TO buchweitz@ example there are two facet rays:
  Example
   F = facetRays buchweitz 0
  Text
   The second column is mu buchweitz 0, the mu vector of the Buchweitz example.
   Adding multiples of it to the Weierstrass semigroups ordinary point
   of genus 12, we eventually reach a semigroup that fails the Buchweitz
   test to be a Weierstrass semigroup:
  Example
   b = {0}|flatten entries F_{1}
   L = toList (13..25)
   for i from 0 to 15 list (
       L' = L+i*13*b;
       G = gaps L';
       #sums(G, G) - 3*(genus L' -1)
       )
  Text
    We conjecture that the same phenomen  for any semigroup L0
    of multiplicity 13 in place of L. Here is a "random" example:
  Example
    setRandomSeed 0
    L0 = {13}|aperySet ({13}|apply(1 + random 10, i->13+random 10))
    for i from 0 to 20 list (
       L' = L0+i*13*b;
       G = gaps L';
       #sums(G, G) - 3*(genus L' -1)
       )
SeeAlso
 aperySet
 coneEquations
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
  of integers; the rows form the Hilbert basis  of the cone
 M:Matrix
  of module generators of the cone 
Description
  Text
   Using Normaliz we compute the face of the Kunz cone containing L.
   In case of allSemigroups m the output describes the complete Kunz cone
   of all semigroups of multiplicity m.
  Example
   allSemigroups {4,7,9}
   allSemigroups 4
  Text
   On the face with the @TO buchweitz@ example there are two facet rays:
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
 coneEquations
 buchweitzCriterion
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
 makeUnfolding
 (makeUnfolding, Ideal)
 (makeUnfolding, List) 
 [makeUnfolding, "BaseField"]
 [makeUnfolding, Verbose ]
Headline
 Makes the universal homogeneous unfolding of an ideal with positive degree parameters
Usage
 (A,unfolding) = makeUnfolding I
 (A,unfolding) = makeUnfolding sgrp
Inputs
 I:Ideal
 sgrp:List
  generators of a semigroup
Outputs
 A: Ring
   algebra of unfolding parameters
 unfolding: Matrix
   equations of the unfolding
Description
  Text
   Given a (quasi)homogeneous ideal in a ring S = kk[x_0..x_n]
   the function creates a positively graded polynomial ring A = kk[a_{i,j}]
   and computes the unfolding of I as an ideal 
   of SA = kk[x_0..x_n, a_{i,j}]. This can be used as a step in computing the
   semi-universal deformation of the affine cone defined by I.

   In the case of

   makeUnfolding sgrp

   the routine first forms the ideal of the semigroup ring, and applies makeUnfolding to this.
  Example
   L={4,5,7}
   I := semigroupIdeal L;
   (A,unfolding):= makeUnfolding I;
   S=ring I
   fI=res I
   degs=flatten (gens A/degree)
   n=floor(max degs/2+3)
   restricted=ideal select(gens A, y-> (degree y)_0<n);
   SA=ring unfolding
   runfolding=unfolding%sub(restricted,SA);
   transpose runfolding
   J=flatteningRelations(I,A,runfolding);
   cJ=decompose J;#cJ
   ideal prune (A/J)
   family=runfolding%sub(J,SA);
  Text
   This is a flat family!
  Example
   betti res ideal family == betti res I
   fiber=ideal sub(family,vars S|random(S^1,S^(numgens A)));
   singFiber=radical ideal gens gb (fiber+minors(codim I,jacobian fiber))
  Text
   Thus the family is a smoothing of S/I so
   the semigroup L in the example is a Weierstrass semigroup by Pinkham's thesis.
SeeAlso
 flatteningRelations
///

doc ///
Key
 flatteningRelations
 (flatteningRelations, Ideal, Ring, Matrix)
Headline
 Compute the flattening relations of an unfolding
Usage
 J= flattening(I,A,unfolding)
Inputs
 I:Ideal
  homogeneous with respect to a possibly nonstandard NN-grading
 A: Ring
  the ring of parameters of the unfolding
 unfolding: Matrix
  an unfolding of gens I
Outputs
 J: Ideal
   of A
Description
  Text
   Given the tuple (I,A,unfolding) the function computes the flattening relations
   via the set of Buchberger test syzygies.
   The procedure terminates since the parameters
   of A have positive degree, and the unfolding is homogeneous.
  Example
   L={4,6,7}
   I = trim semigroupIdeal L;
   (A,unfolding)=makeUnfolding I
   S=ring I
   fI=res I
   degs=flatten (gens A/degree)
   n=floor(max degs/2)+3
   restricted=ideal select(gens A, y-> (degree y)_0<n);
   SA=ring unfolding
   runfolding=unfolding%sub(restricted,SA);
   transpose runfolding
   J=flatteningRelations(I,A,runfolding);
   cJ=decompose J;#cJ
   ideal prune (A/J)
   family=runfolding%sub(J,SA);
   betti res ideal family == betti res I
  Text
   Thus this is a flat family!
  Example
   fiber=ideal sub(family,vars S|random(S^1,S^(numgens A)));
   singFiber=radical ideal gens gb (fiber+minors(codim I,jacobian fiber))
  Text
   Thus the family is a smoothing of S/I so
   the semigroup L in the example is a Weierstrass semigroup by Pinkham's thesis.

SeeAlso
 makeUnfolding
 ///

doc ///
Key
 findPoint
 (findPoint, Ideal)
 [findPoint, Verbose ]
Headline
 Find a kk-rational point in a variety
Usage
 point=findPoint c
Inputs
 I:Ideal
Outputs
 B: Matrix
   coordinates of a point in the finite ground field
Description
  Text 
   Given ideal c the functions adds random linear equations L to c to obtain
   1-dimensional ideal. Since the ground field is finite, decompose the ideal c+L
   will lead to a point with positive probability. Thus repeating will lead to success.
  Example
    kk=ZZ/101
    R=kk[x_0..x_6]
    c=ideal random(R^1,R^{2:-1,2:-2})
    B=findPoint c
    sub(c,B)==0

  
///


doc ///
Key
 isSmoothableSemigroup
 (isSmoothableSemigroup, List, RR, ZZ)
 [isSmoothableSemigroup, "BaseField"]
 [isSmoothableSemigroup, Verbose ]
Headline
 Look for a smoothing family
Usage
 b=isSmoothableSemigroup(L,r,n)
Inputs
 L:List
   the generators of a semigroup
 r:RR
 n:ZZ
   numbers which influences the truncation
Outputs
 b: Boolean
   true if a smoothing family was found, false no smoothing family was found
Description
  Text
    After computing an unfolding and restricting the
    unfolding to variables of degree larger than
    
             (maximal degree of a parameter)*r+n,
	     
    we compute the flattening relations J of the restricted unfolding.
    If J defines a union of components X,
    we check whether the fiber over a random closed point of each X is smooth.
    If we find a smooth fiber we return true, else we return false.
  Example
    L={6,8,9,11}
    genus L
    elapsedTime isSmoothableSemigroup(L,0.30,0)
    elapsedTime isSmoothableSemigroup(L,0.14,0)
SeeAlso
 makeUnfolding
 flatteningRelations
 getFlatFamily
 isARandomFiberSmooth
 ///


doc ///
Key
 isWeierstrassSemigroup
 (isWeierstrassSemigroup, List, RR)
 [isWeierstrassSemigroup, "BaseField"]
 [isWeierstrassSemigroup, Verbose]
Headline
 Experimentally decide whether L is a Weierstrass semigroup
Usage
 b=isWeierstrassSemgroup(L,r)
Inputs
 L:List
   the generators of a semigroup
 r:RR
   numbers which influences the truncation
Outputs
 b: Boolean
   true if a smoothing family was found, false no smoothing family was found
Description
  Text
    After computing an unfolding we successivly restricting the
    unfolding to variables of degree larger  an integer n  for an n with
    
             n<=(maximal degree of a parameter)*r,
	     
    compute the flattening relations J of the restricted unfolding.
    If J defines a union of components X,
    we check whether the fiber over a random closed point of each X is smooth.
    If we find a smooth fiber we return true, else we continue with n-1 until we checked
    the full unfolding.
  Example
    L={6,8,9,11}
    genus L
    elapsedTime isWeierstrassSemigroup(L,0.15)
SeeAlso
 makeUnfolding
 flatteningRelations
 getFlatFamily
 isARandomFiberSmooth
 ///

doc ///
Key
 nonWeierstrassSemigroups
 (nonWeierstrassSemigroups, ZZ,ZZ)
 (nonWeierstrassSemigroups, ZZ,ZZ,List)
 [nonWeierstrassSemigroups, "BaseField"]
 [nonWeierstrassSemigroups, Verbose]
Headline
 Find possibly non Weierstrass Semigroups
Usage
 LL=nonWeierstrassSemgroups(m,g)
 LL=nonWeierstrassSemgroups(m,g,LLdifficult)
Inputs
 m:ZZ
   the multiplicity of a semigroup
 g:ZZ
   the genus of a semigroup
 LLdifficult: List
   List of difficult semigroups which we exclude from the test
Outputs
 LL: List
   List of possible non Weierstrass semigroups including LLdifficult
Description
  Text
    We test which semigroups of multiplicity m and genus g are smoothable.
    If no smoothing was found then L is a candidate for a non Weierstrass semigroup.
    In this search certain semigroups L in LLdifficult, where the computation is particular heavy are
    excluded.
  Example
    elapsedTime nonWeierstrassSemigroups(6,7)
    LLdifficult={{6, 8, 9, 11}}
    elapsedTime nonWeierstrassSemigroups(6,8,LLdifficult,Verbose=>true)
  Text
   In the verbose mode we get timings of various computation steps and further information.
   The first line,
   (17,5),
   indicates that there 17 semigroups of multiplicity 6 and genus 8 of which only 5 is not flagged
   as smoothable by the function knownExample.
   The second line,
   {6, 7, 8, 17},
   gives the current semigroup.
   The timing under various  headers tells how much time was used in each of the steps.   
SeeAlso
 isWeierstrassSemigroup
 LabBookProtocol
 ///







 
doc ///
Key
 getFlatFamily
 (getFlatFamily, List, RR, ZZ)
 [getFlatFamily, "BaseField"]
 [getFlatFamily, Verbose ]
Headline
 Compute the flat family depending on a subset of parameters of the universal unfolding
Usage
 (I,J1,family)=getFlatFamily(L,RR,ZZ)
Inputs
 L:List
   the generators of a semigroup
 r:RR
 n:ZZ
   numbers which influences the truncation
Outputs
 I: Ideal
    semigroup ideal
 J1:Ideal
    flatness relations among the parameters
 family: Matrix
    defining equation of the family
Description
  Text
    After computing an unfolding and restricting the
    unfolding to variables of degree larger than
    
             (maximal degree of a parameter)*r+n,
    
    we compute the flattening relations and remove dependent variables.
    The remaining flattening relation are returned in the ideal J1.
    Using the function isARandomFiberSmooth we then can check with good luck
    whether a random fiber over some component of J1 is smooth.
  Example
    L={6,8,10,11}
    genus L
    (I,J1,family)=getFlatFamily(L,0.30,0);
    betti res ideal family == betti res I
    isARandomFiberSmooth(I,J1,family)
    support family
    support family /degree
    gens ring J1 /degree
  Text
    Parameters of the universal unfolding of degree <= 22*0.3 are not used
  Example
    (I,J1,family)=getFlatFamily(L,0.00,11);
    support family
    support family /degree
  Text
    Parameters of the universal unfolding of degree < 11) are not used
  Example
    isARandomFiberSmooth(I,J1,family)
    A = ring family
    transpose family
SeeAlso
 makeUnfolding
 flatteningRelations
 getFlatFamily
 isARandomFiberSmooth
 ///

doc ///
Key
 isARandomFiberSmooth
 (isARandomFiberSmooth, Ideal, Ideal, Matrix)
 [isARandomFiberSmooth, "BaseField"]
 [isARandomFiberSmooth, Verbose ]
Headline
 Test whether a random fiber is smooth
Usage
 b=isARandomFiberSmooth(I,J1,family)
Inputs
 I: Ideal 
  semigroup ideal
 J1:Ideal
  flatness relations among the parameters
 family:Matrix
   a flat family
Outputs
 b: Boolean
   true if a random fiber is smooth, false otherwise
Description
  Text    
    We check whether a random fiber over a random closed point of each component of  J1 is smooth.
    If we find a smooth fiber we return true, else we return false.
  Example
    L={6,8,10,11}
    genus L
    (I,J1,family)=getFlatFamily(L,0.30,0);
    isARandomFiberSmooth(I,J1,family)
    SA=ring family
    transpose family
SeeAlso
 makeUnfolding
 flatteningRelations
 getFlatFamily
 ///

 doc ///
Key
  heuristicSmoothness
  (heuristicSmoothness, Ideal)
  [heuristicSmoothness, "BaseField"]
  [heuristicSmoothness, Verbose ]
Headline
  Check whether an affine curve is smooth
Usage
  b=heuristicSmoothness c
Inputs
  c: Ideal
    of an affine curve
Outputs
  b: Boolean
    true if the computation showes that c is smooth false otherwise
Description
  Text
    We  check for smoothness using only some of the minors of
    the jacobian matrix. If we are lucky this establishes smoothness.
    With bad luck we might fail even in case when c is smooth.   
  Example
    kk=ZZ/2; S=kk[x_0..x_3]
    setRandomSeed "some singular and some smooth curves";
    elapsedTime tally apply(10,i-> (
	    c=minors(2,random(S^2,S^{3:-2}));
	    c=sub(c,x_0=>1);
	    R=kk[support c];c=sub(c,R);
	    heuristicSmoothness c))
SeeAlso

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
   The semingroup ideal of the semigroup generated by L
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
   returns the semigroup ring modulo the element of least degree.
   The kunzRing shares many properties with the semigroup ring.
  Example
   semigroupRing {3,5}
   kunzRing {3,5} 
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
 coneEquations
 (coneEquations, ZZ)
 (coneEquations, List)
 [coneEquations, "Inhomogeneous"]
Headline
 Find the equations of the Kunz cones
Usage
 M =  coneEquations m
 M = coneEquations sgrp
Inputs
 m:ZZ
  multiplicity
 sgrp:List
  generators of a semigroup
Outputs
 M:Matrix
  m-1 x d matrix whose columns represent inequalities defining the Kunz cone
Description
  Text
   Let S be the numerical semigroup defined by a list sgrp,
   have multiplicity m and apery set a_1,\dots,a_(m-1) and
   set mu_i = (a_i -i)//m. The homogeneous Kunz cone of
   semigroups of multiplity m is the convex polyhedral cone
   defined by the inequalities of the form

   a_i + a_j - a_(i+j) \geq 0.

   where 1\leq i,j\leq m-1 and i+j\neq m is interpreted mod m.
   The function coneEquations m returns an m-1 x d matrix of ZZ
   whose columns are the coefficients of the left hand sides of these inequalities.
   The function coneEquations sgrp does the same, with additional columns representing
   the additional inequalities of this type that are satisfied by
   the Apery set apery(sgrp). For m = 3, the semigroup {3,4,5} is interior (and thus
   satisfies no further equations), while the semigroups {3,4} and {3,5} are on
   the two extremal rays of the cone.
  Example
   coneEquations 3
   coneEquations {3,4,5}
   coneEquations {3,4}
   coneEquations {3,5}
   allSemigroups 3
  Text
   The inhomogeneous Kunz cone does the same, but for the numbers mu_i instead of
   a_i. Thus when i+j > m the inequality mu_i+mu_j-mu_(i+j) \geq 0 is replaced by the inequality

   mu_i+mu_j - mu_(i+j) -1.

   The function coneEquations(m, "Inhomogeneous" => true) returns the same matrix
   as in the homogeneous case, with one more row, where the last row represents the
   constant terms of this inquality:
  Example
   eq=coneEquations(3, "Inhomogeneous" => true)
   coneEquations({3,4,5}, "Inhomogeneous" => true)
   coneEquations({3,4}, "Inhomogeneous" => true) 
   coneEquations({3,5}, "Inhomogeneous" => true)
   (H,M)=allSemigroups 3
   (H,M)=allSemigroups 4
   M1=(M|matrix apply(rank target M,i->{-1}))
   eqInh=coneEquations(4, "Inhomogeneous" => true)
   eqh=coneEquations(4)
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
   which is given by @TO coneEquations@. The number of rays grows rather quickly with m;
   the actual number is unknown. Use @TO facetRays@ to determine the rays bounding the face
   on which a given semigroup lies.
  Example
   coneRays 3
   coneRays 4
   facetRays {4,5,6}
SeeAlso
 coneEquations
 facetRays
///


--<<docTemplate
 -* Test section *-

 TEST/// -*fractionalIdeal and numberToMonomial*-
debug loadPackage("NumericalSemigroups", Reload => true)
   sgrp = {3,5}
   R = semigroupRing sgrp
   mons = semigroup sgrp
   assert(apply(mons, n -> numberToMonomial(R,n)) == {1, x_0, x_2, x_0^2, x_0*x_2, x_0^3, x_2^2})
   assert(
    I := fractionalIdeal(sgrp, {2,3});
    R := ring I;
    I == ideal(R_1,R_0^2)
    )
///
 
 TEST ///-*buchweitzSemigroups*-
assert(buchweitzSemigroups 6 == {})
--elapsedTime buchweitzSemigroups(13,16)
///

TEST///-*test of findPoint*-
kk=ZZ/101
R=kk[x_0..x_5]
setRandomSeed 0
c=ideal random(R^1,R^{2:-1,2:-2})
point=findPoint c
assert(sub(c,point)== 0)
///	

TEST///-*flattening and unfolding*-
assert ((L = mingens {5,6, 8,9, 10,12, 13, 17}) == {5, 6, 8, 9})
I=semigroupIdeal L
(A,unfolding)=makeUnfolding I;
assert(numgens A == 68)
J=flatteningRelations(I,A,unfolding);
numgens J
assert(numgens J == 260)
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
socle L == {84, 79, 62}
///

TEST/// -*conductor*-
assert(conductor {7, 24} == 6*23)
///

TEST///-*test of facetRays*-
assert(facetRays{3,4} == matrix"1;2")
assert(facetRays{3,5} == matrix"2;1")
assert(facetRays{3,4,5} == matrix"1,2;2,1")
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

   
TEST///-*test of coneEquations*-
    (H,M)=allSemigroups 3
    eq=coneEquations 3
    assert(all(flatten entries (H*eq),e->e>=0))
    eqInh=coneEquations(3,"Inhomogeneous" => true)
    M1=(M|matrix apply(rank target M,i->{-1}))
    assert(all(flatten entries (M1*eqInh),e->e>=0))
///
   
end--

-* Development section *-
restart
loadPackage ("NumericalSemigroups", Reload=>true)
uninstallPackage "NumericalSemigroups"
restart
installPackage "NumericalSemigroups"
check "NumericalSemigroups"
viewHelp "NumericalSemigroups"



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


