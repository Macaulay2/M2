newPackage("MultigradedBGG",
    Version => "1.2",
    Date => "11 April 2025",
    Headline => "the multigraded BGG correspondence and differential modules",
    Authors => {
	{Name => "Maya Banks",         	     Email => "mdbanks@wisc.edu",      HomePage => "https://sites.google.com/wisc.edu/mayabanks" },
        {Name => "Michael K. Brown",         Email => "mkb0096@auburn.edu",    HomePage => "http://webhome.auburn.edu/~mkb0096/" },
	{Name => "Tara Gomes",	    	     Email => "gomes072@umn.edu",      HomePage => "https://cse.umn.edu/math/tara-gomes" },
	{Name => "Prashanth Sridhar",	     Email => "pzs0094@auburn.edu",    HomePage => "https://sites.google.com/view/prashanthsridhar/home"},
	{Name => "Eduardo Torres Davila",    Email => "torre680@umn.edu",      HomePage => "https://etdavila10.github.io/" },
	{Name => "Sasha Zotine",    	     Email => "zotinea@mcmaster.ca",   HomePage => "https://sites.google.com/view/szotine/home" }
    },
    PackageExports => {"NormalToricVarieties", "Complexes"},
    Keywords => {"Commutative Algebra"}
  )

export {
    --methods
    "dualRingToric",
    "toricRR",
    "toricLL",
    "stronglyLinearStrand",
    "unfold",
    "DifferentialModule",
    "differentialModule",
    "foldComplex",
    "resDM",    
    "resMinFlag",
    "minimizeDM",
    "differential",
    --symbols
    "SkewVariable"
    }

--------------------------------------------------
--- Differential Modules
--------------------------------------------------
DifferentialModule = new Type of Complex
DifferentialModule.synonym = "differential module"

differentialModule = method(TypicalValue => DifferentialModule)
differentialModule Complex := C -> (
    if C != naiveTruncation(C, -1, 1) then error "--complex needs to be concentrated in homological degrees -1, 0, and 1";
    if C.dd_0 != C.dd_1 then error "--the two differentials of the complex need to be identical";
    if C.dd_0^2 != 0 then error "--differential must square to zero";
    new DifferentialModule from C
    );

differentialModule Matrix := phi -> (
    if phi^2 != 0 then error "The differential does not square to zero.";
    R := ring phi;
    if target phi != source phi then error "source and target of map are not the same"; 
    new DifferentialModule from (complex({-phi,-phi})[1])--the maps are negated to cancel out the sign introduced by the shift.
    );

ring(DifferentialModule) := Ring => D -> D.ring;
module DifferentialModule :=  (cacheValue symbol module)(D -> D_0);
degree DifferentialModule := List => (D -> degree D.dd_1); 
differential = method();
differential DifferentialModule := Matrix=> (D->D.dd_1);
kernel DifferentialModule := Module => opts -> (D -> kernel D.dd_0); 
image DifferentialModule := Module => (D -> image D.dd_1); 
homology DifferentialModule := Module => opts -> (D -> HH_0 D);

unfold = method();
--Input:  a differential module and a pair of integers low and high
--Output:  the unfolded chain complex of the differential module, in homological degrees
--         low through high.
unfold(DifferentialModule,ZZ,ZZ) := Complex => (D,low,high)->(
    L := toList(low..high);
    d := degree D;
    R := ring D;
    phi := differential D;
    (complex apply(L,l-> phi))[-low]
    )

minFlagOneStep = method()
minFlagOneStep(DifferentialModule) := (D) -> (
    d := degree D;
    R := ring D;
    minDegree := min degrees trim HH_0 D;
    colList := select(rank source (mingens HH_0(D)), i -> degree (mingens HH_0(D))_i == minDegree);
    minDegHom := (mingens HH_0(D))_colList;
    homMat :=  mingens image((minDegHom) % (image D.dd_1));
    G := freeResolution image homMat;
    psi := map(D_0,G_0**R^{d},homMat, Degree=>d);
    newDiff := matrix{{D.dd_1,psi},{map(G_0**R^{d},D_1,0, Degree=>d), map(G_0**R^{d},G_0**R^{d},0, Degree=>d)}};
    assert (newDiff*(newDiff) == 0);
    differentialModule newDiff
)

--Input: a differential module D with degree 0, and an integer k
--Output: the first k iterations of an algorithm that produces the minimal free flag resolution of D
resMinFlag = method();
resMinFlag(DifferentialModule, ZZ) := (D, k) -> (
    d := degree D;
    assert(first d == 0);
    R := ring D;
    s := numgens D_0;
    scan(k,i-> D = minFlagOneStep(D));
    t := numgens D_0;
    newDiff := submatrix(D.dd_1, toList(s..t-1),toList(s..t-1));
    differentialModule (complex({-newDiff**R^{-d},-newDiff**R^{-d}})[1])
)

        
killingCyclesOneStep = method();
killingCyclesOneStep(DifferentialModule) := (D)->(
    d := degree D;
    R := ring D;
    homMat := mingens image((gens HH_0 D) %  (image D.dd_1));
    G := freeResolution(image homMat, LengthLimit => 1);
    psi := map(D_0,G_0**R^{d},homMat, Degree=>d);
    newDiff := matrix{{D.dd_1,psi},{map(G_0**R^{d},D_1,0, Degree=>d),map(G_0**R^{d},G_0**R^{d},0, Degree=>d)}}; 
    assert (newDiff*(newDiff) == 0);
    differentialModule newDiff
    )

--Input:  a differential module D and an integer k
--Output: the first k iterations of an algorithm that produces a free flag resolution of D.

resDM = method();
resDM(DifferentialModule,ZZ) := (D,k)->(
    d := degree D;
    R := ring D;
    s := numgens D_0;
    scan(k,i-> D = killingCyclesOneStep(D));
    t := numgens D_0;
    newDiff := submatrix(D.dd_1, toList(s..t-1),toList(s..t-1));
    differentialModule (complex({-newDiff**R^{-d},-newDiff**R^{-d}})[1])
    )

--same as above, but default value of k 
resDM(DifferentialModule) := (D)->(
    k := dim ring D + 1;
    d := degree D;
    R := ring D;
    s := numgens D_0;
    scan(k,i-> D = killingCyclesOneStep(D));
    t := numgens D_0;
    newDiff := submatrix(D.dd_1, toList(s..t-1),toList(s..t-1));
    differentialModule (complex({-newDiff**R^{-d},-newDiff**R^{-d}})[1])
    )

--  Subroutines and routines to produce the minimal part of a matrix.
--  Input: a square matrix
--  Output: a square matrix, the result of partially minimizing the input.
minimizeDiffOnce = method();
minimizeDiffOnce(Matrix,ZZ,ZZ) := (A,u,v) -> (
    a := rank target A;
    R := ring A;
    inv := (A_(u,v))^(-1);
    N := map(source A,source A, (i,j) -> if i == v and j != v then -inv*A_(u,j) else 0) + id_(R^a);
    Q := map(target A,target A, (i,j) -> if j == u and i != u then -inv*A_(i,v) else 0) + id_(R^a);
    A' := Q*N^(-1)*A*N*Q^(-1);
    newRows := select(a,i-> i != u and i != v);
    newCols := select(a,i-> i != u and i != v);
    A'_(newCols)^(newRows)
    )

units = method();
units(Matrix) := A->(
    a := rank source A;
    L := select((0,0)..(a-1,a-1), (i,j)->  isUnit A_(i,j));
    L
    )

--  Input: A square matrix
--  Output: a minimization of it.
minimizeDiff = method();
minimizeDiff(Matrix) := A ->(
    NU := #(units A);
    while NU > 0 do(
 	L := units A;
	(u,v) := L_0;
	A = minimizeDiffOnce(A,u,v);
	NU = #(units A);
	);
    A
    )

--  Input:  A free differential module
--  Output: A minimization of that DM.
minimizeDM = method();
minimizeDM(DifferentialModule) := r ->(
    R := ring r;
    d := degree r;
    A := minimizeDiff(r.dd_1);
    degA := map(target A, source A, A, Degree=>d);
    differentialModule (complex({-degA,-degA})[1])
    )

---
---

--  Input:  a free complex F and a degree d
--  Output: the corresponding free differential module of degree d
foldComplex = method();
foldComplex(Complex,ZZ) := DifferentialModule => (F,d)->(
    R := ring F;
    FDiff := directSum apply(min F .. max F + 1, i -> F.dd_i);
    FMod := directSum apply(min F .. max F, i -> F_i ** R^{i*d});
    degFDiff := map(FMod,FMod,FDiff, Degree=>d); 
    differentialModule(complex({-degFDiff,-degFDiff})[1]) 
    )

--------------------------------------------------
--- Toric BGG
--------------------------------------------------

-- Exported method for dualizing an algebra.
-- Input:   a polynomial ring or exterior algebra S
-- Output:  the Koszul dual algebra. When S is a polynomial ring graded by a group A,
--          the Koszul dual exterior algebra has an A x Z grading, where
--          deg(e_i) = (-deg(x_i), -1). When S is an exterior algebra graded by A x Z, 
--          the Koszul dual polynomial ring has an A grading where deg(x_i) = -\pi_A(deg(e_i)),
--          where pi_A : A x Z --> A is the natural surjection.
dualRingToric = method(Options => {
    	    Variable        => getSymbol "x",
	    SkewVariable => getSymbol "e"});
dualRingToric PolynomialRing := opts -> S ->(
    kk := coefficientRing S;
    degs := null;
    if isSkewCommutative S == false then(
    	degs = apply(degrees S,d-> (-d)|{-1});
	e := opts.SkewVariable;
    	ee := apply(#gens S, i-> e_i);
    	return kk[ee,Degrees=>degs,SkewCommutative=>true, MonomialOrder => Lex]
	); 
    if isSkewCommutative S == true then(
    	degs = apply(degrees S, d-> drop(-d,-1));
	y := opts.Variable;
    	yy := apply(#gens S, i-> y_i);
    	return kk[yy,Degrees=>degs,SkewCommutative=>false]
	);       
    )

-- Non-exported method for contracting a matrix by another.
matrixContract = method()
matrixContract (Matrix,Matrix) := (M,N) -> (
    S := ring M;
    if M==0 or N==0 then return map(S^(-degrees source N),S^(degrees source M),0);
    assert(rank source M == rank target N);
    mapmatrix := transpose matrix apply(rank target M, i-> 
	apply(rank source N, j->		
           sum(rank source M, k -> contract(M_(i,k),N_(k,j)))
	   )
       );
    -- this null input is intentional. 
    -- it chooses the source to make the map homogeneous. 
    transpose map(S^(-degrees source N), , mapmatrix)
    )


-- Non-exported method for redefining a degree list which makes the quotient
-- operation in toricRR well-defined
pickOutMonomials = method();
pickOutMonomials (Ring, List) := (S, L) -> (
    h := matrix {heft S};
    D := max for l in L list (h * vector l)_0;
    S' := (coefficientRing S)(monoid[gens S, Degrees => toList (#gens S:1)]);
    fromS'toS := map(S,S', gens S);
    i := -1;
    stopflag := true;
    newL := flatten while(stopflag and i < 15) list (
	i = i + 1;
	for m in first entries basis(i, S') list (
	    deg := degree fromS'toS m;
	    if (h * vector deg)_0 <= D then deg else continue
	    )
	);
    unique flatten for a in L list (
	for b in newL list (
	    if member(a - b,newL) then a-b else continue
	    )
	)
    )

-- Exported method for computing the multigraded BGG functor of a module over
-- a polynomial ring.
-- Input:   M a finitely generated (multi)-graded S-module.
--          L a list of degrees.
-- Output:  The differential module RR(M) in degrees from L.
toricRR = method();
toricRR(Module,List) := (N,L) ->(
    M := coker presentation N;
    S := ring M;
    grade := #(degrees S)_0;
    if L === {} then error "--expected non-empty list of degrees";
    if any(L, l -> #l != grade) then error("--expected each multidegree to be of length " | grade); 
    if not isCommutative S then error "--base ring is not commutative";
    if heft S === null then error "--need a heft vector for polynomial ring";
    -- we need to modify L so that the "quotient" differential is well-defined
    L = pickOutMonomials(S,L);
    E := dualRingToric S;
    -- pick out the generators in the selected degree range.
    f0 := matrix {for d in unique L list gens image basis(d,M)};
    -- this is the degree of the anti-canonical bundle, which
    -- we need to twist by when we take the koszul dual.
    wEtwist := append(-sum degrees S, -numgens S);
    -- here are the degrees of the generators we will have in the dual.
    f0degs := apply(degrees source f0, d -> (-d | {0}) + wEtwist);
    if #f0degs == 0 then complex map(E^0, E^0, 0) else (	
    	relationsM := presentation M;
    	SE := S**E;
    	tr := sum(dim S, i-> SE_(dim S+i)*SE_i);
    	newf0 := sub(f0,SE)*tr;
    	relationsMinSE := sub(relationsM,SE);
    	newf0 = newf0 % relationsMinSE;
    	newg := matrixContract(transpose sub(f0,SE), newf0);
	-- null input is to make the map have the correct degrees to be homogeneous.
	newg = transpose map(source newg, , transpose newg);
    	g' := sub(newg,E);
	del := map(E^f0degs,E^f0degs, -g', Degree => degree 1_S | {-1});
	differentialModule(complex({del, del})[1])
    	)
    )

-- If there is no input of L, we make a simple choice based on some information on M.
toricRR Module := M -> (
    L := if length M < infinity then unique flatten degrees basis M
    else join(degrees M, apply(degrees ring M, d -> (degrees M)_0 + d));
    toricRR(M,L)
    )

-- Exported method for computing the multigraded BGG functor of a module over the exterior algebra.
-- Input: N a finitely generated (multi)-graded E-module.
-- Output: a complex of modules over the polynomial ring.
toricLL = method();
toricLL Module := N -> (
    E := ring N;
    if not isSkewCommutative E then error "ring N is not skew commutative";
    S := dualRingToric E;
    N = coker presentation N;
    bb := basis N;
    b := degrees source bb;
    homDegs := sort unique apply(b, i-> last i);
    inds := new HashTable from apply(homDegs, i-> i=> select(#b, j-> last(b#j) == i));
    sBasis := new HashTable from apply(homDegs, i-> i => bb_(inds#i));
    FF := new HashTable from apply(homDegs, i ->(
	    i => S^(apply((degrees sBasis#i)_1, j-> -drop(j,-1)))
	    )
	);
    EtoS := map(S,E,toList (numgens S:0));
    differential := sum for i to numgens E-1 list (
        S_i* EtoS matrix basis(-infinity, infinity, map(N,N,E_i))
	);
    if #homDegs == 1 then (complex {map(S^0,FF#0,0)})[1] else (
	complex apply(drop(homDegs,-1), i-> map(FF#i,FF#(i+1), (-1)^(homDegs#0)*differential_(inds#(i+1))^(inds#(i))))[-homDegs#0]
	)
    )

-- Exported method for computing a strongly linear strand for a module over a polynomial ring.
-- Input: M a (multi)-graded module over a polynomial ring
-- Output: a Complex, the strongly linear strand of the minimal
--     	   free resolution of M (in the sense of the paper "Linear strands  
--    	   of multigraded free resolutions" by Brown-Erman).
stronglyLinearStrand = method();
stronglyLinearStrand Module := M -> (
    S := ring M;
    h := heft S;
    if h === null then error("--ring M does not have heft vector");
    if not same degrees M then error("--M needs to be generated in a single degree");
    degM := first degrees M;
    canonical := sum flatten degrees vars S;
    degrange := unique prepend(degM, apply(degrees S, d -> d + degM));
    RM := toricRR(M,degrange);
    mat := RM.dd_0;
    cols := positions(degrees source mat, x -> drop(x,-1) == degM + canonical);
    N := ker mat_cols;
    toricLL N
)


beginDocumentation()

undocumented {
    SkewVariable    
    }

--------------------------------------------------
--- Differential Modules
--------------------------------------------------

doc ///
   Key 
      MultigradedBGG
   Headline 
      Package for working with Multigraded BGG and Differential Modules
   Description
      Text
         This package implements the multigraded BGG correspondence, as
	 described, for instance, in Section 2.2 of the paper "Tate resolutions
	 on toric varieties" by Brown-Erman. Applying the BGG functor to 
	 a module over a multigraded polynomial ring gives a differential
	 E-module, rather than a complex of E-modules; this package therefore
	 also implements differential modules. Highlights of the package include
	 methods for building free resolutions of differential modules;
	 implementations of the multigraded BGG functors; and a method for
	 computing the strongly linear strand of the minimal free resolution of a
	 module over a multigraded polynomial ring, in the sense of the paper
	 "Linear strands of multigraded free resolutions" by Brown-Erman.
   SeeAlso
      DifferentialModule
      resDM
      toricRR
      toricLL
      stronglyLinearStrand
   References
       Text
       [1] @HREF{"https://arxiv.org/pdf/2202.00402v4","Linear strands of multigraded free resolutions"}@ (with Daniel Erman), Mathematische Annalen 390 (2024), 2707–2725
       [2] @HREF{"https://arxiv.org/pdf/2108.03345v3","Tate resolutions on toric varieties"}@ (with Daniel Erman), Journal of the European Mathematical Society, published online (2024)

///

doc ///
   Key 
      differential
      (differential, DifferentialModule)
   Headline 
      returns the differential of a differential module
   Usage
      differential D
   Inputs
      D : DifferentialModule
   Outputs
        : Matrix
   Description
      Text
         This method returns the differential of a differential 
	 module.
      Example
         R = QQ[x]/(x^3)
         phi = map(R^1, R^1, x^2, Degree=>2)
         D = differentialModule phi
	 differential D
   SeeAlso
      (degree, DifferentialModule)
      (image, DifferentialModule)
      (kernel, DifferentialModule)
      (homology, DifferentialModule)
       
///

doc ///
   Key 
      (degree, DifferentialModule)
   Headline 
      returns the degree of the differential
   Usage
      degree D
   Inputs
      D : DifferentialModule
   Outputs
        : List
   Description
      Text
         This method returns the degree of the differential of a differential 
	 module. In more detail: since the source and target of the differential
         are required to be equal, we must specify the degree of 
	 the differential in order for the differential to 
	 be homogeneous; this method returns that degree.
      Example
         R = QQ[x]/(x^3)
         phi = map(R^1, R^1, x^2, Degree=>2)
         D = differentialModule phi
	 degree D == {2}
   SeeAlso
      (differential, DifferentialModule)
///

doc ///
   Key 
      (homology, DifferentialModule)
   Headline 
      computes the homology of a differential module
   Usage
      HH D
   Inputs
      D : DifferentialModule
   Outputs
        : Module
   Description
      Text
         This computes the homology of a differential module. More specifically:
	 since we interpret differential modules as 3-term complexes, this
	 returns the zeroth homology module.
      Example
         R = QQ[x,y]
      	 M = R^1/ideal(x^2,y^2)
      	 phi = map(M,M,x*y)
      	 D = differentialModule phi
	 HH D
   SeeAlso
      (image, DifferentialModule)
      (kernel, DifferentialModule)
///

doc ///
   Key 
      (image, DifferentialModule)
   Headline 
      computes the image of the differential of a differential module
   Usage
      image D
   Inputs
      D : DifferentialModule
   Outputs
        : Module
   Description
      Text
         This method computes the image of the differential of $D$, as a
	 submodule of $D$.
      Example
         R = QQ[x,y]
      	 M = R^1/ideal(x^2,y^2)
      	 phi = map(M,M,x*y, Degree => 2)
      	 D = differentialModule phi
	 D' = image D
   SeeAlso
      (differential, DifferentialModule)
      (image, DifferentialModule)
      (kernel, DifferentialModule)
///

doc ///
   Key 
      (kernel, DifferentialModule)
   Headline 
      computes the kernel of the differential in a differential module.
   Description
      Text
         Computes the kernel of the differential in a differential module.
   SeeAlso
      (differential, DifferentialModule)
      (image, DifferentialModule)
      (homology, DifferentialModule)
///

doc ///
   Key 
      (module, DifferentialModule)
   Headline 
      computes the underlying module of a differential module.
   Description
      Text
         Returns the underlying module of a differential module.
   SeeAlso
      (ring, DifferentialModule)
      (image, DifferentialModule)
      (differential, DifferentialModule)
///

doc ///
   Key 
      (ring, DifferentialModule)
   Headline 
      returns the ring of a differential module.
   Description
      Text
         Returns the ring of a differential module. 
   SeeAlso
      (module, DifferentialModule)
      (differential, DifferentialModule)
///


doc ///
   Key 
      differentialModule
      (differentialModule, Matrix)
   Headline
      converts a square zero matrix into a differential module
   Usage
      differentialModule(f)
   Inputs
      f : Matrix
          representing a module map with the same source and target
   Outputs
      : DifferentialModule 
   Description
      Text
         Given a module map $f: M \rightarrow M$ of degree $a$ this creates a degree $a$ 
         differential module from $f$ represented as a 3-term chain complex in homological 
         degrees $-1, 0$, and $1$. If $a \neq 0$, then since the source and target of $f$
         are required to be equal, we must specify the degree of the differential to be $a$ 
         in order for the differential to be homogeneous.
      Example
         R = QQ[x]
         phi = map(R^1/(x^2),R^1/(x^2), x, Degree=>1)
         differentialModule(phi)
   SeeAlso
      (differentialModule, Complex)
      (differential, DifferentialModule)
///

doc ///
   Key 
      (differentialModule, Complex)
   Headline
      converts a complex into a differential module
   Usage
      differentialModule C
   Inputs
      C : Complex
          a complex concentrated in homological degrees -1, 0, 1
   Outputs
      : DifferentialModule 
   Description
      Text
         Given a complex of modules in homological degrees $-1, 0$, and $1$, with
         the differentials being identical, this method produces the corresponding
         DifferentialModule.
      Example
         S = QQ[x,y]
         del = map(S^{-1,0,0,1},S^{-1,0,0,1},matrix{{0,y,x,-1},{0,0,0,x},{0,0,0,-y},{0,0,0,0}}, Degree=>2)
         C = complex{-del, -del}[1]
         D = differentialModule C
         D.dd
   SeeAlso
      (differentialModule, Complex)
      (differential, DifferentialModule)
///


doc ///
   Key 
      unfold
      (unfold,DifferentialModule,ZZ,ZZ)
   Headline
      converts a differential module into a 1-periodic complex
   Usage
      unfold(D,a,b)
   Inputs
      D : DifferentialModule
      a : ZZ
      b : ZZ
   Outputs
      : Complex
   Description
      Text
         Given a differential module D and integers a and b, this method produces a
         chain complex with the module D in homological degrees a through b, where
         all maps are the differential of D.
      Example
         phi = matrix{{0,1},{0,0}};
         D = differentialModule(phi);
         unfold(D,-3,4)
   SeeAlso
      (differentialModule, Complex)
      resMinFlag
      resDM
      foldComplex
///

doc ///
   Key 
      resMinFlag
      (resMinFlag, DifferentialModule, ZZ)
   Headline
      gives a minimal free flag resolution of a differential module of degree 0 
   Usage
      resMinFlag(D, k)
   Inputs
      D: DifferentialModule
      k: ZZ
   Outputs
      : DifferentialModule
   Description
      Text
         Let $R$ be a positively graded ring with $R_0$ a field. Given a differential module $D$ of degree 0 
	 with finitely generated homology, this method gives a portion of the minimal free flag resolution of 
	 $D$, using Algorithm 2.11 from the accompanying paper for this package, "The multigraded BGG correspondence
	 in Macaulay2". As this resolution will often be infinite, the integer $k$ indicates how many steps of
	 this algorithm will be applied. 
      Example
         R = ZZ/101[x, y];
         k = coker vars R;
	 f = map(k, k, 0);
	 D = differentialModule(f);
	 F = resMinFlag(D, 3);
	 F.dd_0
   SeeAlso
      (differentialModule, Complex)
      unfold
      resDM
      foldComplex
      minimizeDM
///

doc ///
   Key 
      foldComplex
      (foldComplex,Complex,ZZ)
   Headline
      converts a chain complex into a differential module
   Usage
      foldComplex(C)
   Inputs
      C : Complex
      d : ZZ
   Outputs
      : DifferentialModule
   Description
      Text
         Given a chain complex C and integer d it creates the corresponding
         (flag) differential module of degree d.
      Example
         R = QQ[x,y];
         C = freeResolution ideal(x,y)
         D = foldComplex(C,0);
         D.dd_1
   SeeAlso
      (differentialModule, Complex)
      unfold
      resDM
      resMinFlag
///


doc ///
   Key 
      resDM
      (resDM,DifferentialModule)
      (resDM,DifferentialModule,ZZ)
   Headline
      uses a "killing cycles"-style construction to find a free resolution of a differential module
   Usage
      resDM(D)
      resDM(D,k)
   Inputs
      D : DifferentialModule
      k : ZZ
   Outputs
      : DifferentialModule
   Description
      Text
         Given a differential module D, this method creates a free flag resolution of D, using Algorithm 2.7 from the accompanying 
	 paper "The multigraded BGG correspondence in Macaulay2". The default resDM(D) runs the algorithm for the 
	 number of steps determined by the dimension of the ambient ring. Alternatively, resDM(D,k) iterates k steps 
	 of the algorithm.
      Example
         R = QQ[x,y];
         M = R^1/ideal(x^2,y^2);
         phi = map(M,M,x*y, Degree=>2);
         D = differentialModule phi;
         r = resDM(D)
         r.dd_1
      Text
         The default algorithm runs for dim R + 1 steps.
         Adding the number of steps as a second argument is like adding a LengthLimit.
      Example
         R = QQ[x]/(x^3);
         phi = map(R^1,R^1,x^2, Degree=>2);
         D = differentialModule phi;
         r = resDM(D)
         r.dd_1      
         r = resDM(D,6)
         r.dd_1
   SeeAlso
      (differentialModule, Complex)
      resMinFlag
      unfold
      foldComplex
      minimizeDM
///


doc ///
   Key 
      DifferentialModule
   Headline
      The class of differential modules.
   Description
      Text
         A differential module is just a module with a square zero endomorphism. 
         Given a module map $f: M \rightarrow M$ of degree $a$, we represent a 
         differential module from $f$ as a 3-term chain complex in homological 
         degrees $-1, 0$, and $1$. If $a \neq 0$, then since the source and target of $f$
         are required to be equal, we must specify the degree of the differential to be $a$ 
         in order for the differential to be homogeneous.
   SeeAlso
      MultigradedBGG
      differentialModule
      (differentialModule, Matrix)
      (differentialModule, Complex)
///


doc ///
   Key 
      minimizeDM
      (minimizeDM,DifferentialModule)
   Headline
      minimizes a square matrix or a differential module
   Usage
      minimizeDM(D)
   Inputs
      D : DifferentialModule
   Outputs
      : DifferentialModule
   Description
      Text
         Given a differential module D, this code breaks off trivial
         blocks, producing a quasi-isomorphic differential module D' with a minimal
         differential.
      Example
         R = QQ[x,y];
         M = R^1/ideal(x^2,y^2);
         phi = map(M,M,x*y, Degree=>2);
         D = differentialModule phi;
         r = resDM(D)
         r.dd_1
         mr = minimizeDM(r)
         mr.dd_1
   SeeAlso
      (differentialModule, Complex)
      resMinFlag
      unfold
      foldComplex
      resDM
///

--------------------------------------------------
--- Toric BGG
--------------------------------------------------

doc ///
   Key 
      dualRingToric
      (dualRingToric, PolynomialRing)
      [dualRingToric, Variable]
      [dualRingToric, SkewVariable]
   Headline
      computes the Koszul dual of a multigraded polynomial ring or exterior algebra
   Usage
      dualRingToric R
   Inputs
      R : PolynomialRing
          either a standard polynomial ring, or an exterior algebra
      Variable => Symbol
      SkewVariable => Symbol
   Outputs
      : PolynomialRing
        which is the Koszul dual of R
   Description
      Text
         This method computes the Koszul dual of a polynomial ring or exterior algebra. In
         particular, if $S = k[x_0, \ldots, x_n]$ is a $\mathbb{Z}^m$-graded ring for some $m$, 
         and $\operatorname{deg}(x_i) = d_i$, then the output of dualRingToric is the 
         $\mathbb{Z}^{m+1}$-graded exterior algebra on variables $e_0, \ldots, e_n$ with degrees 
         $(-d_i, -1)$.
      Example
         R = ring(hirzebruchSurface(2, Variable => y))
         E = dualRingToric(R, SkewVariable => f)
      Text
         On the other hand, if $E$ is a $\mathbb{Z}^{m+1}$-graded exterior algebra on $n+1$ 
         variables $e_0, \ldots, e_n$ with $\operatorname{deg}(e_i) = (-d_i, -1)$, then 
         dualRingToric E is the $\mathbb{Z}^m$-graded polynomial ring $k[x_0, \ldots, x_n]$ with 
         $\operatorname{deg}(x_i) = d_i$.
      Example
         RY = dualRingToric E
         degrees RY == degrees R
      Text
         This method preserves the coefficient ring of the input ring.
      Example
         S = ZZ/101[x,y,z, Degrees => {1,1,2}]
         E' = dualRingToric S
         coefficientRing E' === coefficientRing S
   SeeAlso
      toricRR
      toricLL
      stronglyLinearStrand
///

doc ///
   Key 
      toricRR
      (toricRR, Module)
      (toricRR, Module, List)
   Headline
      computes the BGG functor of a module over a multigraded polynomial ring. 
   Usage
      toricRR M
      toricRR(M,L)
   Inputs
      M : Module
          a module over a multigraded polynomial ring
      L : List 
          a list of multidegrees of the polynomial ring
   Outputs
      : DifferentialModule
        a quotient of the differential module obtained by applying the multigraded BGG functor R 
        to M. The size of this quotient is determined by the list L. 
   Description
      Text
         Let $A$ be a finitely generated free abelian group. Given an $A$-graded polynomial ring $S$ 
         with $A \oplus \mathbb{Z}$-graded Koszul dual exterior algebra E, the BGG functor
       	 $\mathbf{R}$ sends an $S$-module $M$ to a free differential $E$-module with linear 
       	 differential: see Section 3 of the paper accompanying this package for details. 
       	 The free $E$-module underlying $\mathbf{R}(M)$ is $\bigoplus_{d \in A} M_d \otimes_k E^*(-d, 0)$,
       	 where $E^*$ denotes the dual of $E$ over the ground field $k$. This module has rank given by the
       	 dimension of $M$ as a $k$-vector space, which is typically infinite. Thus, this method usually only 
       	 computes a finite rank quotient of $\mathbf{R}(M)$. Specifically: toricRR(M) is the quotient
       	 of $\mathbf{R}(M)$ given by those summands $M_d \otimes_k E^*(-d, 0)$ such that
       	 $d = e + a \operatorname{deg}(x_i)$, where $e$ is a generating degree of $M$, $a \in \{0,1\}$, 
       	 and $0 \le i \le n$.      
      Example
         S = ring weightedProjectiveSpace {1,1,2}
       	 M = coker random(S^2, S^{3:{-5}});
       	 toricRR M
       	 T = ring hirzebruchSurface 3;
       	 M' = coker matrix{{x_0}}
       	 toricRR M'
      Text
         There is also an optional input for a list L of degrees in $A$: toricRR(M, L) is the quotient
	 $\bigoplus_{a \in L} M_d \otimes_k E^*(-d, 0)$ of $\mathbf{R}(M)$.
      Example
         L = {{0,0}, {1,0}}
	 toricRR(M',L)
      Text
         For some lists of degrees L, the denominator of the quotient is not a differential module. Hence,
	 the method sometimes enlarges the list of degrees in order to have a well-defined quotient. We
	 demonstrate that phenomenon with the following example.
      Example
         S = ring hirzebruchSurface 1
	 L1 = {{0,0}, {1,0}, {0,1}}
	 L2 = {{0,0}, {1,0}, {0,1}, {-1,1}};
	 N1 = toricRR(S^1, L1);
	 N2 = toricRR(S^1, L2);
	 phi = map(ring N2, ring N1, gens ring N2)
	 assert(phi N1.dd_0 - N2.dd_0 == 0)
      Text
	 One can compute sheaf cohomology over weighted projective spaces using the multigraded BGG correspondence,
	 A detailed explanation can be found in Example 3.3 of the paper accompanying this package. 
      Example
         X = weightedProjectiveSpace {1,1,2};
	 S = ring X;
	 M = coker matrix{{x_0, x_1}};
	 D = toricRR(M, for i from 0 to 4 list {i})
	 F = resDM(D, 3)
	 F.dd_0
	 kk = coker vars ring F_0;
	 sum flatten entries basis({-2,-1}, Hom(kk, F_0)) == rank HH^0(X, sheaf(M**S^{{-2}}))
   Caveat
       A heft vector is necessary for the computation to produce a well-defined differential module.
   SeeAlso
       dualRingToric
       toricLL
       stronglyLinearStrand
///

doc ///
   Key 
      toricLL
      (toricLL, Module)
   Headline
      computes the BGG functor of a module over the Koszul dual exterior algebra of a multigraded polynomial ring
   Usage
      toricLL N
   Inputs
      N : Module
          a module over the Koszul dual exterior algebra of a multigraded polynomial ring
   Outputs
      : Complex
        a complex of modules over a multigraded polynomial ring, the result of applying the 
        BGG functor L to N
   Description
      Text
         Given a multigraded polynomial ring $S$ with Koszul dual exterior algebra E, the BGG functor
      	 $\mathbf{L}$ sends an $E$-module to a linear complex of $S$-modules. 
      Example
         S = ring hirzebruchSurface 3
      	 E = dualRingToric S
      	 C = toricLL(coker matrix{{e_0, e_1}})
   SeeAlso
      dualRingToric
      toricRR
      stronglyLinearStrand
///

doc ///
   Key 
      stronglyLinearStrand
      (stronglyLinearStrand, Module)
   Headline
      computes the strongly linear strand of the minimal free resolution of a finitely generated graded module over a multigraded polynomial ring, provided the module is generated in a single degree.
   Usage
      stronglyLinearStrand M
   Inputs
      M : Module
          a finitely generated graded module over a multigraded polynomial ring that is generated 
	  in a single degree
   Outputs
      : Complex
        the strongly linear strand of the minimal free resolution of M
   Description
      Text
         The strongly linear strand of the minimal free resolution of a multigraded module $M$ is defined 
      	 in the paper "Linear strands of multigraded free resolutions" by Brown-Erman. It is, roughly
      	 speaking,the largest subcomplex of the minimal free resolution of $M$ that is linear, in the 
      	 sense that its differentials are matrices of linear forms. The method we use for computing 
      	 the strongly linear strand uses BGG, mirroring Corollary 7.11 in Eisenbud's textbook
      	 "The geometry of syzygies".
      Example
         S = ZZ/101[x_0, x_1, x_2, Degrees => {1,1,2}]
      	 M = coker matrix{{x_0, x_2 - x_1^2}}
      	 L = stronglyLinearStrand(M)
      	 L == koszulComplex {x_0}
   SeeAlso
      toricRR
      toricLL
      dualRingToric
///




--- TESTS

--------------------------------------------------
--- Differential Modules
--------------------------------------------------

-- Constructor tests
TEST /// 
    S = QQ[x,y]
    m = matrix{{0,x,y,1},{0,0,0,-y},{0,0,0,x},{0,0,0,0}}
    phi = map(S^{0,1,1,2}, S^{0,1,1,2} ,m, Degree=>2)
    D = differentialModule phi
    assert(D.dd_0^2==0)
    assert(isHomogeneous D.dd_0)
    assert(degree D=={2})
    assert(prune homology D==cokernel matrix{{x,y}})
///

TEST ///
S = QQ[x,y]
del = map(S^{-1,0,0,1},S^{-1,0,0,1},matrix{{0,y,x,-1},{0,0,0,x},{0,0,0,-y},{0,0,0,0}}, Degree=>2)
C = complex{-del, -del}[1]
D = differentialModule C
assert(degree D == {2})
assert(isHomogeneous D.dd_0)
assert(D.dd_0^2 == 0)
assert(del == D.dd_0)
///

TEST ///
    S = QQ[x,y]
    m = matrix{{0,x^2,x*y,1},{0,0,0,-y},{0,0,0,x},{0,0,0,0}}
    phi = map(S^{0,1,1,3}, S^{0,1,1,3} ,m, Degree=>3)
    D = differentialModule phi
    assert(D.dd_0^2==0)
    assert(isHomogeneous D.dd_0)
    assert(degree D=={3})
    assert(prune homology D==cokernel matrix{{x*y,x^2}})
///

-- Testing minimizeDM
TEST /// 
    S = QQ[x,y]
    m = matrix{{0,x,y,1},{0,0,0,-y},{0,0,0,x},{0,0,0,0}}
    phi = map(S^{0,1,1,2}, S^{0,1,1,2} ,m, Degree=>2)
    D = differentialModule phi
    M = minimizeDM D
    assert(M.dd_1^2==0)
    assert(isHomogeneous M.dd_0)
    assert(degrees M_0=={{-1},{-1}})
    assert(degree M=={2})
///

TEST ///
    S = QQ[x,y]
    m = matrix{{0,x^2,x*y,1},{0,0,0,-y},{0,0,0,x},{0,0,0,0}}
    phi = map(S^{0,1,1,3}, S^{0,1,1,3} ,m, Degree=>3)
    D = differentialModule phi
    M = minimizeDM D
    delM = map(S^{1,1},S^{1,1},matrix{{x^2*y,x*y^2},{-x^3,-x^2*y}},Degree=>3)
    assert(differential M==delM)
///

-- Testing resDM
TEST ///
    S = QQ[x,y]
    m = matrix{{x*y,y^2},{-x^2,-x*y}}
    phi = map(S^2, S^2, m, Degree=>2)
    D = differentialModule phi
    F = resDM D
    del = map(S^{-1,0,0,1},S^{-1,0,0,1},matrix{{0,y,x,-1},{0,0,0,x},{0,0,0,-y},{0,0,0,0}}, Degree=>2)
    assert(F.dd_0^2==0)
    assert(isHomogeneous F.dd_0)
    assert(degree F=={2})
    assert(differential F==del)
///

TEST ///
    S = QQ[x,y,z]
    phi = map(S^4, S^4, matrix{{x*y,y^2,z,0},{-x^2,-x*y,0,z},{0,0,-x*y,-y^2},{0,0,x^2,x*y}})
    D = differentialModule phi
    F = resDM D    
    assert(F.dd_0^2==0)
///

--Testing resMinFlag
TEST ///
    R = ZZ/101[x, y];
    k = coker vars R;
    f = map(k, k, 0);
    D = differentialModule(f);
    F = resMinFlag(D, 3);
    K = foldComplex(koszulComplex vars R, 0)
    d1 = mutableMatrix F.dd_0
    d2 = mutableMatrix K.dd_0
    assert(d1 == columnSwap(rowSwap(d2, 1, 2), 1, 2))
///

-- Testing foldComplex
TEST ///
    S = QQ[x,y,z]
    K = koszulComplex vars S
    F0 = foldComplex(K,0)
    F1 = foldComplex(K,1)
    F4 = foldComplex(K,4)
    assert(isHomogeneous differential F1)
    assert(degree F0=={0})
    assert(degree F1=={1})
    assert(degree F4=={4})
    assert(F4.dd_0^2==0)
///

-- Testing unfold
TEST ///
    S = QQ[x,y]
    phi = map(S^{1,1},S^{1,1},matrix{{x^2*y,x*y^2},{-x^3,-x^2*y}},Degree=>3)
    D = differentialModule phi
    C = unfold(D,-2,2)
    assert(C.dd_0==D.dd_0)
    assert(C.dd_1==C.dd_0)
    assert(degree C.dd_0=={3})
    assert(C_-2==C_3)
///

--------------------------------------------------
--- Toric BGG
--------------------------------------------------

-- Testing dualRingToric
TEST ///
S = ring(hirzebruchSurface(2, Variable => y));
E = dualRingToric(S,SkewVariable => f);
SY = dualRingToric(E);
assert(degrees SY == degrees S)
///

-- Testing toricRR
TEST ///
S = ring hirzebruchSurface 3;
M = coker matrix{{x_0}};
L = {{0,0}, {1,0}};
D = toricRR(M, L);
assert(degree D == {0,0,-1})
E = ring D;
f = map(E^{{1, -2, -4}} ++ E^{{0, -2, -4}}, E^{{1, -2, -4}} ++ E^{{0, -2, -4}}, matrix{{0, 0}, {e_2, 0}});
assert(D.dd_0 == f)
L = {{0,0}, {1,0}, {-3, 1}, {0,1}, {2,0}};
D = toricRR(M,L);
assert(degree D == {0,0,-1})
assert(D.dd^2 == 0)
--this takes several seconds, so we don't include it
--M = coker random(S^2, S^{3:{-3,-2}});
--D = toricRR M
--assert(degree D == {0,0,-1})
--assert(D.dd^2 == 0)
///

TEST ///
S = ring weightedProjectiveSpace {1,1,2}
M = coker random(S^2, S^{3:{-5}});
D = toricRR M
assert(degree D == {0,-1})
assert(D.dd^2 == 0)
///

--Testing toricLL
TEST ///
S = ring hirzebruchSurface 3;
E = dualRingToric S;
C = toricLL(E^1)
assert (isHomogeneous C)
assert ((C.dd)^2 == 0)
--The following 5 assertions check that C is isomorphic to the 
--Koszul complex on x_0, ..., x_3 (up to a twist and shift)
assert (HH_(0) C == 0)
assert (HH_(-1) C == 0)
assert (HH_(-2) C == 0)
assert (HH_(-3) C == 0)
assert (minors(1, C.dd_(-3)) == ideal vars ring C)
N = coker vars E
C = toricLL(N)
assert(rank C_0 == 1)
assert(C_-1 == 0)
assert(C_1 == 0)
///

TEST ///
S = ring hirzebruchSurface 3;
E = dualRingToric S;
-- cyclic but non-free module:
C = toricLL(coker matrix{{e_0, e_1}})--this should be isomorphic (a shift of) the Koszul complex on x_2, x_3
assert(isHomogeneous C)
assert(C.dd_(-1) == map((ring C)^{{1, 1}}, (ring C)^{{1, 0}} ++ (ring C)^{{0, 1}}, matrix{{x_3, -x_2}}))
assert(HH_(-1) C == 0)
assert(HH_(0) C == 0)
assert(C_(-3) == 0)
assert(C_(1) == 0)
///

TEST ///
E = dualRingToric (ZZ/101[x_0, x_1, Degrees => {1, 2}]);
--non-cyclic module
N = module ideal(e_0, e_1);
C = toricLL(N);
S = ring C;
f = map(S^{{3}}, S^{{1}} ++ S^{{2}}, matrix{{x_1, -x_0}});
C' = complex({f})[2];
assert(C == C')
///

--Testing stronglyLinearStrand 
TEST ///
S = ring weightedProjectiveSpace {1,1,1,2,2};
I = minors(2, matrix{{x_0, x_1, x_2^2, x_3}, {x_1, x_2, x_3, x_4}});
M = Ext^3(module S/I, S^{{-7}});
L = stronglyLinearStrand M;
loadPackage "TateOnProducts"
M = coker(L.dd_1)
S = ring M
A = coker map(S^{-1} ++ S^{-1} ++ S^{-1}, S^{-2} ++ S^{-2} ++ S^{-2} ++ S^{-2} ++ S^{-3} ++ S^{-3}, matrix{{x_0, 0, x_1, 0, x_3, 0}, {0,x_0,-x_2,x_1,-x_4,x_3}, {-x_2,-x_1,0,-x_2,0,-x_4}})
assert(isIsomorphic(M, A))
assert(HH_2 L == 0)
assert(HH_1 L == 0)
--Aside: M is the canonical module of the coordinate ring of a copy of P^1 embedded in
--the weighted projective space P(1,1,1,2,2). 
///

TEST ///
S = ring hirzebruchSurface 3;
M = coker vars S;
L = stronglyLinearStrand(M)--should give the Koszul complex, and the following 5 assertions check that it does.
assert(numcols basis HH_0 L == 1)
assert(HH_1 L == 0)
assert(HH_2 L == 0)
assert(HH_3 L == 0)
assert(HH_4 L == 0)
M = coker matrix{{x_1, x_2^2}}
L = stronglyLinearStrand(M)
assert(L == koszulComplex {x_1})
///

TEST ///
  S = QQ[x,y]
  K = koszulComplex vars S
  assert(differential foldComplex(K, 0) == map(S^{{0}, 2:{-1}, {-2}},S^{{0}, 2:{-1}, {-2}},{{0, x, y, 0}, {0, 0, 0, -y}, {0, 0, 0, x}, {0, 0, 0, 0}}))
  assert(differential foldComplex(dual K, 0) == map(S^{{2}, 2:{1}, {0}},S^{{2}, 2:{1}, {0}},{{0, -y, x, 0}, {0, 0, 0, x}, {0, 0, 0, y}, {0, 0, 0, 0}}))
///
