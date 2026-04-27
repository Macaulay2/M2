-- -*- coding: utf-8 -*-
newPackage(
    "ExteriorExtensions", 
    Version => "1.0", 
    Date => "April 23, 2025", 
    Authors => {
	{Name => "Luke Oeding", Email => "oeding@auburn.edu", HomePage => "http://webhome.auburn.edu/~lao0004/"}}, 
    Headline => "Builds an algebra extending an exterior algebra", 
    Keywords => {"Lie Groups and Lie Algebras"},
    Certification => {
	"journal name" => "Journal of Software for Algebra and Geometry",
	"journal URI" => "https://msp.org/jsag/",
	"article title" => "ExteriorExtensions: a package for Macaulay2",
	"acceptance date" => "2025-07-16",
	"published article URI" => "https://msp.org/jsag/2025/15-1/p06.xhtml",
	"published article DOI" => "10.2140/jsag.2025.15.69",
	"published code URI" => "https://msp.org/jsag/2025/15-1/jsag-v15-n1-x06-ExteriorExtensions.m2",
	"version at publication" => "1.0",
	"volume number" => "15",
	"volume URI" => "https://msp.org/jsag/2025/15-1/"
	}
    )
export {
"ExteriorExtension", 
"exteriorExtension", 
"getDiagonal", 
"makeTraceless", 
"LieAlgebra", 
"appendage", 
"LieAlg2Matrix", 
"matrix2LieAlg", 
"findGrade", 
"HodgeStar", 
"bases", 
"bracket", 
"ad", 
"powerRanks", 
"powerTraces", 
"getBlock", 
"blockPowerRanks", 
"prettyBlockPowerRanks", 
"structureTensor",
"KillingMatrix"}
--------------------------------------------------------------------------------
-- CODE
--------------------------------------------------------------------------------

--------------------------------
-- subroutines --
--------------------------------

-- Input:
-- matrix M
-- Output:
-- matrix (the diagonal of M)
getDiagonal = M -> apply(rank source M, i-> M_(i, i));

-- Input:
-- matrix M (n x n)
-- Output:
-- matrix (M - (1/n)trace(M)*Id)
makeTraceless = M ->(R := ring M; n := rank source M; (M -(1/n)*(trace M)*id_(R^n)) );

ExteriorExtension = new Type of MutableHashTable; 
-- Input:
-- integers pow, nvars
-- Output:
-- no output - defines global variables and functions for the relevant rings, and the bracket operations
-- global mutableExports defined: exteriorAlgebra, HodgeStar, grade, basisG, bracket, Ad, powerRanks, powerTraces, getBlock, blockPowerRanks, prettyBlockPowerRanks, structureTensor, structureTensorMatrices, KillingMatrix
exteriorExtension = method();
--- The user just picks the first exterior module to add - the exterior power pow, and the number of variables nvars, the symbol e and the field QQ are chosen by default.
exteriorExtension (ZZ, ZZ) := (pow, nvars) -> (e:=local e; exteriorExtension(pow, nvars, e, QQ));
-- here the user picks their own ring KK, and we've tested  fraction fields, finite field extensions, rational numbers and finite fields, but do not currently expect this to function properly over RR or CC.
exteriorExtension (ZZ, ZZ, Ring) := (pow, nvars, KK) -> (e:=local e;  exteriorExtension(pow, nvars, e, KK));
-- the user may provide their own symbol
exteriorExtension (ZZ, ZZ, Symbol) := (pow, nvars, e) -> exteriorExtension(pow, nvars, e, QQ);
--- the user may provide their own symbol and ground ring.
exteriorExtension (ZZ, ZZ, Symbol, Ring) := (pow, nvars, e, KK) -> (
  if instance(KK, InexactFieldFamily) or instance(KK, InexactField) then error "inexact arithmetic not supported internally.";
  extensionAlg := new ExteriorExtension;
  ----------------------------------------------------------------------------------------------------------------
  --- setting up the exterior algebra and some of its properties
  ----------------------------------------------------------------------------------------------------------------
  exteriorAlg := KK[e_0..e_(nvars-1), SkewCommutative => true];
  extensionAlg.appendage = exteriorAlg;
  h := local h;
  E := local E;
  --- make the Lie algebra in variables
  LieAlg := KK[h_1..h_(nvars -1), 
      (flatten for i to nvars -1 list for j from i+1 to nvars -1 list E_{i, j}), 
      (flatten for i to nvars -1 list for j from i+1 to nvars -1 list E_{j, i})]; -- a copy of sl_n = a_0
  extensionAlg.LieAlgebra = LieAlg;
  -- store some properties of the Lie algebra:
  edim := binomial(nvars, pow); -- the dimension of a_1
  delta := (basis(nvars, exteriorAlg))_(0, 0); -- the volume form
  -- some properties of the appendage we're adding:
  basis1E := gens exteriorAlg;
  -- we need some helper functions to help compute the brackets later:
  setComplement:= (I, n) -> toList(set(0..(n-1)) - set I);
  indexStar := I -> det id_(ZZ^nvars)_(flatten {I, setComplement(I, nvars)});
  HodgeStar2function:= (IN) -> sum(subsets(nvars, first degree IN), aa -> contract(product(aa, i-> exteriorAlg_i), IN)*(indexStar aa)*product(setComplement(aa, nvars), i-> exteriorAlg_i));
  extensionAlg.HodgeStar = HodgeStar2function;
  --- make the Lie algebra in a preferred basis. We start with the Cartan, then the raising and lowering operators.
  lDim := nvars^2-1; -- the dimension of sl_n
  basisCartanSln := apply(nvars-1, i-> diagonalMatrix ( apply(i, k-> 0_KK)|{1_KK, -1}|apply(nvars-i-2, k-> 0_KK))); -- there is an extra (-1)^i
  -- nilpotent raising operators:
  basisN1 := flatten for i to nvars -1 list for j from i+1 to nvars -1 list (
    zm := mutableMatrix apply(nvars, j-> apply(nvars, i-> 0));
    tmp := zm;
    tmp_(i, j)=1;
    matrix tmp
    );
  basisN2 := transpose\ basisN1; -- nilportent lowering operators
  basisG0 := basisCartanSln|basisN1|basisN2; -- the entire Lie algebra:
  grade  := sub(nvars/gcd(nvars, pow), ZZ); -- the algebra is Z_m graded, this finds m
  basisG := new HashTable from {0=> basisG0}|apply(grade-1, i->
     (i+1)=> flatten entries basis(((i+1)*pow)%nvars, exteriorAlg)); -- makes a list of all the bases of the grade pieces
  extensionAlg.bases = basisG;
  -- function to convert from the degree of the input to the grading on the algebra.
  findGrade2 := elt -> (if class elt === Matrix then (return 0) else for i from 1 to grade-1 do if degree elt === degree (first basisG#i) then return i);
  extensionAlg.findGrade = findGrade2;
  -- I will need this matrix to solve for the expression in a basis of the result of a bracket. Used in the function matrix2LieAlg2:
  Qmat := transpose ( ( matrix apply(nvars-1, i-> getDiagonal( basisCartanSln_i)) )_(toList(0..nvars-2)));
  -- inverting this submatrix works because we only need to make the change of basis on traceless matrices
  genMatSln := sum(lDim, i -> sub(basisG0#i, LieAlg)*LieAlg_i); -- a generic matrix in sln
  -- convert matrix representatives of  Lie algebra elements to expressions in variables:
  matrix2LieAlg2 := mat -> (
    Mat := sub(mat, LieAlg);
    use LieAlg;
      sum( flatten for i to nvars -1 list for j to nvars -1 list (if i!=j then Mat_(i, j)*E_{i, j} else continue))+
     (transpose( Qmat^(-1)*transpose matrix{(getDiagonal Mat)_(toList(0..nvars-2))})*transpose((basis(1, LieAlg))_{0..nvars-2}))_(0, 0)
  );
  extensionAlg.matrix2LieAlg = matrix2LieAlg2;
  -- convert Lie algebra elements (in variables) to matrix representatives:
  LieAlg2Matrix2 := lform -> sum(flatten entries basis(1, LieAlg), xx -> contract(xx, lform)*contract(xx, genMatSln));
  extensionAlg.LieAlg2Matrix = LieAlg2Matrix2;
  ------------------------------------------------------------
  --------------------define the brackets helper functions
  ------------------------------------------------------------
  bracket0Ext := (mat, T) -> (
    s := flatten entries (matrix({basis1E})*mat);
    sum(nvars, i-> sub(T, basis1E_i=>s_i)) +(-nvars+(degree T)_0)*T );
  bracketExtExt := (t1, t2) -> (t1*t2);
  bracketExtExtDual := (u, us) -> makeTraceless(
    -diff(transpose diff(matrix{ basis1E}, u), diff(matrix{ basis1E}, HodgeStar2function us)) ); 
  bracketExtDualExt := (us, u) -> (-1)*transpose makeTraceless(
    -diff(transpose diff(matrix{ basis1E}, HodgeStar2function us), diff(matrix{ basis1E}, u))); 
  bracketExtDualExtDual := (t1, t2) -> HodgeStar2function( (HodgeStar2function t1)*(HodgeStar2function t2)); 
  ------------------------------------------------------------
  ----- put all brackets in one method
  ------------------------------------------------------------
  bracket2 := method();
  bracket2 (Matrix, Matrix) := (A, B) -> A*B-B*A;
  bracket2 (Matrix, exteriorAlg) := (A, T) -> bracket0Ext(A, T);
  bracket2 (exteriorAlg, Matrix) := (T, A) -> -bracket0Ext(A, T); -- make it skew-commuting even when the grades don't agree -- we could change this later.
  bracket2 (exteriorAlg, exteriorAlg) := (S, T) -> (
    if  S == 0 or  T  == 0  then return 0;  -- zero
    if (degree S)#0 + (degree T)#0 <nvars then return bracketExtExt(S, T);
    if (degree S)#0 + (degree T)#0 ==nvars and (degree S)#0 <= (degree T)#0 then return bracketExtExtDual(S, T);
    if (degree S)#0 + (degree T)#0 ==nvars and (degree S)#0 > (degree T)#0 then return bracketExtDualExt(S, T);
    if (degree S)#0 >nvars/2 or (degree T)#0 >nvars/2 then return bracketExtDualExtDual(S, T);
  );
  extensionAlg.bracket = bracket2;
  ------------------------------------------------------------
  ----- Making functions attached to the exteriorExtension:
  ----- Functions that are exported are documented in the doc files
  ------------------------------------------------------------  
  --- make the adjoint operator Ad_T(x) = [T, x] for elements in the exterior Algebra
  -- Input:
  -- an element T of the exteriorAlgebra (or a matrix mat)
  -- Output:
  -- matrix (the standard matrix rep of the adjoint operator)
  Ad := method();
  Ad (exteriorAlg) := T-> (
      tGrade := findGrade2 T; -- changed order of the double loop and put in some transposes...
     matrix for i from 0 to grade-1 list( for j to grade-1 list(
  	    if (tGrade + j)%grade == i then(
  	        if i==0 then sub( transpose matrix apply(basisG#j, 
                 myForm -> flatten entries contract(matrix2LieAlg2(bracket2(T, myForm)), basis(1, LieAlg)) ) , KK)
  	        else  ( sub( transpose matrix apply(basisG#j, 
                 myForm -> flatten entries contract(matrix {basisG#i}, bracket2(T, myForm))), KK) )
     )
  	    else ( transpose matrix map (KK^(#basisG#j), KK^(#basisG#i), 0) )
  ))
  );
  Ad (Matrix) := mat-> (
     matrix for i from 0 to grade-1 list( for j to grade-1 list(
  	    if i==j then
  	        if i==0 then transpose sub(matrix(apply(basisG#0, xx -> flatten entries( contract(matrix2LieAlg2(bracket2(mat, xx)), basis(1, LieAlg))) )), KK)
  	        else sub(transpose matrix apply(basisG#i, ww-> flatten entries contract(matrix{basisG#i}, bracket2( mat, ww))), KK)
  	    else ( transpose matrix map (KK^(#basisG#j), KK^(#basisG#i), 0) )
  ))
  );
  extensionAlg.ad = Ad;
  -- Input:
  -- integers i, j, and matrix GT
  -- Output:
  -- matrix (the i, j block of GT (indices refer to the graded pieces)
  getBlock2 := (i, j, GT) -> ( bs:= apply(grade+1, i->  sum(i, j->#basisG#j));
       GT^{bs#i..bs#(i+1) -1}_{bs#j..bs#(j+1) -1}
      );
  extensionAlg.getBlock = getBlock2;
  -- Input:
  -- matrix mat
  -- Output:
  -- list of powerRanks of matrix up to the dimension of the algebra
  powerRanks2 := mat -> (
    rtmp := 0;  tt := mat;
    {rank tt}|for i from 2 to nvars^2 + edim-1 list( tt = tt*mat;
      tmp:=rank(tt); if rtmp !=tmp then (rtmp=tmp; tmp) else break)
  );
  extensionAlg.powerRanks = powerRanks2;
  blockNames := flatten(apply(grade, j->  apply(grade, i-> {i, j})));
  blockRank := tt -> apply(blockNames, xx -> rank getBlock2(xx#0, xx#1, tt));
  -- Input:
  -- matrix mat
  -- Output:
  -- table of powerRanks of blocks of powers of mat
  blockPowerRanks2 := mat -> (flag:=0; rtmp := 0; tt := mat; tmp := rank tt;
   {blockRank(tt)| {tmp}}|
   for i from 2 to nvars^2 + edim-1 list( tt = tt*mat; tmp:=rank(tt);
  	if rtmp ==tmp then flag=1+flag;
  	if flag <3 then (rtmp=tmp; blockRank(tt)| {tmp}) else break )
      );
  extensionAlg.blockPowerRanks = blockPowerRanks2;
  -- Input:
  -- matrix mat
  -- Output:
  -- netlist labeled table of powerRanks of blocks of powers of mat
  prettyBlockPowerRanks2 := mat -> netList ({apply(blockNames, xx ->
  	    concatenate(concatenate("g", toString (xx#0)), toString (xx#1)))|{toString("total")}} | blockPowerRanks2 mat);
  extensionAlg.prettyBlockPowerRanks =prettyBlockPowerRanks2;
  -- Input:
  -- matrix mat
  -- Output:
  -- table of values of traces of powers of mat until it stabilizes
  powerTraces2 := mat -> ( rtmp := 0; tt := mat; tmp := rank tt;
    NL := {{toString("power"), toString("trace"), toString("rank")}};
    NL = NL|( {{1}|{trace(tt)}| {tmp}});
    for i from 2 to nvars^2 + 2*edim-1 do( tt = tt*mat; tmp:=rank(tt);
  	 if tmp!=0 then NL = NL| ({{i}|{trace(tt)}|{tmp}}) else break);
    netList NL );
  extensionAlg.powerTraces = powerTraces2;
  extensionAlg
 -- output everything:
 --(exteriorAlgebra, basisG, grade, findGrade, HodgeStar, bracket, Ad, powerRanks, powerTraces, getBlock, 
--  blockPowerRanks, prettyBlockPowerRanks, structureTensor)
);

structureTensor = method();
structureTensor (ExteriorExtension) := ea -> ( 
  -- Input: ExteriorExtension
   -- Output:
   -- the structureTensor of the algebra -- may take a long time.
B := apply(flatten apply(#ea.bases, i-> ea.bases#i), xx-> entries ea.ad(xx)); -- this could be parallelized, but it might not be faster.
bTmp := length B;
new HashTable from flatten flatten for k to bTmp-1 list for j to bTmp-1 list for i to bTmp-1 list if B#i#j#k !=0 then (i,j,k)=>B#i#j#k else continue 
)

KillingMatrix = method();
KillingMatrix (ExteriorExtension) := ea -> (
  basisG:=ea.bases;
  grade:=#basisG;
 -- Input: ExteriorExtension
 -- Output: the Killing Matrix
 -- the Killing matrix of the algebra (traces of brackets of all basis elements) -- may take a long time.
 stm := for xx in flatten apply(grade, i-> basisG#i) list ea.ad(xx); -- serial way
 --stm := parallelApply(flatten apply(grade, i-> basisG#i), xx-> ea.ad(xx)); -- parallel way to compute this
 matrix(apply(stm, xx -> apply(stm, yy -> trace(xx*yy)))) 
)

beginDocumentation()

doc ///
Key
  ExteriorExtensions
Headline
  Builds a \(\mathbb{Z}_m\) -graded algebra that equivariantly extends a Lie algebra
Description
  Text
    {\em ExteriorExtensions} Builds a \(\mathbb{Z}_m\) -graded algebra that equivariantly extends a Lie algebra \( \mathfrak{sl}_n\) via the non-zero graded piece of the exterior algebra by defining the bracket products. Constructs matrix representations of adjoint operators. Computes ranks of blocks coming from the grading.
    Computing matrix representations of adjoint operators and computing ranks of their blocks. The user can also call for the Killing matrix of the algebra.

Subnodes
  exteriorExtension
  ExteriorExtension
///

doc ///
Key
  ExteriorExtension

Headline
  A new algebra constructed from the direct sum of a Lie algebra and a module

Description
  Text
    A ExteriorExtension (treated as a mutable hash table) is built by the command {\tt exteriorExtension}. The menu lists the ways to interact with an ExteriorExtension.
    The operation takes the Lie algebra \(\mathfrak{sl}_n(\mathbb{F}\) of traceless \(n \times n\) matrices over a field \(\mathbb{F}\) and appends the non-zero graded piece of an exterior algebra \(\bigoplus_{i=1}^{n-1}\bigwedge^i \mathbb{F}^n \).
  Example
    ea = exteriorExtension(4, 8);

Subnodes
  LieAlgebra
  appendage
  findGrade
  HodgeStar
  bases
  bracket
  ad
  powerRanks
  powerTraces
  getBlock
  blockPowerRanks
  prettyBlockPowerRanks
  structureTensor
  KillingMatrix
  LieAlg2Matrix
  matrix2LieAlg
///

doc ///
Key
  exteriorExtension
  (exteriorExtension, ZZ, ZZ)
  (exteriorExtension, ZZ, ZZ, Symbol)
  (exteriorExtension, ZZ, ZZ, Ring)
  (exteriorExtension, ZZ, ZZ, Symbol, Ring)

Headline
  Make an extension of an exterior algebra

Usage
  extensionAlgebra = exteriorExtension(pow, nvars)
  extensionAlgebra = exteriorExtension(pow, nvars, e)
  extensionAlgebra = exteriorExtension(pow, nvars, KK)
  extensionAlgebra = exteriorExtension(pow, nvars, e, KK)

Inputs
  pow:ZZ
  nvars:ZZ
  e:Symbol
  KK:Ring
Outputs
  extensionAlgebra:ExteriorExtension
Consequences
  Item
    Sets the value for several symbols and functions attached to the ExteriorExtension:
    "appendage", "findGrade", "HodgeStar", "bases", "bracket", "ad", "powerRanks", "powerTraces", 
    "getBlock", "blockPowerRanks", "prettyBlockPowerRanks",

Description
  Text
    This module sets the functions and parameters that make the required algebra.
    It constructs a natural graded algebra \(\mathfrak{a}\), with \(\mathfrak{a}_0 =  \mathfrak{sl}_n\), and  \(\mathfrak{a}_1= \bigwedge^k \mathbb{C}^n\), and \(\mathfrak{a}_i\) for \(i>1\) defined as needed to make the algebra closed.
    Provides some functionalitity for computations with the algebra. Sometimes we build a known algebra. Let's see how this might work.

  Example
    ea24 = exteriorExtension(2, 4, QQ)
  Text
    We have set the algebra \(\mathfrak{sl}_2(\mathbb C) \oplus \bigwedge^{2}\mathbb{C}\). The functions associated to this algebra are stored by the symbol we assign the output. 
    Let's see if the bilinear bracket satisfies the Lie algebra axioms at least at random points in each graded piece of the algebra:
    i.e. we check skew-commutativity and the Jacobi identity.
  Example
    A = makeTraceless random(QQ^4, QQ^4)
    B = makeTraceless random(QQ^4, QQ^4)
    ea24.bracket(A, B) + ea24.bracket(B, A)
    ea24.ad(ea24.bracket(A, B)) - ea24.bracket(ea24.ad(A), ea24.ad(B))
  Text
    We shouldn't be surprised that we get 0 for both since the grade 0 piece is a Lie algebra by assumption.

  Example
    A = random(2, ea24.appendage)
    B = random(2, ea24.appendage)
    ea24.bracket(A, B) + ea24.bracket(B, A)
    ea24.ad(ea24.bracket(A, B)) - ea24.bracket(ea24.ad(A), ea24.ad(B))
  Text
    For both points in grade 1 we see the axioms hold at least for random elements.

  Example
    A = makeTraceless random(QQ^4, QQ^4)
    B = random(2, ea24.appendage)
    ea24.bracket(A, B) + ea24.bracket(B, A)
    ea24.ad(ea24.bracket(A, B)) - ea24.bracket(ea24.ad(A), ea24.ad(B))
  Text
    For a point in grade 0 and a point in grade 1 we see the axioms hold.
    Caveat: It's important to make the matrix traceless, otherwise the Jacobi identity doesn't work.

  Example
    printWidth =200;
    K = KillingMatrix ea24
    rank K
  Text
    The Killing matrix is \(21 \times 21\) and non-degenerate. The only possibility is \(\mathfrak{sp}_6\).
    One checks that the Killing matrix has a full set of real eigenvectors, so the isomorphism of \(\mathfrak{a}\) and \(\mathfrak{sp}_6\) holds over the real numbers.
  Example
   toList eigenvalues K 

  Example
    ea36 = exteriorExtension(3, 6, QQ)
  Text
    Let's try a slightly larger example.
  Example
    printWidth = 200
    A = ea36.ad first ea36.bases#1
  Text
    Produces the \(55 \times 55\) adjoint matrix associated to the first basis vector of \(\mathfra{a}_1\), and computes the ranks of the blocks for each power.
  Example
    ea36.prettyBlockPowerRanks A

  Text
    This computes the ranks of the blocks for each power of the matrix. The blocks are determined by the dimensions of the graded pieces of the algebra.

///

doc ///

Key
  getDiagonal

Headline
  Extract the diagonal of a matrix

Usage
  v = getDiagonal(A)

Inputs
  A:Matrix
Outputs
  v:List

Description
  Text
    Extract the diagonal of a matrix
  Example
    A = random(QQ^4, QQ^4)
    v = getDiagonal A

///

doc ///
Key
  makeTraceless

Headline
  Project matrix to the traceless matrices.

Usage
  Ap = makeTraceless(A)

Inputs
  A:Matrix
Outputs
  Ap:Matrix

Description
  Text
    Project matrix to the traceless matrices.
  Example
    A = random(QQ^4, QQ^4)
    Ap = makeTraceless A


///

doc ///
Key
  appendage

Headline
  a placeholder for the object (exterior algebra) added on to the Lie algebra by exteriorExtension

Usage
  extensionAlgebra.appendage

Description
  Text
    Exporting this symbol gives the ring a name that the user can see when, for instance, matrices are constructed between modules over this ring

  Example
    extensionAlg = exteriorExtension(3, 6, e, QQ);
    gens extensionAlg.appendage
    e_0*e_1

  Text
    This is the exterior algebra.
SeeAlso
  bracket
  HodgeStar
///

doc ///
Key
  LieAlgebra

Headline
  a placeholder for the base Lie algebra

Usage
  extensionAlgebra.LieAlgebra

Description
  Text
    Exporting this symbol gives the ring a name that the user can see when, for instance, matrices are constructed between modules over this ring

  Example
    extensionAlg = exteriorExtension(3, 6, e, QQ);
    gens extensionAlg.LieAlgebra

  Text
    This is the underlying matrix Lie algebra.

  Example
    extensionAlg.bases#0

  Text
    This is another way to find the basis of the Lie algebra as it is the 0-graded piece of the Extension Algebra


SeeAlso
  bracket
  bases
///

doc ///
Key
  bracket

Headline
  Compute products in an ExteriorExtension

Description
  Text
    The bracket of two elements in the Exterior Extension.
    This is a method function that accepts pairs from any part of the ExteriorExtension
  Example
    ea = exteriorExtension(2, 4, e, QQ);
    A = first ea.bases#0
    B = last ea.bases#1
    C = ea.bracket(A, B)



///

doc ///
Key
  ad

Headline
  Constructs the standard matrix representation of the adjoint operator

Description
  Text
    The matrices constructed by this can get very large.

  Example
    ea = exteriorExtension(3, 7, e, QQ);
    A = ea.ad(first ea.bases#1);
    rank A

  Text
    This rank is an invariant
SeeAlso
  powerRanks
  powerTraces
  blockPowerRanks
  prettyBlockPowerRanks
  getBlock
///

doc ///
Key
  powerRanks

Headline
  Compute the ranks of powers of a matrix

Description
  Text
    This is what you do:

  Example
    ea = exteriorExtension(3, 6, QQ);
    A = ea.ad(first ea.bases#1);
    ea.powerRanks A

  Text
    The list outputs the rank of each power. These are invariants of the element x in ad(x).
///

doc ///
Key
  powerTraces

Headline
  Compute the traces of powers of a matrix

Description
  Text
    This is what you do:

  Example
    ea = exteriorExtension(3, 6, QQ);
    A = ea.ad(first ea.bases#1);
    ea.powerTraces A

  Text
    The list outputs the trace of each power until the power ranks stabilize, or until the power is equal to the dimension of the algebra. These are invariants of the element x in ad(x).

  Example
    ea = exteriorExtension(3, 9, QQ);
    E = ea.appendage
    A = ea.ad(E_0*E_1*E_2 + E_3*E_4*E_5 + E_6*E_7*E_8);
    ea.powerTraces A  
  Text
    The list could be quite long, as this example shows.    
///

doc ///
Key
  structureTensor

Headline
  Constructs the structureTensor of the ExteriorExtension

Description
  Text
    The structure tensor of an algebra is the representative of the bilinear product as a hypermatrix.
    The matrices constructed by this can get very large, and take a long time to compute. The output can be very large, so we return a hashTable with only the non-zero values.

  Example
    printWidth = 450;
    ea = exteriorExtension(3, 6, QQ);
    B = structureTensor ea
    
  Text
    This tensor B can be used to compute any product of basis elements -- the number in \(B(i,j,k)\) is the coefficient on \(e_k\) of the product of \(e_i\) and \(e_j\).
///

doc ///
Key
  KillingMatrix

Headline
  Constructs the Killing matrix of the ExteriorExtension

Description
  Text
    The Killling matrix is constructed by taking the adjoint of every basis vector of the algebra, then computing the trace of each bracket between them and storing the results in a matrix. 
    The matrices constructed by this can get very large, and take a long time to compute.

  Example
    printWidth = 450;
    ea = exteriorExtension(3, 6, QQ);
    K = KillingMatrix ea
    rank K

  Text
    This rank is an invariant of the algebra.
///

doc ///
Key
  HodgeStar

Headline
  The Hodge star in an exterior algebra

Description
  Text
    The Hodge star works like this:

  Example
    ea = exteriorExtension(3, 6, e, QQ);
    ea.HodgeStar(first ea.bases#1)

  Text
    This is the exterior algebra. {\tt HodgeStar} of a module is the Hodge dual.
///


doc ///
Key
  getBlock

Headline
  Extract a block of the matrix representation of an adjoint operator for ExteriorExtensions

Description
  Text
    The blocks get extracted based on their grade
    -- Input:
    -- integers i, j, and matrix GT
    -- Output:
    -- matrix (the i, j block of GT (indices refer to the graded pieces)

  Example
    ea = exteriorExtension(3, 6, e, QQ);
    A = ea.ad(first ea.bases#1);
    printWidth = 300;
    ea.getBlock(1, 1, A)
    ea.getBlock(1, 0, A)

  Text
    This extracts blocks of the operator. The sizes are determined by the graded pieces of the algebra.
///

doc ///
Key
  blockPowerRanks

Headline
  Computes a list of ranks of blocks for each power of a matrix representation of an adjoint operator for ExteriorExtensions

Description
  Text
    -- Input:
    -- matrix mat
    -- Output:
    -- table of ranks of blocks of powers of mat

  Example
    ea = exteriorExtension(3, 6, e, QQ);
    A = ea.ad(first ea.bases#1);
    ea.blockPowerRanks(A)

  Text
    This computes a list of ranks of blocks for each power of a matrix representation of an adjoint operator for ExteriorExtensions
SeeAlso
  prettyBlockPowerRanks
///

doc ///
Key
  prettyBlockPowerRanks

Headline
  same as blockPowerRanks, but prettier

Description
  Text
    -- Input:
    -- matrix mat
    -- Output:
    -- labeled netList table of ranks of blocks of powers of mat

  Example
    ea = exteriorExtension(3, 6, e, QQ);
    A = ea.ad(first ea.bases#1);
    ea.prettyBlockPowerRanks(A)

  Text
    This computes a list of ranks of blocks for each power of a matrix representation of an adjoint operator for ExteriorExtensions and displays it as a pretty table.

SeeAlso
  blockPowerRanks
///

doc ///
Key
  findGrade

Headline
  Find which graded piece of the algebra an element is in

Description
  Text
    Let's build a 7-graded algebra and find the grade for several elements

  Example
    extensionAlg = exteriorExtension(3, 7);
    E = extensionAlg.appendage;
    extensionAlg.findGrade (E_0*E_1)
    extensionAlg.findGrade (E_0*E_1*E_2*E_3)

///

doc ///
Key
  bases

Headline
  gives a list of the bases of the different graded pieces of the ExteriorExtension

Description
  Text
    Let's build a 7-graded algebra and see the bases of each piece

  Example
    extensionAlg = exteriorExtension(3, 7);
    extensionAlg.bases#0
    extensionAlg.bases#1
    extensionAlg.bases#3

///

doc ///
Key
  LieAlg2Matrix

Headline
  Converts from the LieAlgebra to Matrix form (with the standard basis)

Description
  Text
    Let's build a 7-graded algebra and see the bases of each piece and see the elements of the (symbolic) Lie algebra get converted to matrices

  Example
    ea = exteriorExtension(3, 7, QQ);
    sln = ea.LieAlgebra;
    (gens sln)#0
    ea.LieAlg2Matrix (gens ea.LieAlgebra)#0

///


doc ///
Key
  matrix2LieAlg

Headline
  Converts from Matrix form to the LieAlgebra (with the standard basis)

Description
  Text
    Let's build a 7-graded algebra and see the bases of each piece getting converted to the symbolic ements in the Lie algebra.

  Example
    ea = exteriorExtension(3, 7, QQ);
    ea.bases#0#0
    sln = ea.LieAlgebra
    ea.matrix2LieAlg (ea.bases#0#0)
    ea.bases#0#8
    ea.matrix2LieAlg (ea.bases#0#8)
  Text
    If you don't name the Lie algebra something it has an annoying printout.
///

end
--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

restart
uninstallPackage("ExteriorExtensions")
installPackage"ExteriorExtensions"

restart
loadPackage"ExteriorExtensions"
ea24 = exteriorExtension(2, 4, e, QQ)
A = ea24.ad(e_0*e_1 + e_2*e_3)
ea24.prettyBlockPowerRanks A
ea24.powerTraces A
sort eigenvalues A
(S,U,Vt) =SVD(sub(A, RR));
B = ea24.ad(e_0*e_2 + e_1*e_3);
ea24.bracket(A, B) + ea24.bracket(B, A)

ea24.bracket(A, ea24.ad(e_0*e_1))
ea24.prettyBlockPowerRanks A

B = ea24.ad(e_0*e_2 + e_1*e_3);
0*ea24.bracket(A, B)== ea24.bracket(A, B)

p = e_0*e_1 + e_2*e_3
q = e_0*e_2 + e_1*e_3
ea24.bracket(e_0*e_1 + e_2*e_3, e_0*e_2 + e_1*e_3)
ea24.bracket(ea24.ad(p), ea24.ad(q))
ea24.ad(ea24.bracket(p, q))

L = apply(ea24.bases#1 , xx-> sub( ea24.bracket( xx, p), QQ))
R = QQ[x_0.. x_5]
decompose ideal flatten entries sum(6, i-> sub(L_i, R)*x_i)
first ea24.bases#1 + last ea24.bases#1

restart

loadPackage"ExteriorExtensions"
ea48 = exteriorExtension(4, 8, e, QQ);
    A = makeTraceless random(QQ^8, QQ^8);
    B = makeTraceless random(QQ^8, QQ^8);
    ea48.bracket(A, B) + ea48.bracket(B, A)
    ea48.ad(ea48.bracket(A, B)) - ea48.bracket(ea48.ad(A), ea48.ad(B))

    a = random(4, ea48.appendage)
    b = random(4, ea48.appendage)
    ea48.bracket(a, b) + ea48.bracket(b, a)
    ea48.ad(ea48.bracket(a, b)) - ea48.bracket(ea48.ad(a), ea48.ad(b))

    ea48.bracket(A, b) + ea48.bracket(b, A)
    ea48.ad(ea48.bracket(A, b)) - ea48.bracket(ea48.ad(A), ea48.ad(b))
time K = KillingMatrix ea48
rank K

matrix apply(7, i-> apply(7, j-> 2*K_(i, j)/K_(j, j)))
K - transpose K -- symmetric
rank K  -- non-degenerate
kev = toList eigenvalues K
#kev
time stm0 =  for xx in  ea48.bases#0 list ea48.ad(xx);  
time stm1 =  for xx in  ea48.bases#1 list ea48.ad(xx);
unique apply(stm0, xx -> source xx)
unique apply(stm0, xx -> target xx)
M0 = matrix apply(stm0, xx-> apply(stm0, yy->  trace(xx*yy)))
M01 = matrix apply(stm0, xx-> apply(stm1, yy->  trace(xx*yy)))
M1 = matrix apply(stm1, xx-> apply(stm1, yy->  trace(xx*yy)))
eigenvalues M1

-- find a cartan subalgebra? It's strange that if you pick the cartan in sl_8, perhaps the roots have different angles than if you pick a cartan in the other part?
apply( ea48.bases#1, xx-> apply( ea48.bases#1, yy-> ea48.bracket(xx, yy)))

time stm1 =  for xx in  ea48.bases#1 list ea48.ad(xx);
--- maximal abelian subalgebra such that the adjoint operators are diagonalizable. 
L = stm1_(toList (0..8));
 apply( L, xx-> apply( L, yy-> ea48.bracket(xx, yy) ))
apply(9, i-> eigenvalues L#i) -- these are all nilpotent!
Ap = ea48.ad(first ea48.bases#1);
 for xx in eigenvalues Ap list if not round(abs(xx)) ==0 then xx else continue
rank Ap

list2E = L-> sum(L, l-> product(l, i-> e_(i-1)))

a0 = list2E {{1, 2, 3, 4}, {5, 6, 7, 8}}
A0 = ea48.ad(a0);
# for xx in eigenvalues A0 list if not round(abs(xx)) ==0 then xx else continue
rank A0

-- more examples
restart
loadPackage"ExteriorExtensions"
ea48 = exteriorExtension(4, 8, e, QQ);

list2E = L-> sum(L, l-> product(l, i-> e_(i-1)))
aa = {{{1, 2, 3, 4}, {5, 6, 7, 8}}, {{1, 3, 5, 7}, {6, 8, 2, 4}}, {{1, 5, 6, 2}, {8, 4, 7, 3}}, 
{{1, 6, 8, 3}, {4, 7, 5, 2}}, {{1, 8, 4, 5}, {7, 2, 6, 3}}, {{1, 4, 7, 6}, {2, 3, 8, 5}}, {{1, 7, 2, 8}, {3, 5, 4, 6}}}

for i to length(aa) -1 do(
adList_i = ea48.ad(list2E aa_i);
print(# for xx in eigenvalues adList_i list if not round(abs(xx)) ==0 then xx else continue, rank adList_i);
)

netList apply(7, i-> apply(7, j-> ea48.bracket(adList_i, adList_j) ))


    A = makeTraceless random(QQ^8, QQ^8);
    B = makeTraceless random(QQ^8, QQ^8);
    ea48.bracket(A, B) + ea48.bracket(B, A)
    ea48.ad(ea48.bracket(A, B)) - ea48.bracket(ea48.ad(A), ea48.ad(B))

    a = random(4, ea48.appendage);
    b = random(4, ea48.appendage);
    ea48.bracket(a, b) + ea48.bracket(b, a)
    ea48.ad(ea48.bracket(a, b)) - ea48.bracket(ea48.ad(a), ea48.ad(b))

    ea48.bracket(A, b) + ea48.bracket(b, A)
    ea48.ad(ea48.bracket(A, b)) - ea48.bracket(ea48.ad(A), ea48.ad(b))



time stm0 =  for xx in  ea48.bases#0 list ea48.ad(xx);
L0 = stm0_(toList (0..6));
 apply( L0, xx-> apply( L0, yy-> ea48.bracket(xx, yy) ))

L0 = stm0_(toList (0..7));
netList apply( L0, xx-> apply( L0, yy-> ea48.bracket(xx, yy) == 0 ))
time K = KillingMatrix ea48
rank K

restart
loadPackage"ExteriorExtensions"
ea = exteriorExtension(2,4); 
keys ea

KillingMatrix ea
T = structureTensor ea
ea.bracket(first ea.bases#0, ea.bases#1#1)
ea.ad(first ea.bases#1)
ea.HodgeStar first ea.bases#1
ea.powerTraces(ea.ad(first ea.bases#1 + last ea.bases#1))
ea#LieAlgebra

extensionAlg = exteriorExtension(3, 6, e, QQ);
    gens extensionAlg.LieAlgebra

peek exteriorExtension
    ea = exteriorExtension(3, 9)
    E = ea.appendage
    A = makeTraceless random(QQ^9, QQ^9);
    B = makeTraceless random(QQ^9, QQ^9);
    ea.bracket(A, B) + ea.bracket(B, A)
    ea.ad(ea.bracket(A, B)) -ea.bracket(ea.ad(A), ea.ad(B)) 

   A = makeTraceless random(QQ^9, QQ^9);
   B = random(3, E);
   ea.bracket(A, B) + ea.bracket(B, A)
   t1 =    ea.ad(ea.bracket(A, B));
   t2 =  ea.bracket(ea.ad(A), ea.ad(B));
   t1-t2   -- gives zero! 
   A = makeTraceless random(QQ^9, QQ^9);
   B = random(6, E);
   ea.bracket(A, B) + ea.bracket(B, A)
   ea.ad(ea.bracket(A, B)) - ea.bracket(ea.ad(A), ea.ad(B)) -- gives zero!
   A = random(3, E);
   B = random(6, E);
   ea.bracket(A, B) + ea.bracket(B, A) 
   t1 =    ea.ad(ea.bracket(A, B));
   t2 =  ea.bracket(ea.ad(A), ea.ad(B)); 
   rank(t1-t2) -- gives zero!
   A = makeTraceless random(QQ^9, QQ^9)
   pt = random(3, E);
   ea.bracket(A, ea.HodgeStar(pt )) + ea.HodgeStar ea.bracket(transpose A, pt) -- checks equations 2.5 and 2.6 from Vinberg-Elashvili
   pt = E_0*E_1*E_2; tmp  = ea.bracket(pt, ea.HodgeStar(pt)) 
   ea.bracket(tmp, pt) 


restart
loadPackage"ExteriorExtensions"
viewHelp ExteriorExtensions
 ea = exteriorExtension(3, 6, QQ);
B = structureTensor(ea);
#keys B
