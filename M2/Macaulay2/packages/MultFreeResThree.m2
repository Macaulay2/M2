newPackage ( "MultFreeResThree",
    Version => "0.1",
    Date => "22 April 2020",
    Authors => {
	{ Name => "Lars Winther Christensen",
	  Email => "lars.w.christensen@ttu.edu",
	  HomePage => "http://www.math.ttu.edu/~lchriste/index.html" },
      { Name => "Luigi Ferraro",
	  Email => "ferrarl@wfu.edu",
	  HomePage => "http://users.wfu.edu/ferrarl/" },
	{ Name => "Francesca Gandini",
	  Email => "fra.gandi.phd@gmail.com",
	  HomePage => "TBD" },
	{ Name => "Oana Veliche", 
	  Email => "o.veliche@northeastern.edu",
	  HomePage => "https://cos.northeastern.edu/faculty/oana-veliche/" }
	},
    Headline => "Multiplication in free resolution of length three",
    -- Certification => { -- this package was certified under its old name, "CodepthThree"
    -- 	 "journal name" => "The Journal of Software for Algebra and Geometry",
    -- 	 "journal URI" => "http://j-sag.org/",
    -- 	 "article title" => "Local rings of embedding codepth 3: A classification algorithm",
    -- 	 "acceptance date" => "2014-07-11",
    --      "published article DOI" => "http://dx.doi.org/10.2140/jsag.2014.6.1",
    -- 	 "published article URI" => "http://msp.org/jsag/2014/6-1/jsag-v6-n1-p01-s.pdf",
    -- 	 "published code URI" => "http://msp.org/jsag/2014/6-1/jsag-v6-n1-x01-code.zip",
    -- 	 "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/CodepthThree.m2",
    -- 	 "release at publication" => "4b2e83cd591e7dca954bc0dd9badbb23f61595c0",
    -- 	 "version at publication" => "1.0",
    -- 	 "volume number" => "6",
    -- 	 "volume URI" => "http://msp.org/jsag/2014/6-1/"
    -- 	 }
    Reload => true,
    DebuggingMode => false
    )

export { "multtable" }


--==========================================================================
-- EXPORTED FUNCTIONS
--==========================================================================

----------------------------------------------------------------------------
-- Implementation of the classification algorithm
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- torAlgData

-- R a quotient of a polynomial algebra

-- Returns a hash table with the following data of the local ring
-- obtained by localizing R at the irrelevant maximal ideal:

-- c: codepth
-- e: embedding dimension
-- h: Cohen-Macaulay defect
-- m: minimal number of generators the defining ideal
-- n: type
-- Class: class (B ,C ,G ,GS, GT, GH, H ,S, T) in the classification due to 
--   Weyman and to Avramov Kustin and Miller; see [L.L. Avramov, 
--   A cohomological study of local rings of embedding codepth 3, 
--   J. Pure Appl. Algebra, 216, 2489--2506 (2012)] and [L.L. Avramov, Homological
--   asymptotics  of modules over local rings, Commutative algebra (Berkeley, CA, 1987), 
--   Math. Sci. Res. Inst.  Publ., vol. 15, Springer, New York, 1989, pp.~33--62] 
--   for an overview
-- p: classification parameter
-- q: classification parameter
-- r: classification parameter
-- isCI: boolean, TRUE if R is complete intersection, otherwise FALSE
-- isGolod: boolean, TRUE if R is Golod, otherwise FALSE
-- isGorenstein: boolean, TRUE if R is Gorenstein, otherwise FALSE
-- PoincareSeries: rational function, Poincare series in closed form
-- BassSeries: rational function, Bass series in closed form

multtable = (d1,d2,d3) -> (
    Q := ring d1; --add check that all matrices come from same ring
    m := numcols d1;
    l := numcols d2;
    n := numcols d3;
    
    EE := new MutableHashTable;
    for i from 1 to m do (
	for j from i+1 to m do (
	 a := d1_(0,i-1)*(id_(Q^m))^{j-1} - d1_(0,j-1)*(id_(Q^m))^{i-1};
    	 b := ( matrix entries transpose a ) // d2;
	 EE#(i,j) = ( matrix entries b );
	 EE#(j,i) = -EE#(i,j);
	 );
     for i from 1 to m do (EE#(i,i) = matrix entries map(Q^l,Q^1,(i,j) -> 0)); --move this after the );?
     );

    EF := new MutableHashTable;
    for i from 1 to m do (
	for j from 1 to l do (
    	    c := sum(1..m, k -> d2_(k-1,j-1) * (EE#(i,k)));
    	    d := d1_(0,i-1)*((id_(Q^l))_(j-1));
	    e := (matrix entries (matrix d - c)) // d3;
    	    EF#(i,j) = (matrix entries e);
	    );
	);
    EEE := new MutableHashTable;
    for i from 1 to m do (
	for j from i+1 to m do (
	    for k from j+1 to m do(
    	    c := sum(1..l, s -> (EE#(i,j))_(s-1,0)*(EF#(k,s)));
    	    EEE#(i,j,k) = (matrix entries c);
	    EEE#(j,i,k) = -EEE#(i,j,k);
	    EEE#(i,k,j) = -EEE#(i,j,k);
	    EEE#(k,j,i) = -EEE#(i,j,k);
	    EEE#(j,k,i) = EEE#(i,j,k);
	    EEE#(k,i,j) = EEE#(i,j,k);
	    );
	);
    );
    for i from 1 to m do(
	for j from 1 to m do(
	    EEE#(i,i,j) = matrix entries map(Q^n,Q^1,(i,j) -> 0);
	    EEE#(i,j,i) = matrix entries map(Q^n,Q^1,(i,j) -> 0);
	    EEE#(j,i,i) = matrix entries map(Q^n,Q^1,(i,j) -> 0);
	   );
     );
	
 EEE
 )

--EEE = (i,j,k) -> (
--l := numgens source d2 ;
--c := sum(1..l, s -> (EE(i,j))_(s-1,0)*EF(k,s));
--return matrix entries c)

-- torAlgData = ( cacheValue "torAlg" ) toralgdata
	    

end
--==========================================================================
-- end of package code
--==========================================================================

uninstallPackage "MultFreeResThree"
restart
loadPackage "MultFreeResThree"
--check "TorAlgebra"

Q = QQ[x,y,z]

I = ideal (x*y, y*z, x^3, y^3-x*z^2,x^2*z,z^3)

F = res I
    d1 = matrix entries (F.dd)_1;
    d2 = matrix entries (F.dd)_2;
    d3 = matrix entries (F.dd)_3;    

ee = multtable(d1,d2,d3)
peek ee
ee#(1,2,3)
ee#(2,1,3)
a = map(Q^1,Q^3,(i,j) -> 0)

numcols ee#(1,1)

n=numcols d3

 matrix entries map(Q^n,Q^1,(i,j) -> 0)

----------------------------------------------------------------------------
-- Functions for presenting classification data
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- torAlgDataList
--
-- R a quotient of a polynomial algebra
-- L a list of keys for the hash table returned by torAlgClass
--
-- Returns a list of the values of the specified keys

--==========================================================================
-- INTERNAL ROUTINES
--==========================================================================

----------------------------------------------------------------------------
-- Routines used by 
----------------------------------------------------------------------------

----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- Auxiliary routines
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- sup
--
-- C a chain complex
--
-- Returns the supremum of C: the highest degree of a non-zero module

-- sup = C -> (
--     j := max C;
--     while true do (
--         if j < min C then (
-- 	    break -infinity 
-- 	    )
-- 	else (
-- 	    if C_j != 0 then (
-- 		break j 
-- 		)
-- 	    else (
-- 		j = j-1
-- 		)
-- 	    )
-- 	)
--     )


-- ----------------------------------------------------------------------------
-- -- grade
-- --
-- -- I a homogeneous ideal in a polynomial algebra
-- --
-- -- Returns the grade of I

-- grade = I -> (
--     - sup prune HH(dual res I)
--     )

-- ----------------------------------------------------------------------------
-- -- zeroIdeal
-- --
-- -- R a ring
-- --
-- -- Returns the zero ideal of R

-- zeroIdeal = R -> ideal (map(R^1,R^0,0)) 


--==========================================================================
-- DOCUMENTATION
--==========================================================================

beginDocumentation()

doc ///
  Key
    TorAlgebra
  Headline
    Classification of local rings based on multiplication in homology
  Description

    Text 
      Let $I$ be an ideal of a regular local ring $Q$ with residue
      field $k$. The minimal free resolution of $R=Q/I$ carries a
      structure of a differential graded algebra. If the length of the
      resolution, which is called the codepth of $R$, is at most $3$,
      then the induced algebra structure on Tor$_Q*$ ($R,k$) is unique
      and provides for a classification of such local rings.
      
      According to the multiplicative structure on Tor$_Q*$ ($R,k$), a
      non-zero local ring $R$ of codepth at most 3 belongs to exactly one of
      the (parametrized) classes designated {\bf B}, {\bf C}(c), {\bf G}(r), {\bf
      H}(p,q), {\bf S}, or {\bf T}. An overview of the theory can be
      found in L.L. Avramov, {\it A cohomological study of local rings
      of embedding codepth 3}, @HREF"http://arxiv.org/abs/1105.3991"@.

      There is a similar classification of Gorenstein local rings of codepth
      4, due to A.R. Kustin and M. Miller. There are four classes,
      which in the original paper, {\it Classification of the
      Tor-Algebras of Codimension Four Gorenstein Local rings}
      @HREF"https://doi.org/10.1007/BF01215134"@, are called A, B, C,
      and D, while in the survey {\it Homological asymptotics of
      modules over local rings}
      @HREF"https://doi.org/10.1007/978-1-4612-3660-3_3"@ by
      L.L. Avramov, they are called CI, GGO, GTE, and GH(p),
      respectively. Here we denote these classes {\bf C}(c), {\bf GS},
      {\bf GT}, and {\bf GH}(p), respectively.
  
      The package implements an algorithm for classification of local
      rings in the sense discussed above. For rings of codepth at most
      3 it is described in L.W. Christensen and O. Veliche, {\it Local
      rings of embedding codepth 3: a classification algorithm},
      @HREF"http://arxiv.org/abs/1402.4052"@. The classification of
      Gorenstein rings of codepth 4 is analogous. 
      
      The package also recognizes Golod rings, Gorenstein rings, and
      complete intersection rings of any codepth. To recognize Golod
      rings the package implements a test found in J. Burke, {\it Higher
      homotopies and Golod rings}
      @HREF"https://arxiv.org/abs/1508.03782"@. ///


doc ///
  Key
    torAlgData
  Headline
    invariants of a local ring and its class (w.r.t. multiplication in homology)
  Usage
    torAlgData R
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra  by an ideal contained in the irrelevant maximal ideal
  Outputs
      : HashTable
        a hash table with invariants of the local ring obtained by
  	localizing {\tt R} at the irrelevant maximal ideal
  Description
  
    Text 
      Computes invariants of the local ring obtained by localizing
      {\tt R} at the irrelevant maximal ideal and, provided that it
      has codepth at most 3, classifies it as belonging to one of the
      (parametrized) classes {\bf B}, {\bf C}(c), {\bf G}(r), {\bf
      H}(p,q), {\bf S}, or {\bf T}. Rings of higher codepth are
      classified as {\bf C}(c) (complete intersection), {\bf Gorenstein},
      {\bf Golod}, or {\tt no class}. Gorenstein rings of codepth 4 are further
      classified as belonging to one of the (parametrized) classes
      {\bf C}(4), {\bf GS}, {\bf GT}, or {\bf GH}(p). 
      
      Returns a hash table with the following data of the local ring:
  
      "c": codepth
      
      "e": embedding dimension
      
      "h": Cohen-Macaulay defect
      
      "m": minimal number of generators of defining ideal
      
      "n": type
      
      "Class": class ('B', 'C', 'G', 'GH', 'GS', 'GT', 'H', 'S', 'T',
      'Golod', 'Gorenstein' `zero ring', or 'no class')
      
      "p": classification parameter
      
      "q": classification parameter
      
      "r": classification parameter
      
      "isCI": boolean
      
      "isGorenstein": boolean
      
      "isGolod": boolean
      
      "PoincareSeries": Poincar\'e series in closed from (rational function)
      
      "BassSeries": Bass series in closed from (rational function)
      
    Example
      Q = QQ[x,y,z];
      data = torAlgData (Q/ideal (x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3))
      data#"PoincareSeries"

    Example
      Q = QQ[w,x,y,z];
      torAlgData (Q/ideal (w^2-x*y*z,x^3,y^3,x^2*z,y^2*z,z^3-x*y*w,x*z*w,y*z*w,z^2*w-x^2*y^2))

    Example
      Q = QQ[v,w,x,y,z];
      torAlgData (Q/(ideal(v^2-w^3)*ideal(v,w,x,y,z)))

    Example
      Q = QQ[u,v,w,x,y,z];
      torAlgData (Q/ideal (u^2,v^2,w^2-y^4,x^2,x*y^15))

    Text  
      To extract data from the hash table returned by the function one may use  
      @TO torAlgDataList@ and @TO torAlgDataPrint@.
       
  Caveat
      If the embedding dimension of {\tt R} is large, then the response time
      may be longer, in particular if {\tt R} is a quotient of a polynomial
      algebra over a small field. The reason is that the function attempts to
      reduce {\tt R} modulo a generic regular sequence of generators of the irrelevant
      maximal ideal. The total number of attempts made can be controlled with
      @TO setAttemptsAtGenericReduction@.
      
      If {\tt R} is a quotient of a polynomial algebra by a
      homogeneous ideal, then it is graded and the relevant invariants
      of the local ring obtained by localizing {\tt R} at the
      irrelevant maximal ideal can be determined directly from {\tt R}.
      If {\tt R} is a quotient of a polynomial algebra by a
      non-homogeneous ideal, then the function uses the package @TO
      LocalRings@ to compute some of the invariants.
///

doc ///
  Key
    torAlgClass
  Headline
    the class (w.r.t. multiplication in homology) of a local ring
  Usage
    torAlgClass R
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra by an ideal contained in the irrelevant maximal ideal
  Outputs
      : String
        the (parametrized) class of the local ring obtained by
        localizing {\tt R} at the irrelevant maximal ideal, provided
        that this ring is non-zero and of codepth at most 3 or Gorenstein 
	or Golod; otherwise "no class"
  Description
  
    Text 
      Classifies the local ring obtained by localizing {\tt R} at
      the irrelevant maximal ideal as belonging to one of the
      (parametrized) classes {\bf B}, {\bf C}(c), {\bf G}(r), {\bf H}(p,q),
      {\bf S}, or {\bf T}, provided that it is codepth at most 3.
      
    Example
      Q = QQ[x,y,z];
      torAlgClass Q
      torAlgClass (Q/ideal (x*y))
      torAlgClass (Q/ideal (x^2,y^2))
      torAlgClass (Q/ideal (x^2,y^2,x*y))
      torAlgClass (Q/ideal (x^2,x*y,y*z,z^2))
      torAlgClass (Q/ideal (x^2,y^2,z^2))      
      torAlgClass (Q/ideal (x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3))
      torAlgClass (Q/ideal (x*z+y*z,x*y+y*z,x^2-y*z,y*z^2+z^3,y^3-z^3))
      torAlgClass (Q/ideal (x^2,y^2,z^2,x*y))
      torAlgClass (Q/ideal (x^2,y^2,z^2,x*y*z))
      
    Text  
      If the local ring is Gorenstein or Golod of codepth 4, then it is classified
      as belonging to one of the (parametrized) classes {\bf C}(4), {\bf GH}(p), 
      {\bf GS}, {\bf GT}, or {\bf codepth 4 Golod}.
      
    Example
      Q = QQ[w,x,y,z];
      torAlgClass (Q/ideal (w^2,x^2,y^2,z^2))
      torAlgClass (Q/ideal (y*z,x*z,x*y+z^2,x^2,w*x+y^2+z^2,w^2+w*y+y^2+z^2))
      torAlgClass (Q/ideal (z^2,x*z,w*z+y*z,y^2,x*y,w*y,x^2,w*x+y*z,w^2+y*z))
      torAlgClass (Q/ideal (x^2,y^2,z^2,x*w,y*w,z*w,w^3-x*y*z))
      torAlgClass (Q/(ideal (w,x,y,z))^2)

    Text	  
      If the local ring has codepth at least 5, then it is classified as belonging
      to one of the classes {\bf C}(c), if it is complete intersection, {\bf codepth c Gorenstein}, 
      if it is Gorenstein and not complete intersection, {\bf codepth c Golod}, if it is Golod,
      and {\tt no class} otherwise.
            
    Example
      Q = QQ[u,v,w,x,y,z];
      torAlgClass (Q/ideal (u^2,v^2,w^2,x^2+y^2, x^2+z^2))
      torAlgClass (Q/ideal (w^2,v*w,z*w,y*w,v^2,z*v+x*w,y*v,x*v,z^2+x*w,y*z,x*z,y^2+x*w,x*y,x^2))
      torAlgClass (Q/ideal (x^2*y^2,x^2*z,y^2*z,u^2*z,v^2*z,w^2*z))
      torAlgClass (Q/ideal (u^2,v^2,w^2,x^2,z^2,x*y^15))
      
    Text  
      If the defining ideal of {\tt R} is not contained in the irrelevant maximal ideal, 
      then the resulting local ring is zero, and the function returns {\tt zero ring}.
      
    Example
      Q = QQ[x,y,z];
      torAlgClass (Q/ideal (x^2-1))
///

doc ///
  Key
    isCI
  Headline
    whether the ring is complete intersection
  Usage
    isCI R
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra by an ideal contained in the irrelevant maximal ideal
  Outputs
      : Boolean
        whether the local ring obtained by localizing {\tt R} at the irrelevant maximal ideal is complete intersection

  Description
  
    Text 
      Checks if the local ring obtained by localizing {\tt R} at the
      irrelevant maximal ideal is complete intersection.
      
    Example
      Q = QQ[x,y,z];
      isCI (Q/ideal (x^2,x*y,y*z,z^2))
      isCI (Q/ideal (x^2,y^2))
///

doc ///
  Key
    isGorenstein
  Headline
    whether the ring Gorenstein
  Usage
    isGorenstein R
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra by an ideal contained in the irrelevant maximal ideal
  Outputs
      : Boolean
        whether the local ring obtained by
        localizing {\tt R} at the irrelevant maximal ideal is Gorenstein
	
  Description
  
    Text 
      Checks if the local ring obtained by localizing {\tt R} at the
      irrelevant maximal ideal is Gorenstein.
      
    Example
      Q = QQ[x,y,z];
      isGorenstein (Q/ideal (x^2,x*y,y*z,z^2))
      isGorenstein (Q/ideal (x^2,y^2))
      isGorenstein (Q/ideal (x*z+y*z,x*y+y*z,x^2-y*z,y*z^2+z^3,y^3-z^3))
///

doc ///
  Key
    isGolod
  Headline
    whether the ring is Golod
  Usage
    isGolod R
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra by an ideal contained in the irrelevant maximal ideal
  Outputs
      : Boolean
        whether the local ring obtained by
        localizing {\tt R} at the irrelevant maximal ideal is Golod
	
  Description
  
    Text 
      Checks if the local ring obtained by localizing {\tt R} at the
      irrelevant maximal ideal is Golod
      
    Example
      Q = QQ[x,y,z];
      isGolod (Q/ideal (x^2,x*y,y*z,z^2))
      isGolod (Q/ideal (x^2))
      isGolod (Q/(ideal (x,y,z))^2)      
///

doc ///
  Key
    torAlgDataList
  Headline
    list invariants of a local ring
  Usage
    torAlgDataList(R,L)
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra  by an ideal contained in the irrelevant maximal ideal
    L : List
        a list of keys from the hash table returned by @TO torAlgData@
	
  Outputs
      : List
        the list of values corresponding to the keys specified in {\tt L}
	
  Description
    Text 
      Extracts data from the hash table returned by @TO torAlgData@.

    Example
      Q = QQ[x,y,z];
      R = Q/ideal (x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3);
      torAlgDataList( R, {m, n, Class, p, q, r, PoincareSeries, BassSeries} )            
///      
      
doc ///
  Key
    torAlgDataPrint
  Headline
    print invariants of a local ring
  Usage
    torAlgDataPrint (R,L)
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra  by an ideal contained in the irrelevant maximal ideal
    L : List 
        a list of keys from the hash table returned by @TO torAlgData@    
  Outputs
      : String
        the string of keys specified in {\tt L} together with their values
  Description
    Text 
       Extracts data from the hash table returned by @TO torAlgData@.

    Example
      Q = QQ[x,y,z];
      R = Q/ideal (x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3);
      torAlgDataPrint( R, {c, e, h, m, n, Class, p, q, r} )      
     
///

doc ///
  Key
    setAttemptsAtGenericReduction
  Headline
    control the number of attempts to compute Bass numbers via a generic reduction
  Usage
    setAttemptsAtGenericReduction(R,n)
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra by an ideal contained in the irrelevant maximal ideal
    n : ZZ
        a positive integer
  Outputs
      : ZZ
        the number of attempts that will be made to perform a generic reduction to compute the Bass 
	numbers of the local ring obtained by localizing {\tt R} at the irrelevant maximal ideal
  Description
  
    Text 
      Changes the number of attempts made to reduce {\tt R} modulo a generic regular sequence of
      generators of the irrelevant maximal ideal in order to compute the Bass numbers of the
      local ring obtained by localizing {\tt R} at the irrelevant maximal ideal. The function has
      the effect of setting {\tt R.attemptsAtGenericReduction = n}, and the number of attempts made is 
      at most {\tt n^2}. If {\tt R.attemptsAtGenericReduction} is not set, then at most 625 attempts
      are made.
      
    Example
      Q = ZZ/2[u,v,w,x,y,z];
      R = Q/ideal (x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3);
      R.?attemptsAtGenericReduction
      setAttemptsAtGenericReduction(R,100)
      R.attemptsAtGenericReduction
      
    Text 
      If the value of {\tt R.attemptsAtGenericReduction} is too small, then the computation of Bass
      numbers may fail resulting in an error message. Notice, though, that if the local ring obtained     
      by localizing {\tt R} at the irrelevant maximal ideal has embedding dimension at most 3, then the 
      Bass numbers are computed without any attempt to reduce the ring, and {\tt R.attemptsAtGenericReduction}
      has no significance.

    Example
      Q = ZZ/2[x,y,z];
      R = Q/ideal (x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3);
      setAttemptsAtGenericReduction(R,0)
      torAlgClass R
///

doc ///
  Key
    attemptsAtGenericReduction
  Headline
    see setAttemptsAtGenericReduction
  Description
  
    Text 
      See @TO setAttemptsAtGenericReduction@

///


--===================================================================================================
-- TESTS
--===================================================================================================

-- #0 zero ring, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( promote(1,Q) )
assert( torAlgClass(Q/I) === "zero ring" )
///

-- #1 zero ring, local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( x^2-1 )
assert( torAlgClass(Q/I) === "zero ring" )
///

-- #2 C(0), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(u+v+w+x+y+z)
assert( torAlgClass(Q/I) === "C(0)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 0, 0, 0, 1, "C", 0, 0, 0} )
///

-- #3 C(0), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x-y^2-z^7+u+v+w)
assert( torAlgClass(Q/I) === "C(0)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert(L === {5, 0, 0, 0, 1, "C", 0, 0, 0} )
///

-- #4 C(1), graded
TEST ///
Q = QQ[x]
I = ideal(x^2)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {1, 1, 0, 1, 1, "C", 0, 0, 0} )
///

-- #5 C(1), local
TEST ///
Q = QQ[x]
I = ideal(x^2-x^3)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert(L === {1, 1, 0, 1, 1, "C", 0, 0, 0} )
///

-- #6 C(1), graded
TEST ///
Q = ZZ/53[x,y]
I = ideal(x*y)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {2, 1, 0, 1, 1, "C", 0, 0, 0} )
///

-- #7 C(1), local
TEST ///
Q = ZZ/53[x,y]
I = ideal(x*y-x^3)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert(L === {2, 1, 0, 1, 1, "C", 0, 0, 0} )
///

-- #8 C(1), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x*y,x+y+z,u+v+w)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 1, 0, 1, 1, "C", 0, 0, 0} )
///

-- #9 C(1), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2-y^3+z^3)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 1, 0, 1, 1, "C", 0, 0, 0} )
///

-- #10 C(2), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x*y,u*v,x+y+z+u+v+w)
assert( torAlgClass(Q/I) === "C(2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 2, 0, 2, 1, "C", 1, 0, 0} )
///

-- #11 C(2), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x*y,u*v,z^2-w)
assert( torAlgClass(Q/I) === "C(2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 2, 0, 2, 1, "C", 1, 0, 0} ) 
///

-- #12 S, graded
TEST ///
Q = QQ[x,y]
I = ideal(x^2,x*y)
assert( torAlgClass(Q/I) === "S" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {2, 2, 1, 2, 1, "S", 0, 0, 0} )
///

-- #13 S, local
TEST ///
Q = QQ[x,y]
I = ideal((x+y^2)^2,(x+y^2)*y)
assert( torAlgClass(Q/I) === "S" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert(L === {2, 2, 1, 2, 1, "S", 0, 0, 0} )
///

-- #14 S, graded
TEST ///
Q = ZZ/53[u,v,w,x,y,z]
I = ideal(x^2,x*y,y*z,x+y+z+u+v+w)
assert( torAlgClass(Q/I) === "S" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 2, 0, 3, 2, "S", 0, 0, 0} )
///

-- #15 S, local
TEST ///
Q = ZZ/53[u,v,w,x,y,z]
I = ideal(x^2*y-y^2,x^3-x*y)
assert( torAlgClass(Q/I) === "S" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 2, 1, 2, 1, "S", 0, 0, 0} ) 
///

-- #16 B, graded
TEST ///
setRandomSeed "TorAlgebra";
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,x*y,z^2,y*z,x+y+z+u+v+w)
assert( torAlgClass(Q/I) === "B" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 3, 1, 4, 1, "B", 1, 1, 2} )
///

-- #17 B, local
TEST ///
setRandomSeed "TorAlgebra";
Q = QQ[u,v,w,x,y,z]
I = ideal(x^4-z*w^2,y^3-x^2*w,z^3-x*y,w^3-x*y^2*z^2,z^2*x^3-y*w^2,u,v)
assert( torAlgClass(Q/I) === "B" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 3, 0, 5, 2, "B", 1, 1, 2} ) 
///

-- #18 C(3), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal (u*v,w*x,y*z)
assert( torAlgClass(Q/I) === "C(3)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 3, 0, 3, 1, "C", 3, 1, 3} ) 
///

-- #19 C(3), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(u*v,w*(x+w^2),y*z)
assert( torAlgClass(Q/I) === "C(3)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 3, 0, 3, 1, "C", 3, 1, 3} )
///

-- #20 G(7) Gorenstein, graded
TEST ///
Q = QQ[x,y,z]
I = ideal(x^3,x^2*z,x*(z^2+x*y),z^3-2*x*y*z,y*(z^2+x*y),y^2*z,y^3)
assert( torAlgClass(Q/I) === "G(7), Gorenstein" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {3, 3, 0, 7, 1, "G", 0, 1, 7} ) 
///

-- #21 G(7) Gorenstein, local
TEST ///
Q = QQ[x,y,z]
I = ideal(x^6,x^4*z,x^2*(z^2+x^2*y),z^3-2*x^2*y*z,y*(z^2+x^2*y),y^2*z,y^3)
assert( torAlgClass(Q/I) === "G(7), Gorenstein" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {3, 3, 0, 7, 1, "G", 0, 1, 7} )
///

-- #22 G(2), graded
TEST ///
setRandomSeed "TorAlgebra";
Q = QQ[u,v,w,x,y,z]
I = ideal(x*y^2,x*y*z,y*z^2,x^4-y^3*z,x*z^3-y^4)
assert( torAlgClass(Q/I) === "G(2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 3, 1, 5, 2, "G", 0, 1, 2} ) 
///

-- #23 G(4), local
TEST ///
setRandomSeed "TorAlgebra";
Q = QQ[u,v,w,x,y,z]
I = ideal (x^6,x^2*(z^2+x^2*y),z^3-2*x^2*y*z,y*(z^2+x^2*y),y^2*z,y^3)+x^4*z*ideal(x,y,z)
assert( torAlgClass(Q/I) === "G(4)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L ===  {6, 3, 0, 7, 2, "G", 0, 1, 4} )
///

-- #24 H(0,0), graded
TEST ///
Q = QQ[x,y,z]
I = ideal(x^2,x*y^2)*ideal(y*z,x*z,z^2)  
assert( torAlgClass(Q/I) === "H(0,0)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {3, 3, 2, 5, 2, "H", 0, 0, 0} ) 
///

-- #25 H(0,0), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,x*(y+v^2)^2)*ideal((y+v^2)*z,x*z,z^2)
assert( torAlgClass(Q/I) === "H(0,0)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L ===  {6, 3, 2, 5, 2, "H", 0, 0, 0} )
///

-- #26 H(3,2), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*y,x+u+z,y+v+w) 
assert( torAlgClass(Q/I) === "H(3,2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 3, 0, 4, 2, "H", 3, 2, 2} ) 
///

-- #27 H(3,2), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,(y+w^2)^2,z^2,x*(y+w^2))
assert( torAlgClass(Q/I) === "H(3,2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L ===  {6, 3, 0, 4, 2, "H", 3, 2, 2} )
///

-- #28 T, graded
TEST ///
setRandomSeed "TorAlgebra";
Q = QQ[u,v,w,x,y,z]
I = ideal(x^3,y^3,z^3,x^2*y*z,x+y+x+u+v+w)
assert( torAlgClass(Q/I) === "T" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L ===  {5, 3, 0, 4, 3, "T", 3, 0, 0} )
///

-- #29 T, local
TEST ///
setRandomSeed "TorAlgebra";
Q = QQ[u,v,w,x,y,z]
I = ideal((x+u^2)^2,y^2,z^3,(x+u^2)*y*z^2) 
assert( torAlgClass(Q/I) === "T" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 3, 0, 4, 3, "T", 3, 0, 0} ) 
///

-- #30 C(4), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( u^2,v^2,w^2,x^2,y+z )
assert( torAlgClass(Q/I) === "C(4)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 4, 0, 4, 1, "C", 6, 4, 4} ) 
///

-- #31 C(4), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( u^2-v^3,z^6,w^2,x^2,y )
assert( torAlgClass(Q/I) === "C(4)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 4, 0, 4, 1, "C", 6, 4, 4} ) 
///

-- #32 GT, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,u*w^2-x*y*z)
assert( torAlgClass(Q/I) === "GT" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 4, 0, 7, 1, "GT", 3, 3, 7} ) 
///

-- #33 GT, local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,w^2-x*y*z)
assert( torAlgClass(Q/I) === "GT" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 4, 0, 7, 1, "GT", 3, 3, 7} ) 
///

-- #34, GH(5) graded
TEST ///
setRandomSeed "TorAlgebra"
Q = QQ[w,x,y,z]
I = ideal fromDual(matrix random(3,Q))
assert( torAlgClass(Q/I) === "GH(5)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 4, 0, 6, 1, "GH", 5, 6, 6} ) 
///

-- #35 GH(2), local,
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(w^2-x*y*z,x^3,y^3,x^2*z,y^2*z,z^3-x*y*w,x*z*w,y*z*w,z^2*w-x^2*y^2)
assert( torAlgClass(Q/I) === "GH(2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 4, 0, 9, 1, "GH", 2, 3, 9} ) 
///

-- #36 GS, graded
TEST ///
setRandomSeed "TorAlgebra"
Q = QQ[w,x,y,z]
I = ideal fromDual(matrix random(2,Q))
assert( torAlgClass(Q/I) === "GS" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 4, 0, 9, 1, "GS", 0, 0, 9} ) 
///

-- #37 codepth 4 Golod, graded
TEST ///
Q = QQ[w,x,y,z]
I = ideal(w^2,x^2,y^2)*ideal(y^2,z^2)
assert( torAlgClass(Q/I) === "codepth 4 Golod" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 4, 2, 6, 1, "Golod", 0, 0, "-"} ) 
///

-- #38 codepth 4 Golod, local
TEST ///
Q = QQ[w,x,y,z]
I = ideal(w^2-y^3,x^2,y^2)*ideal(y^2,z^2)
assert( torAlgClass(Q/I) === "codepth 4 Golod" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 4, 2, 6, 1, "Golod", 0, 0, "-"} ) 
///

-- #39 codepth 4 no class, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,w^2)
assert( torAlgClass(Q/I) === "codepth 4 no class" )
///

-- #40 codepth 4 no class, local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,u+x*w,y*w,z*w,w^2)
assert( torAlgClass(Q/I) === "codepth 4 no class" )
///

-- #41 C(5), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( u^2,v^2,w^2,x^2,y^2 )
assert( torAlgClass(Q/I) === "C(5)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 5, 0, 5, 1, "C", 10, 10, 5} ) 
///

-- #42 C(5), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( u^2-v^3,z^6,w^2,x^2,y^3 )
assert( torAlgClass(Q/I) === "C(5)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 5, 0, 5, 1, "C", 10, 10, 5} ) 
///

-- #43 codepth 5 Gorenstein, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,w^3-x*y*z,v^2)
assert( torAlgClass(Q/I) === "codepth 5 Gorenstein" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 5, 0, 8, 1, "Gorenstein", "UNDETERMINED", "UNDETERMINED", 8} ) 
///

-- #44 codepth 5 Gorenstein, local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,w^2-x*y*z,v^2)
assert( torAlgClass(Q/I) === "codepth 5 Gorenstein" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 5, 0, 8, 1, "Gorenstein", "UNDETERMINED", "UNDETERMINED", 8} ) 
///

-- #45 codepth 5 Golod, graded
TEST ///
Q = QQ[v,w,x,y,z]
I = ideal(w^2,x^2,y^2)*ideal(y^2,z^2,v^3-x*y*z)
assert( torAlgClass(Q/I) === "codepth 5 Golod" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 5, 2, 9, 1, "Golod", 0, 0, "-"} ) 
///

-- #46 codepth 5 Golod, local
TEST ///
Q = QQ[v,w,x,y,z]
I = ideal(v,w,x,y,z)*ideal(z^2-x*y*v)
assert( torAlgClass(Q/I) === "codepth 5 Golod" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 5, 4, 5, 1, "Golod", 0, 0, "-"} ) 
///

-- #47 codepth 5 no class, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,w^2,v^2)
assert( torAlgClass(Q/I) === "codepth 5 no class" )
///

-- #48 codepth 5 no class, local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,w^2-x*y*z*v,v^2)
assert( torAlgClass(Q/I) === "codepth 5 no class" )
///

-- #49 codepth 6 no class, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal (u^2,v^2,w^2,x^2,x*y^15,w*z^4)
assert( torAlgClass(Q/I) === "codepth 6 no class" )
///

-- #50 C(6), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal (u^2,v^2,w^2,x^2,y^2,x*y+z^2)
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 6, 0, 6, 1, "C", 15, 20, 6} )
///

-- #51 Codepth 6 Gorenstein, graded
TEST ///
Q = ZZ/53[u,v,w,x,y,z]
I = ideal fromDual(matrix random(3,Q))
L = torAlgDataList(Q/I,{e, c, h, m, n, Class})
assert( L === {6, 6, 0, 15, 1, "Gorenstein"} )
///

-- #52 Codepth 6 Golod, graded
TEST ///
Q = ZZ/53[u,v,w,x,y,z]
I = ideal(x^2*y^2,x^2*z,y^2*z,u^2*z,v^2*z,w^2*z,z^2)
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 6, 4, 7, 1, "Golod", 0, 0, "-"} )
///

end



