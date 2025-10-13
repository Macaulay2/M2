-- -*- coding: utf-8 -*-
newPackage(
    	  "PseudomonomialPrimaryDecomposition",
	  Headline => "Primary decomposition of square free pseudomonomial ideals",
   	  Version => "0.3",
	  Date => "January, 2022",
	  Authors => {{
		    Name => "Alan A. Veliz-Cuba",
		    Email => "avelizcuba1@udayton.edu",
		    HomePage => "https://sites.google.com/site/alanvelizcuba/"
		    }},
	  DebuggingMode => false,
	  Keywords => {"Commutative Algebra"},
	  Certification => {
	       "journal name" => "The Journal of Software for Algebra and Geometry",
	       "journal URI" => "https://msp.org/jsag/",
	       "article title" => "Primary decomposition of squarefree pseudomonomial ideals",
	       "acceptance date" => "18 July 2022",
	       "published article URI" => "https://msp.org/jsag/2022/12-1/p04.xhtml",
	       "published article DOI" => "10.2140/jsag.2022.12.27",
	       "published code URI" => "https://msp.org/jsag/2022/12-1/jsag-v12-n1-x04-PseudomonomialPrimaryDecomposition.m2",
	       "release at publication" => "4c86bb7c1f80a36c5e2ce6786863f5702c13ddda",	    -- git commit number in hex
	       "version at publication" => "0.3",
	       "volume number" => "12",
	       "volume URI" => "https://msp.org/jsag/2022/12-1/"
	       }
	  )


export({"isSquarefreePseudomonomialIdeal", "primaryDecompositionPseudomonomial","isPseudomonomial"})


-------------------------------------------------------------------
-- subroutines to translate between polynomials and bitwise form --
-------------------------------------------------------------------


-- obtains the generators of a ring in which an ideal lives
-- Input:
-- an ideal I
-- Output:
-- A list of generators of R
getAllVariables = I -> gens ring I;


-- define the zero polynomial in bitwise form
-- this is added only for clarity in the algorithm
zeropolynomial := {-1,-1};


-- transforms a square free pseudomonomial to bitwise string...
-- for example, x1*(x3-1)*x4 will be converted to {1001,0100} = {9,4}
-- Input:
-- Square free pseudomonomial P in polynomial form
-- Output:
-- A list of the form {num1,num2} (so-called bitwise form of P)
fromPoly2Bit = (P,allVariables) -> ( 
    if P==0 then return zeropolynomial;
    factoredP := factor P; 
    -- initialize bitwise form {0,0}
    bitwisePOS := 0; 
    bitwiseNEG := 0;
    -- add factors to bitwise form
    for i to #factoredP-1 do ( 
        -- evaluate ith factor and skip units
        base := value factoredP#i; 
        if isUnit base then continue;
        -- find variable of ith factor and position in allVariables
        xi := (support base)_0; 
        pos := position(allVariables,x -> x === xi);
        -- make position of variable equal to 1 in bitwise format (adding 2^pos)
        if   xi === base then bitwisePOS = bitwisePOS | (1 << pos);
        if xi-1 === base then bitwiseNEG = bitwiseNEG | (1 << pos);
    );
    {bitwisePOS,bitwiseNEG}
)


-- finds log in base 2
-- Input:
-- a number m
-- Output:
-- a natural number
-- log2 = m -> round(log_2 m); -- OLD CODE
log2 = m -> (
    ans := 0;
    while (1 << ans) < m do ans = ans+1;
    ans
)


-- transforms a polynomial in bitwise form to polynomial form
-- Input:
-- (B,allVariables), B must be of the form {2^(k-1),0} or {0,2^(k-1)}
-- Output:
-- polynomial polyB of the form xk or xk-1
fromBit2Poly = (B,allVariables) -> ( 
    polyB := 1;
    if B_0 != 0 then polyB = allVariables_(log2(B_0));
    if B_1 != 0 then polyB = allVariables_(log2(B_1))-1;
    polyB
)



------------------------------------------------------------------------------
-- subroutines for intermediate steps of primary decomposition ---------------
-- NOTE: In ALL subroutines here the polynomial inputs are in bitwise form ---
------------------------------------------------------------------------------


-- determines if a square free pseudomonomial divides another
-- Input:
-- Two square free pseudomonomials in bitwise form B1, B2
-- Output:
-- true or false
isDivisor = (B1,B2) -> (
    -- we use the fact that P1|P2 if and only if
    -- product of factors xi in P1 divides the product of factors xi in P2 
    -- and
    -- product of factors xi-1 in P1 divides the product of factors xi-1 in P2.
    -- This is equivalent to B1_0<=B2_0 and B1_1<=B2_1 (bitwise)
    -- So we use the Boolean rule a<=b iff a&b=a (bitwise)
    (B1_0)&(B2_0) == B1_0 and (B1_1)&(B2_1) == B1_1
)


-- sets zi=0 in a square free pseudomonomial
-- Input:
-- A square free pseudomonomial ideal in bitwise form, and zi=xi-0/1 in bitwise form
-- Output:
-- A square free pseudomonomial ideal G where zi was set to 0 and zi-1 was set to 1 (in bitwise form)
---- Uses the functions:
---- isDivisor
---- Uses the constants
---- zeropolynomial
setEqualtoZero = (G,zi) -> (
    apply(G, B -> (   
        -- if zi divides B then zi=0 makes B=0 
        if isDivisor(zi,B) then return zeropolynomial;
        -- if zi-1 divides B then zi=0 removes zi-1 from B
        if (zi_0)&(B_1) == (zi_0) and 0 != zi_0 then return {B_0,B_1-zi_0};
        if (zi_1)&(B_0) == (zi_1) and 0 != zi_1 then return {B_0-zi_1,B_1};
        -- if neither zi nor zi-1 divide B then B is unchanged
        return B;  
    ))
)


-- finds the minimal elements according to a partial order
-- Input:
-- A list {G,ord}
-- G is a list and ord is a partial order
-- Output:
-- A set, minG
findMinimal = (G,ord) -> (
    minG := {};
    for i to #G-1 do (
        -- if g<=Gi for some g in minG then Gi cannot be minimal
        if any(minG, g-> ord(g,G_i)) then continue; 
        -- if g<!=Gi for all g in minG then Gi may be minimal
        -- and we remove from minG all g's such that Gi<=g 
        minG = select(minG, g -> not ord(G_i,g));
        minG = append(minG, G_i);
    );
    minG
)


-- removes redundant generators in the ideal <G>
-- Input:
-- list G of square free pseudomonomials in bitwise form
-- Output:
-- list with redundant generators removed in bitwise form
---- Uses the functions
---- findMinimal
---- isDivisor
---- Uses the constants
---- zeropolynomial
simplifyGens = G -> (
    -- remove zero polynomials and polynomials that are multiples of others
    redG := select(G,B -> B != zeropolynomial);
    findMinimal(unique redG,isDivisor)
)


-- determines if square free pseudomonomial is linear nonconstant ( zi=xi-0/1)
-- Input:
-- Square free pseudomonomial in bitwise form
-- Output:
-- true or false
isLinearPseudomonomial = B-> (
    -- if polynomial is 1 then false
    -- (B={0,0} in bitwise form)
    if (B_0) == 0 and (B_1) == 0 then return false;
    -- if polynomial is xi or xi-1 then true
    --  ({2^i,0} or {0,2^i} in bitwise form)
    if (B_0)&(-1+B_0) == 0 and B_1 == 0 then return true;
    if (B_1)&(-1+B_1) == 0 and B_0 == 0 then return true;
    false
)


-- finds all the factors of a square free pseudomonomial P (in bitwise form B)
-- Input:
-- Square free pseudomonomial in bitwise form
-- Output:
-- A list of square free pseudomonomials, each in bitwise form
getFactors = B -> (
    allfactors := {}; 
    leftoverB := B; 
    i := 0;
    while (leftoverB_0) != 0 or (leftoverB_1) != 0 do (
        -- check if xi or xi-1 is a factor
        pi := 1 << i;
        -- checking if xi is a factor (using bitwise form)
        if (leftoverB_0)&pi == pi then (
            allfactors = append(allfactors,{pi,0});
            leftoverB = {leftoverB_0-pi,leftoverB_1};
        );  
        -- checking if xi-1 is a factor (using bitwise form)  
        if (leftoverB_1)&pi == pi then (
            allfactors = append(allfactors,{0,pi});
            leftoverB = {leftoverB_0,leftoverB_1-pi};
        ) ;
        i = i+1;
    );
    allfactors
)


-- determines if a square free pseudomonomial ideal is generated by linear polynomials
-- Input:
-- list of generators of a square free pseudomonomial ideal in bitwise form
-- Output:
-- true or false
---- Uses the functions:
---- isLinearPseudomonomial
isPrimaryPseudomonomial = G -> (
    not any(G, B -> not isLinearPseudomonomial B)
    -- if any(G, B -> not isLinearPseudomonomial B) then return false;
    -- true
)


-- determines if a square free pseudomonomial ideal has gens xi and xi-1
-- Input:
-- list of generators of a square free pseudomonomial ideal in bitwise form
-- Output:
-- true or false
---- Uses the functions:
---- isLinearPseudomonomial
isProperPseudomonomial = G -> (
    if G == {{0,0}} then return false;
    -- look for generators xi and xi-1
    -- ({2^i,0} and {0,2^i} in bitwise form)
    for i to #G-1 do (
        if isLinearPseudomonomial(G_i) then (
            for j from i+1 to #G-1 do (
                if isLinearPseudomonomial(G_j) then (
                    if (G_i)_0 == (G_j)_1 and (G_i)_1 == (G_j)_0 then return false;
                )
            )
        )
    );
    true
)


-- writes a square free pseudomonomial ideal as an intersection
-- Input:
-- list G of generators of I in bitwise form
-- Output:
-- a list G1,...,Gm of polynomials (in bitwise form) where the intersection(G1,...,Gm)=I 
---- Uses the functions:
---- isLinearPseudomonomial
---- getFactors
---- setEqualtoZero
factorIdeal = G -> (
    --check if there is a polynomial of degree 2 or higher
    nonlinearpolynomials := select(1,G, B -> not isLinearPseudomonomial B);
    if nonlinearpolynomials == {} then return {G};
    -- use square free pseudomonomial of degree>=2: z1z2...zk
    B := nonlinearpolynomials_0;
    allfactors := getFactors(B);
    -- find  and simplify <G>=<G,z1>inter...inter<G,zk> 
    apply(allfactors, Bi -> append(setEqualtoZero(G,Bi),Bi))
)


-- finds the primary decomposition of a square free pseudomonomial ideal in bitwise form
-- Input:
-- list of generators of I in bitwise form
-- Output:
-- list of generators of primary ideals in bitwise form
---- Uses the following functions
---- isProperPseudomonomial
---- factorIdeal
---- simplifyGens
---- isPrimaryPseudomonomial
---- findMinimal
bitwisePD = G -> (    
    -- first check if ideal has generators xi and xi-1
    if not isProperPseudomonomial G then return {};
    -- initialize primary decomposition (P) and remaining ideals in the intersection (D)
    P := {}; 
    D := {G};
    while D != {} do (
        -- write <G>=<G,z1>inter...inter<G,zk>
        -- for each G in D
        DI := apply(D, GI -> (factorIdeal GI)/simplifyGens );
        -- combine all intersections into a single intersection
        ---- List + List := (X,Y) -> join(X,Y); -- old line 1
        ---- D = sum DI;                       -- old line 2
        --D = {}; scan(DI,GI -> D = join(D,GI));
        D = flatten DI;
        -- omit ideals that are generated by the polynomial 1
        D = select(D, GI -> isProperPseudomonomial GI);
        -- if ideal is primary save in G, if not primary save in D
        P = join(P, select(D, GI -> isPrimaryPseudomonomial GI) );
        D = select(D, GI -> not isPrimaryPseudomonomial GI);
    );
    findMinimal(P,isSubset)
)


-- determines if a polynomial is square free pseudomonomial
-- Input:
-- Polynomial P in bitwise form
-- Output:
-- true or false
isPseudomonomial = P -> ( 
    -- check if polynomial is a unit or zero
    if P == 0 then return false;
    if isUnit P then return true;
    -- factor polynomial P and initialize the support list
    factoredP := factor P;
    allSupport := {};
    -- test if some factor is not of the form (xi-a) where a=0 or 1
    for i to #factoredP-1 do ( 
        -- evaluate ith factor
        base := value factoredP#i; 
        -- if factor is not a unit but is a constant -> not a square free pseudomonomial
        if isUnit base then continue;
        if isConstant base then return false;
        -- find if factor is equal to xi or xi-1
        suppi := support base;
        if #suppi >= 2 then return false;
        if suppi_0 =!= base and suppi_0-1 =!= base then return false;
        allSupport = append(allSupport,suppi_0);
    );
    -- find if there are factors xi, xi-1 simultaneously -> not a square free pseudomonomial
    #(support P) == #allSupport
    -- if #(support P) != #allSupport then return false;
    -- true
)



--------------------------------
-- exported functions --
--------------------------------

-- determines if an ideal is square free pseudomonomial
-- Input:
-- Ideal I in polynomial form
-- Output:
-- true or false
---- Uses the functions:
---- isPseudomonomial
isSquarefreePseudomonomialIdeal = method()
isSquarefreePseudomonomialIdeal Ideal := Boolean => I -> (
    -- remove zero polynomials
    allgens := select(I_*, P -> P != 0); 
    -- look for polynomials that are not square free pseudomonomials
    --for i to #allgens-1 do (
    --    if not isPseudomonomial(allgens_i) then return false;
    --);
    --true
    all(allgens, isPseudomonomial)   
)


-- computes the primary decomposition of a square free pseudomonomial ideal
-- Input:
-- Square free pseudomonomial ideal in polynomial form
-- Output:
-- list of primary ideals in polynomial form
---- Uses the following functions:
---- isSquarefreePseudomonomialIdeal
---- fromPoly2Bit
---- isDivisor
---- findMinimal
---- bitwisePD
---- fromBit2Poly
---- getAllVariables
primaryDecompositionPseudomonomial = method()
primaryDecompositionPseudomonomial Ideal := I -> (
    -- looking for errors
    -- if class I =!= Ideal then error "Input must be a square free pseudomonomial ideal.";
    if not isSquarefreePseudomonomialIdeal(I) then error "Not a square free pseudomonomial ideal.";
    -- finding nonzero generators of ideal
    gensI := select (I_*, P -> P != 0); 
    -- if zero ideal just return trivial primary decomposition
    if gensI == {} then return {I};
    -- transform to bitwise form
    allVariables := getAllVariables I;
    G := apply(gensI, P -> fromPoly2Bit(P,allVariables) );
    -- remove redundant generators
    G = findMinimal(G, isDivisor);
    -- find primary decomposition using bitwise notation
    PD := bitwisePD G;
    -- go back to polynomial notation
    -- print allVariables;
    -- PDpolyform := apply( PD, PDi -> fromBit2Poly(PDi,allVariables) );
    apply( PD, PDi -> ideal apply(PDi, P-> fromBit2Poly(P,allVariables) ))
)



-------------------
-- Documentation --
-------------------

beginDocumentation()

doc ///
Key
  PseudomonomialPrimaryDecomposition
Headline
  primary decomposition of a square free pseudomonomial ideal
Description
  Text
    A pseudomonomial is a polynomial in K[x1,x2,...,xn] that can be written as a product of factors of the form (xi-ai)^ni, where ai is 0 or 1.  The xi's in the product should be distinct. A square free pseudomonomial ideal is an ideal generated by pseudomonomials such that each ni=1.

    This package finds the primary decomposition of square free pseudomonomial ideals. It also determines if an ideal is a pseudomonomial ideal.

    For example, x1^2*(x3-1) is a pseudomonomial, but not square free. The polynomial x1*(x3-1) is a square free pseudomonomial. The ideal ideal(x1*(x3-1),(x1-1)*(x2-1)*x4,x1*x2*x3,(x1-1)*x2*(x5-1)) is a square free pseudomonomial ideal.
  Example
    R = ZZ/2[x1,x2,x3,x4,x5];
    I = ideal(x1*(x3-1),(x1-1)*(x2-1)*x4,x1*x2*x3,(x1-1)*x2*(x5-1));
    isSquarefreePseudomonomialIdeal I
    C = primaryDecompositionPseudomonomial I
    intersect C == I    
///

document {
     Key => {primaryDecompositionPseudomonomial, (primaryDecompositionPseudomonomial, Ideal)},
     Headline => "primary decomposition of a square free pseudomonomial ideal",
     SeeAlso => {"Macaulay2Doc::primaryDecomposition"},
     Usage => "primaryDecompositionPseudomonomial(I)",
     Inputs => { 
	  "I" => {ofClass Ideal, ", a square free pseudomonomial ideal"}
	  },
     Outputs => {
	  "L" => {ofClass List, ", list L={P1,P2,...,Pk} of primary ideals with intersection equal to I"}
	  },
     Caveat => {
	  "The algorithm finds a decomposition even if the base field is not QQ or ZZ/p"
	  },
      "The algorithm is implemented bitwise (using Macaulay2 bitwise operations), which makes calculations faster.",
     " Examples:",
     EXAMPLE lines ///
          R=ZZ/2[x1,x2,x3,x4,x5];
          I = ideal(x1*x2,x3*x4,x5);
     	  primaryDecompositionPseudomonomial(I) 
          ///,
     EXAMPLE lines ///
          R=QQ[x1,x2,x3,x4,x5];
          I = ideal(x1*(x2-1),(x3-1)*x4,x5);
          primaryDecompositionPseudomonomial(I) 
           ///,
     EXAMPLE lines ///         
          R=ZZ/2[x1,x2];
          I = ideal(x1*(x2-1),(x1-1)*(x2-1),x1*x2,(x1-1)*x2);
          primaryDecompositionPseudomonomial(I) 
          ///,
     EXAMPLE lines ///
          R=QQ[x1,x2,x3,x4,x5];
          I = ideal(x5,x1*(x3-1)*(x5-1));
          primaryDecompositionPseudomonomial(I) 
	  ///,
     EXAMPLE lines ///
          R=ZZ/3[x1,x2];
          I = ideal(x5,(x3-1));
          primaryDecompositionPseudomonomial(I) 
	  ///
     }

document {
     Key => {isSquarefreePseudomonomialIdeal, (isSquarefreePseudomonomialIdeal, Ideal)},
     Headline => "determine if an ideal is a square free pseudomonomial ideal",
     SeeAlso => {monomials, monomialIdeal},
     Usage => "isSquarefreePseudomonomialIdeal(I)",
     Inputs => { 
	  "I" => {ofClass Ideal, ", an ideal"}
	  },
     Outputs => {
	  Boolean => {"true or false"}
	  },
     Caveat => {
	  "The algorithm disregards the base field. So, ideal(-x1) is a square free pseudomonomial ideal in F[x1] for F=ZZ,ZZ/p,QQ, whereas ideal(-x1) is not a monomial ideal when F=ZZ."
	  },
      "An ideal is a square free pseudomonomial ideal if its generators are square free pseudomonomials. A square free pseudomonomial is a polynomial of the form P=z1*z2*...*zk such that zi is either xi or xi-1 and a variable can only appear once in P. For example, x1*x3, x1*(x2-1)*(x3-1) are square free pseudomonomial and x2*x3^2, x1*(x1-1)*(x3-1) are not.",
     EXAMPLE lines ///
          R=ZZ/2[x1,x2,x3,x4,x5]; 
          I = ideal(x1*x2,x3*x4,x5);
     	  isSquarefreePseudomonomialIdeal(I) 
          ///,
     EXAMPLE lines ///          
          R=ZZ/2[x1,x2];
          I = ideal(x1*(x2-1),(x1-1)*(x2-1),x1*x2,(x1-1)*x2);
          isSquarefreePseudomonomialIdeal(I) 
	  ///,
     EXAMPLE lines ///          
          R=ZZ/3[x1,x2];
          I = ideal(x1,x2-1);
          isSquarefreePseudomonomialIdeal(I) 
	  ///,
     EXAMPLE lines ///          
          R=QQ[x1,x2,x3,x4,x5];
          I = ideal(x1*(x1-1),(x3-1)*x4,x5);
          not isSquarefreePseudomonomialIdeal(I)
          ///,
     EXAMPLE lines ///          
          R=QQ[x1,x2,x3,x4,x5];
          I = ideal(x1*(x5-1),(x3-1)*x4^2,x5);
          isSquarefreePseudomonomialIdeal(I) 
          ///
     }

------------------------------------------------------------------------------
--- testing internal functions fromPoly2Bit, fromBit2Poly, log2,         -----
--- findMinimal, isLinearPseudomonomial, getFactors (internal functions) -----
------------------------------------------------------------------------------


------ testing fromPoly2Bit----------
TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
assert(fromPoly2Bit(x1*x2,gens R)=={3,0});
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
assert(fromPoly2Bit(x3*x4,gens R)=={12,0});
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
assert(fromPoly2Bit(x5,gens R)=={16,0});
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
assert(fromPoly2Bit(x1*(x2-1),gens R)=={1,2});
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
assert(fromPoly2Bit((x3-1)*x4, gens R)=={8,4});
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
assert(fromPoly2Bit((x1-1)*(x2-1), gens R)=={0,3});
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
assert(fromPoly2Bit(x1*(x3-1)*(x5-1),gens R)=={1,20});
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
assert(fromPoly2Bit(x2-1,gens R)=={0,2});
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
assert(fromPoly2Bit(1_R,gens R)=={0,0});
///

------ testing fromPoly2Bit----------
TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
assert(x1==fromBit2Poly({1,0},gens R));
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
assert(x2==fromBit2Poly({2,0},gens R));
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
assert(x5==fromBit2Poly({16,0},gens R));
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
assert(x2-1==fromBit2Poly({0,2},gens R));
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
assert(x5-1==fromBit2Poly({0,16},gens R));
///

------- testing isDivisor
TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
B1=fromPoly2Bit(x1,gens R); 
B2=fromPoly2Bit(x1*x2, gens R);
assert(isDivisor(B1,B2));
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
B1=fromPoly2Bit(x1*x2,gens R); 
B2=fromPoly2Bit(x1, gens R);
assert(not isDivisor(B1,B2));
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
B1=fromPoly2Bit(x2-1,gens R); 
B2=fromPoly2Bit((x1-1)*(x2-1), gens R);
assert(isDivisor(B1,B2));
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
B1=fromPoly2Bit(x1*(x5-1),gens R); 
B2=fromPoly2Bit(x1*(x3-1)*(x5-1), gens R);
assert(isDivisor(B1,B2));
///

------- testing findMinimal (with respect to inclusion, ie isSubset)
TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
G={set{2,1},set{2,3},set{2,1,3},set{4,5,3,2},set{5}};
assert(set findMinimal(G,isSubset) === set{set{1,2},set{5},set{2,3}})
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
G={set{2},set{2,3},set{2,1,3},set{4,5,3,2,1}};
assert(set findMinimal(G,isSubset) === set{set{2}})
///

-------- testing isLinearPseudomonomial
TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
assert(isLinearPseudomonomial(fromPoly2Bit(x1,gens R)));
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
assert(not isLinearPseudomonomial(fromPoly2Bit(x1*x2,gens R)));
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
assert(isLinearPseudomonomial(fromPoly2Bit(x1-1,gens R)));
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
assert(not isLinearPseudomonomial(fromPoly2Bit((x1-1)*(x2-1),gens R)));
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
assert(not isLinearPseudomonomial(fromPoly2Bit((x1-1)*x2,gens R)));
///


--------- testing getFactors
TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
B=fromPoly2Bit(x1*(x3-1)*(x5-1),gens R);
B1=fromPoly2Bit(x1,gens R); 
B2=fromPoly2Bit(x3-1,gens R);
B3=fromPoly2Bit(x5-1,gens R);
assert(set getFactors(B) === set{B1,B2,B3})
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
B=fromPoly2Bit(x2,gens R);
B1=fromPoly2Bit(x2,gens R); 
assert(set getFactors(B) === set{B1})
///

--------testing setEqualtoZero
TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
I=ideal(x1*(x3-1)*x5,x2*x3);
B1=fromPoly2Bit(x1*(x3-1)*x5,gens R);
B2=fromPoly2Bit(x2*x3*x4,gens R);
Idealbitform={B1,B2};
zi=fromPoly2Bit(x3,gens R);
anscode = setEqualtoZero(Idealbitform,zi);
ansmanual = {fromPoly2Bit(x1*(0-1)*x5,gens R),fromPoly2Bit(x2*0*x4,gens R)};
assert(set anscode === set ansmanual)
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
I=ideal(x1*(x3-1)*x5,x2*x3*x4);
B1=fromPoly2Bit(x1*(x3-1)*x5,gens R);
B2=fromPoly2Bit(x2*x3*x4,gens R);
Idealbitform={B1,B2};
zi=fromPoly2Bit(x1-1,gens R);
anscode = setEqualtoZero(Idealbitform,zi);
ansmanual = {fromPoly2Bit(1*(x3-1)*x5,gens R),fromPoly2Bit(x2*x3*x4,gens R)};
assert(set anscode === set ansmanual)
///

---------- testing simplifyGens
TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
I=ideal(x1*(x3-1)*x5,x2*x3,x2,x1*(x3-1)*x4*x5,0);
B1=fromPoly2Bit(x1*(x3-1)*x5,gens R);
B2=fromPoly2Bit(x2*x3,gens R);
B3=fromPoly2Bit(x2,gens R);
B4=fromPoly2Bit(x1*(x3-1)*x4*x5,gens R);
B5=fromPoly2Bit(0,gens R);
Idealbitform={B1,B2,B3,B4,B5};
assert(set simplifyGens Idealbitform === set {B1,B3})
///

--------- testing isPrimaryPseudomonomial
TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
I=ideal(x1*(x3-1),x2,x4);
B1=fromPoly2Bit(x1*(x3-1),gens R);
B2=fromPoly2Bit(x2,gens R);
B3=fromPoly2Bit(x4,gens R);
Idealbitform={B1,B2,B3};
assert(not isPrimaryPseudomonomial Idealbitform)
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
I=ideal(x3-1,x2,x4);
B1=fromPoly2Bit(x3-1,gens R);
B2=fromPoly2Bit(x2,gens R);
B3=fromPoly2Bit(x4,gens R);
Idealbitform={B1,B2,B3};
assert(isPrimaryPseudomonomial Idealbitform)
///


------ testing isProperPseudomonomial
TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
I=ideal(x3-1,x3,x4*x5);
B1=fromPoly2Bit(x3-1,gens R);
B2=fromPoly2Bit(x3,gens R);
B3=fromPoly2Bit(x4*x5,gens R);
Idealbitform={B1,B2,B3};
assert(not isProperPseudomonomial Idealbitform)
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
I=ideal(x3-1,x2,x4*x5);
B1=fromPoly2Bit(x3-1,gens R);
B2=fromPoly2Bit(x2,gens R);
B3=fromPoly2Bit(x4*x5,gens R);
Idealbitform={B1,B2,B3};
assert(isProperPseudomonomial Idealbitform)
///

------- testing factorIdeal
TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
I=ideal(x3-1,x2,x4*x5);
B1=fromPoly2Bit(x3-1,gens R);
B2=fromPoly2Bit(x2,gens R);
B3=fromPoly2Bit(x4*x5,gens R);
Idealbitform={B1,B2,B3};
anscode=(factorIdeal Idealbitform)/simplifyGens
ansmanual={{B1,B2,fromPoly2Bit(x4,gens R)},{B1,B2,fromPoly2Bit(x5,gens R)}}
assert(set anscode/set=== set ansmanual/set)
///

TEST ///
debug needsPackage "PseudomonomialPrimaryDecomposition"
R=ZZ/2[x1,x2,x3,x4,x5];
I=ideal(x3-1,x2,x3*x5);
B1=fromPoly2Bit(x3-1,gens R);
B2=fromPoly2Bit(x2,gens R);
B3=fromPoly2Bit(x3*x5,gens R);
Idealbitform={B1,B2,B3};
anscode=(factorIdeal Idealbitform)/simplifyGens
ansmanual={{fromPoly2Bit(1,gens R)},{B1,B2,fromPoly2Bit(x5,gens R)}}
assert(set anscode/set=== set ansmanual/set)
///



---------------------------------------------------
--- testing primaryDecompositionPseudomonomial-----
---------------------------------------------------

TEST ///
R=ZZ/2[x1,x2,x3,x4,x5];
I = ideal(x1*x2,x3*x4,x5);
PD = primaryDecompositionPseudomonomial(I);
PDold = primaryDecomposition(I);
PDsorted = sort(apply(PD, J -> sort(J_*)));
PDoldsorted = sort(apply(PDold, J -> sort(J_*)));
assert ( PDsorted == PDoldsorted )
///

TEST ///
R = ZZ/3[x1,x2,x3,x4,x5];
I = ideal(x1*x2,x3*x4,x5);
PD = primaryDecompositionPseudomonomial(I);
PDold = primaryDecomposition(I);
PDsorted = sort(apply(PD, J -> sort(J_*)));
PDoldsorted = sort(apply(PDold, J -> sort(J_*)));
assert ( PDsorted == PDoldsorted )
///

TEST ///
R = QQ[x1,x2,x3,x4,x5];
I = ideal(x1*x2,x3*x4,x5);
PD = primaryDecompositionPseudomonomial(I);
PDold = primaryDecomposition(I);
PDsorted = sort(apply(PD, J -> sort(J_*)));
PDoldsorted = sort(apply(PDold, J -> sort(J_*)));
assert ( PDsorted == PDoldsorted )
///

TEST ///
R = ZZ/2[x1,x2,x3,x4,x5];
I = ideal(x1*(x2-1),(x3-1)*x4,x5)
PD = primaryDecompositionPseudomonomial(I);
PDold = primaryDecomposition(I);
PDsorted = sort(apply(PD, J -> sort(J_*)));
PDoldsorted = sort(apply(PDold, J -> sort(J_*)));
assert ( PDsorted == PDoldsorted )
///

TEST ///
R = ZZ/3[x1,x2,x3,x4,x5];
I = ideal(x1*(x2-1),(x3-1)*x4,x5)
PD = primaryDecompositionPseudomonomial(I);
PDold = primaryDecomposition(I);
PDsorted = sort(apply(PD, J -> sort(J_*)));
PDoldsorted = sort(apply(PDold, J -> sort(J_*)));
assert ( PDsorted == PDoldsorted )
///

TEST ///
R = QQ[x1,x2,x3,x4,x5];
I = ideal(x1*(x2-1),(x3-1)*x4,x5)
PD = primaryDecompositionPseudomonomial(I);
PDold = primaryDecomposition(I);
PDsorted = sort(apply(PD, J -> sort(J_*)));
PDoldsorted = sort(apply(PDold, J -> sort(J_*)));
assert ( PDsorted == PDoldsorted )
///

TEST ///
R = ZZ/2[x1,x2];
I = ideal(x1*(x2-1),(x1-1)*(x2-1),x1*x2,(x1-1)*x2)
PD = primaryDecompositionPseudomonomial(I);
PDold = primaryDecomposition(I);
PDsorted = sort(apply(PD, J -> sort(J_*)));
PDoldsorted = sort(apply(PDold, J -> sort(J_*)));
assert ( PDsorted == PDoldsorted )
///

TEST ///
R = ZZ/3[x1,x2];
I = ideal(x1*(x2-1),(x1-1)*(x2-1),x1*x2,(x1-1)*x2)
PD = primaryDecompositionPseudomonomial(I);
PDold = primaryDecomposition(I);
PDsorted = sort(apply(PD, J -> sort(J_*)));
PDoldsorted = sort(apply(PDold, J -> sort(J_*)));
assert ( PDsorted == PDoldsorted )
///

TEST ///
R = QQ[x1,x2];
I = ideal(x1*(x2-1),(x1-1)*(x2-1),x1*x2,(x1-1)*x2)
PD = primaryDecompositionPseudomonomial(I);
PDold = primaryDecomposition(I);
PDsorted = sort(apply(PD, J -> sort(J_*)));
PDoldsorted = sort(apply(PDold, J -> sort(J_*)));
assert ( PDsorted == PDoldsorted )
///

TEST ///
R=ZZ/2[x1,x2,x3,x4,x5];
I = ideal(x5,x1*(x3-1)*(x5-1));
PD = primaryDecompositionPseudomonomial(I);
PDold = primaryDecomposition(I);
PDsorted = sort(apply(PD, J -> sort(J_*)));
PDoldsorted = sort(apply(PDold, J -> sort(J_*)));
assert ( PDsorted == PDoldsorted )
///

TEST ///
R=ZZ/3[x1,x2,x3,x4,x5];
I = ideal(x5,x1*(x3-1)*(x5-1));
PD = primaryDecompositionPseudomonomial(I);
PDold = primaryDecomposition(I);
PDsorted = sort(apply(PD, J -> sort(J_*)));
PDoldsorted = sort(apply(PDold, J -> sort(J_*)));
assert ( PDsorted == PDoldsorted )
///

TEST ///
R=QQ[x1,x2,x3,x4,x5];
I = ideal(x5,x1*(x3-1)*(x5-1));
PD = primaryDecompositionPseudomonomial(I);
PDold = primaryDecomposition(I);
PDsorted = sort(apply(PD, J -> sort(J_*)));
PDoldsorted = sort(apply(PDold, J -> sort(J_*)));
assert ( PDsorted == PDoldsorted )
///

TEST ///
R = ZZ/2[x1,x2];
I = ideal(x1,x2-1)
PD = primaryDecompositionPseudomonomial(I);
PDold = primaryDecomposition(I);
PDsorted = sort(apply(PD, J -> sort(J_*)));
PDoldsorted = sort(apply(PDold, J -> sort(J_*)));
assert ( PDsorted == PDoldsorted )
///

TEST ///
R = ZZ/3[x1,x2];
I = ideal(x1,x2-1)
PD = primaryDecompositionPseudomonomial(I);
PDold = primaryDecomposition(I);
PDsorted = sort(apply(PD, J -> sort(J_*)));
PDoldsorted = sort(apply(PDold, J -> sort(J_*)));
assert ( PDsorted == PDoldsorted )
///

TEST ///
R = QQ[x1,x2];
I = ideal(x1,x2-1)
PD = primaryDecompositionPseudomonomial(I);
PDold = primaryDecomposition(I);
PDsorted = sort(apply(PD, J -> sort(J_*)));
PDoldsorted = sort(apply(PDold, J -> sort(J_*)));
assert ( PDsorted == PDoldsorted )
///

TEST ///
R = QQ[x1,x2];
I = ideal(x1,x2-1)
PD = primaryDecompositionPseudomonomial(I);
PDold = primaryDecomposition(I);
PDsorted = sort(apply(PD, J -> sort(J_*)));
PDoldsorted = sort(apply(PDold, J -> sort(J_*)));
assert ( PDsorted == PDoldsorted )
///



------------------------------------------------
--- testing isSquarefreePseudomonomialIdeal-----
------------------------------------------------


TEST ///
R = ZZ/2[x1,x2];
I = ideal(x1,x2-1)
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = ZZ/3[x1,x2];
I = ideal(x1,x2-1)
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = QQ[x1,x2];
I = ideal(x1,x2-1)
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = ZZ[x1,x2];
I = ideal(x1,x2-1)
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = GF(4)[x1,x2];
I = ideal(x1,x2-1)
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = GF(9)[x1,x2];
I = ideal(x1,x2-1)
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = ZZ/2[x1,x2];
I = ideal(x1^2,x2-1)
assert ( isSquarefreePseudomonomialIdeal I == false )
///

TEST ///
R = ZZ/3[x1,x2];
I = ideal(x1,x2*(x2-1))
assert ( isSquarefreePseudomonomialIdeal I == false )
///

TEST ///
R = QQ[x1,x2];
I = ideal(x1,x2*(x2-1))
assert ( isSquarefreePseudomonomialIdeal I == false )
///

TEST ///
R = ZZ[x1,x2];
I = ideal(x1,x2*(x2-1))
assert ( isSquarefreePseudomonomialIdeal I == false )
///

TEST ///
R = GF(4)[x1,x2];
I = ideal(x1,x2*(x2-1))
assert ( isSquarefreePseudomonomialIdeal I == false )
///

TEST ///
R = GF(9)[x1,x2];
I = ideal(x1,x2*(x2-1))
assert ( isSquarefreePseudomonomialIdeal I == false )
///

TEST ///
R = ZZ/2[x1,x2,x3,x4];
I = ideal(x1*(x2-1),x3*x4,x2-1)
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = ZZ/3[x1,x2,x3,x4];
I = ideal(x1*(x2-1),x3*x4,x2-1)
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = QQ[x1,x2,x3,x4];
I = ideal(x1*(x2-1),x3*x4,x2-1)
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = ZZ[x1,x2,x3,x4];
I = ideal(x1*(x2-1),x3*x4,x2-1)
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = GF(4)[x1,x2,x3,x4];
I = ideal(x1*(x2-1),x3*x4,x2-1)
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = GF(9)[x1,x2,x3,x4];
I = ideal(x1*(x2-1),x3*x4,x2-1)
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = ZZ/2[x1,x2,x3,x4];
I = ideal(x1*(x2-1)*x3*(x4-1))
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = ZZ/3[x1,x2,x3,x4];
I = ideal(x1*(x2-1)*x3*(x4-1))
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = QQ[x1,x2,x3,x4];
I = ideal(x1*(x2-1)*x3*(x4-1))
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = ZZ[x1,x2,x3,x4];
I = ideal(x1*(x2-1)*x3*(x4-1))
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = GF(4)[x1,x2,x3,x4];
I = ideal(x1*(x2-1)*x3*(x4-1))
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = GF(9)[x1,x2,x3,x4];
I = ideal(x1*(x2-1)*x3*(x4-1))
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = ZZ/2[x1,x2,x3,x4];
I = ideal(1)
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = QQ[x1,x2,x3,x4];
I = ideal(1)
assert ( isSquarefreePseudomonomialIdeal I == true )
///

TEST ///
R = GF(9)[x1,x2,x3,x4];
I = ideal(1)
assert ( isSquarefreePseudomonomialIdeal I == true )
///


end 

-- installPackage("PseudomonomialPrimaryDecomposition"); check(PseudomonomialPrimaryDecomposition)





