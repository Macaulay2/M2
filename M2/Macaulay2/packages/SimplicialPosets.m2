-- -*- coding: utf-8 -*-
-- Copyright (C) 2019  Nathan Nichols
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

newPackage(
    	"SimplicialPosets",
    	Version => "1.0",
    	Date => "December 3, 2019",
	Authors => {{
		    Name => "Nathan Nichols",
		    Email => "nicho997@umn.edu"}},
        Headline => "Package for constructing Stanley simplicial poset rings.",
	Keywords => {"Combinatorial Commutative Algebra"},
	DebuggingMode => false,
	PackageExports => {
	    "Posets",
	    "SimplicialComplexes",
	    "RandomIdeals",
	    "EdgeIdeals",
	    "Graphs"
	    }
    )

export {
    "stanleyPosetIdeal",
    "fromFVector",
    "isBoolean",
    "getFVector",
    "testFVector"
    };

------------------------------------------
------------------------------------------
-- Methods
------------------------------------------
------------------------------------------

------------------------------------------
-- Non-exported functions
------------------------------------------

-- From the Posets package.
-- Given an element "a" of P, returns its index in P.GroundSet.
-- (This index is a's row in P's relation matrix.)
indexElement = (P, a) -> (
    j := position(P.GroundSet, i -> i === a);
    if j === null then error("The element [" | toString a | "] is not in the poset.") else j
    );

-- From the Posets package.
-- Returns the nonzero indices in the ith row of P.relationMatrix, which correspond to 
-- elements greater than the ith element.
principalFilter' = (P, i) -> positions(first entries(P.RelationMatrix^{i}), j -> j != 0)

-- Modified "joinExists" from the Posets package.
-- Returns the index in P.GroundSet of every upper bound of a and b in P. 
-- Expects an element of P.GroundSet, not an index.
upperBounds = (P, a, b) -> (
    -- These are lists of the elements greater than a and b.
    OIa := principalFilter'(P, indexElement(P, a));
    OIb := principalFilter'(P, indexElement(P, b));
    -- "*" is the set intersection operator.
    toList (set(OIa)*set(OIb))
    );

-- Return the minimal upper bounds of a,b in P.
-- Only works on posets with zero.
minUpperBounds = (P, a, b) -> (
    allUB := upperBounds(P, a, b);
    minP := minimalElements(P);
    f := i -> (set(upperBounds(P, P.GroundSet#i, minP#0)))-set({i});
    nonminUB := sum(apply(allUB, f));
    -- minUB is a list of sets. Each element in a set can't be a minimal upper bound.
    if nonminUB == 0 then nonminUB = set();
    (set(allUB) - nonminUB)
    );

------------------------------------------
-- Working with simplicial posets
------------------------------------------

-- Tests it according to theorem 2.1 (Stanley 1989.)
-- Note: there is an additional condition that f_-1 = 1.
testFVector = method()
testFVector List := Boolean => fVec -> (
    
    if fVec#0 =!= 1 then return false;

    len := length fVec;
    smallest := for i from 0 to len-1 list(binomial(len-1, i));
    for i from 0 to len-1 do(
	if smallest#i > fVec#i then return false;
	);
    true
    );

-- Uses the facts that:
-- -Every finite boolean algebra is atomic.
-- -The number of atoms of a finite boolean algebra determines its isomorphism class.
isBoolean = method()
isBoolean Poset := Boolean => P -> (
    atomsP := atoms(P);
    areIsomorphic(P, booleanLattice(#atomsP))
    );

-- Returns true if P is simplicial and false otherwise.
-- the 'isSimplicial' methods is defined in 'Polyhedra'
isSimplicial Poset := Boolean => P -> (
    
    -- Not sure if doing this correctly (did we ever write to the cache?)
    --if P.cache.?isSimplicial then return P.cache.isSimplicial;
    
    minP := minimalElements P;
    if (#minP) != 1 then return false;
    
    zeroP := minP#0;    
        
    -- Test that each interval is a boolean algebra.
    for x in vertices(P) do (
	interval := closedInterval(P, zeroP, x);
    	if not isBoolean(interval) then return false;
	);
    true
    );

-- Returns the f-vector {f_-1,f_0,...,f_{d-1}} of the poset P.
getFVector = method()
getFVector Poset := List => P ->(
    gfP := rankGeneratingFunction(P);
    (M,C) := coefficients gfP;
    -- The highest coefficient is stored in toList(entries C)#0
    fVec := reverse(apply(toList (entries C), i -> i#0));
    
    for n from 0 to (length fVec)-1 list(
	k := fVec#n;
	f := map(ZZ, ring k, (gens ring k)/(i -> i => 1_ZZ));
	f(k)
	)
    );

-- Returns a poset that is simplicial with f-vector fVec.
-- Uses the construction from the proof of theorem 2.1 (Stanley 1989.)
fromFVector = method()
fromFVector List := Poset => fVec -> (
    if not testFVector(fVec) then error "Must be a valid f-vector.";
    len := length fVec;
    -- The (len)th row of Pascal's triangle
    smallest := for i from 0 to len-1 list(binomial(len-1, i));
    -- The difference between the f-vector we want and the f-vector of B_len
    toAdd := fVec - smallest;
    P := booleanLattice(len-1);
    rankPosetP := rankPoset P;
    symNo := 1;
    newRels := {};
    
    for i from 0 to length(toAdd)-1 do (
	amtToAdd := toAdd#i;
	for j from 1 to amtToAdd do(
	    -- "Chosen" can be any element of rank i.    
	    chosen := rankPosetP#i#0;
	    -- The new element is above every element strictly less than "chosen"
	    leq := principalOrderIdeal(P, chosen);
	    allBelow := for k in leq list(
		if k =!= chosen then {k,symNo} else continue
		);
	    newRels = newRels | allBelow;
	    symNo = symNo + 1;
	    );
	);
    newCov := coveringRelations(P) | newRels;
    poset(newCov)
    );

-- Returns an ideal I where ring(I)/I is the Stanley Poset ideal, Ã.
stanleyPosetIdeal = method()
stanleyPosetIdeal Poset := Ideal => P -> (
    
    if not isSimplicial(P) then error "Must be a simplicial poset.";
    
    ringVars := for i from 0 to #vertices(P)-1 list(getSymbol("x"));    
    
    syms := for i from 0 to #ringVars-1 list (
    	ringVars#i_(toString(P.GroundSet#i))
    	);
    
    -- This is the right way to define symbols according to the style guide.
    gndRing := QQ(monoid[syms]);
    syms = gens gndRing;
    gensI := {};
    
    for i in subsets(vertices(P),2) do(	
	a := first i;
	b := last i;     	
	mubs := toList minUpperBounds(P, a, b);	
	if #mubs =!= 0 then(
	    term := (syms#(indexElement(P, a)))*(syms#(indexElement(P,b)));
	    meet := posetMeet(P, a, b);
	    m := syms # (indexElement(P,meet#0));
	    sumUB := sum(apply(mubs, k -> syms#k));
	    final := term - (m*sumUB);
	    gensI = gensI | {final};
	    ) 
	else(
	    elemA := syms#(indexElement(P,a));
	    elemB := syms#(indexElement(P,b)); 
	    gensI = gensI | {elemA*elemB};
	    );
	);
    -- These would create the ring A_P (without a tilde.)
    gensI = toList(set(gensI));
    zeroP := first minimalElements(P);
    zeroVarP := syms # (indexElement(P, zeroP));   
    gensI2 := gensI | {zeroVarP - 1};
        
    ideal(gensI2)
    );




beginDocumentation()

-- Front Page
doc ///
    Key
        SimplicialPosets
    Headline
        A package for working with simplicial posets.
    Description
        Text
	
	    The primary purpose of this package is to implement the simplicial
	    poset ring as defined by Richard P. Stanley in 1989. 

    	    @HREF("https://www.semanticscholar.org/paper/f-vectors-and-h-vectors-of-simplicial-posets-Stanley/2787117152700af2abce3126dd7ba2325685d78b","Stanley's original paper.")@

            {\bf Acknowledgements}:

    	    This package uses some code from the  @TO "Posets"@ package.
	    
	    Thanks to @HREF("http://www-users.math.umn.edu/~reiner/","Victor Reiner")@. This project was his idea.
///

-- Example
doc /// 
    Key
        "Example: Calculating a Stanley simplicial poset ring"
    Description
        Text
            The following is an example of the basic task that this package was written to accomplish.
        Example
            P = fromFVector({1,6,5,1});
	    I = stanleyPosetIdeal(P);
	    R = ring(I)/I;
	    getFVector(P)
	    isSimplicial(P)
    SeeAlso
        stanleyPosetIdeal
///


-- isSimplicial
doc ///
    Key
        isSimplicial
        (isSimplicial,Poset)
    Headline
        Determine if a poset is simplicial.
    Usage
        r = isSimplicial(P)
    Inputs
        P:Poset
            The poset to be tested
    Outputs
        r:Boolean
            Whether the given poset is simplicial or not
    Description
        Text
            This method uses the @TO "isBoolean"@ method to check
	    that every closed interval of P is a boolean algebra.
	    
        Example
            P = booleanLattice(3);
	    isSimplicial(P)
///

-- isBoolean
doc ///
    Key
        isBoolean
        (isBoolean,Poset)
    Headline
        Determine if a poset is a boolean algebra.
    Usage
        r = isBoolean(P)
    Inputs
        P:Poset
            The poset to be tested
    Outputs
        r:Boolean
            Whether the given poset is a boolean algebra or not
    Description
        Text
            Every finite boolean algebra is atomic and the number 
	    of atoms determines its isomorphism class.
	    
	    This makes it possible to use the @TO "atoms"@ method
	    to determine the necessary isomorphism class of P and
	    use the @TO "areIsomorphic"@ method to check if the
	    required isomorphism holds.
	    
        Example
            P = booleanLattice(3);
	    isBoolean(P)
///


-- fromFVector
doc ///
    Key
        fromFVector
        (fromFVector,List)
    Headline
    	If possible, returns a simplicial poset with the given f-vector.
    Usage
	P = fromFVector(L)
    Inputs
        L:List
            The desired F-vector.
    Outputs
        P:Poset
	    A simplicial poset with f-vector L. 
    Description
        Text	    
	    This method is provided as a way to construct an  
	    example of a simplicial poset with a given f-vector.
	    
    	    It implements a construction due to Richard Stanley.
	    
	    For details about this construction and a description
	    of what f-vectors can be constructed, see @HREF("https://www.semanticscholar.org/paper/f-vectors-and-h-vectors-of-simplicial-posets-Stanley/2787117152700af2abce3126dd7ba2325685d78b","Stanley's original paper.")@
	    
        Example
	    P = fromFVector({1,6,5,1});
    	    isSimplicial(P)
	    getFVector(P)
///

-- testFVector	  
doc ///
    Key 
    	testFVector
	(testFVector, List)
    Headline
    	Tests if it is possible for a simplicial poset to have a given f-vector.
    Usage
    	B = testFVector(L)
    Inputs
    	L:List
	    The f-vector to test.
    Outputs
    	B:Boolean
	    Whether a poset with the given f-vector exists.
    Description
    	Text		
    	    See @HREF("https://www.semanticscholar.org/paper/f-vectors-and-h-vectors-of-simplicial-posets-Stanley/2787117152700af2abce3126dd7ba2325685d78b","Stanley's paper")@ for more details about what f-vectors a simplicial poset can have. 
	    
	    This is useful for validating the arguments of a function that takes an f-vector.
    	Example
	    testFVector({1,4,6,4,1})
///

-- getFVector
doc ///
    Key
        getFVector
        (getFVector, Poset)
    Headline
    	Returns the f-vector {f_{-1},f_0,...,f_{d-1}} of the given poset.
    Usage
	L = getFVector(P)
    Inputs
        P:Poset
            A poset.
    Outputs
        L:List
       	    The f-vector {f_-1,f_0,...,f_{d-1}} of the poset P.
    Description
        Text	    
    	    This is a wrapper around the @TO "rankGeneratingFunction"@
            method from the @TO "Posets"@ package.
	Example
	    P = fromFVector({1,6,5,1});
	    L = getFVector(P)
///

-- stanleyPosetIdeal
doc ///
    Key
        stanleyPosetIdeal
        (stanleyPosetIdeal, Poset)
    Headline
    	Returns ideal that defines the simplicial poset ring of the given simplicial poset.
    Usage
	I = stanleyPosetIdeal(P)
    Inputs
        P:Poset
            A simplicial poset.
    Outputs
        I:Ideal
       	    An ideal such that ring(I)/I is the simplicial poset ring of P.
    Description
        Text	    
	    For details about the definition of this ideal, refer
	    to @HREF("https://www.semanticscholar.org/paper/f-vectors-and-h-vectors-of-simplicial-posets-Stanley/2787117152700af2abce3126dd7ba2325685d78b","Stanley's original paper,")@ specifically
	    definition 3.3.
	
	Example
	    P = booleanLattice(3);
	    I = stanleyPosetIdeal(P);
	    SPR := ring(I)/I;
///



------------------------------------------
------------------------------------------
-- Tests
------------------------------------------
------------------------------------------

-- isBoolean test
TEST ///
A = booleanLattice 2
B = booleanLattice 3
C = fromFVector({1,3,3,7})
D = fromFVector({1,2,3})
assert(isBoolean A)
assert(isBoolean B)
assert(not isBoolean(C))
assert(not isBoolean(D))
///

-- isSimplicial test
TEST ///
A = booleanLattice 2
B = booleanLattice 3
C = fromFVector({1,3,3,7})
assert(isSimplicial A)
assert(isSimplicial B)
assert(isSimplicial C)
assert(not isSimplicial chain 5)
///

-- fromFVector test
TEST ///
F1 = {1,4,5}
F2 = {1,5,7,5,3}
F3 = {1,6,6,6}
A = fromFVector(F1)
B = fromFVector(F2)
C = fromFVector(F3)
assert(getFVector(A) == F1)
assert(getFVector(B) == F2)
assert(getFVector(C) == F3)
///

-- getFVector test
TEST ///
A = booleanLattice 3
B = booleanLattice 4
assert(getFVector(A) == {1,3,3,1})
assert(getFVector(B) == {1,4,6,4,1})
///


-- stanleyPosetIdeal test
TEST ///
needsPackage "EdgeIdeals"
-- Number of tests
N = 50;
-- Erdős–Rényi graph parameters 
n = 5;
p = 0.5;

for i from 1 to N do(
    -- Generate an Erdős–Rényi random graph and take the flag complex
    R := QQ[vars(0..n)];
    E := select(edges completeGraph(R,n), (e -> random(1.0) < p));
    G := graph(R,E);
    C := cliqueComplex(G);
    -- When P is the face poset of a simplicial complex C, the Stanley 
    -- poset ideal of P is supposed to be the same as the Stanley-Reisner 
    -- ideal of C.
    P := facePoset C;
    I1 := minimalPresentation stanleyPosetIdeal P;
    I2 := ideal(C);
    M := map(ring(I2), ring(I1), vars(ring(I2)));
    V := vars(ring(I1));
    assert(I2 == M(I1));
    );
///


end--


restart
installPackage("SimplicialPosets")

