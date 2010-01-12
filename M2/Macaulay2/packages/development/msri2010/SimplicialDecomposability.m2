-------------------
-- Package Header
-------------------
needsPackage "Depth";
needsPackage "SimplicialComplexes";
newPackage (
    "SimplicialDecomposability",
    Version => "0.3",
    Date => "xx. January 2010",
    Authors => {{Name => "David W. Cook II", Email => "dcook@ms.uky.edu", HomePage => "http://www.ms.uky.edu/~dcook"}},
    Headline => "Pure k-Decomposability for simplicial complexes.",
    DebuggingMode => true
);
needsPackage "Depth";
needsPackage "SimplicialComplexes";

-- TODO
---- add tests for:  allFaces, isShellable's different strategies
---- add impure variants of iskDecomposable, isSheddingFace, isShellable, isShelling, isVertexDecomposable, shellingOrder

-------------------
-- Exports
-------------------
export {
    allFaces,
    faceDelete,
    iskDecomposable,
    isSheddingFace,
    isShellable,
    isShelling,
    isSimplex,
    isVertexDecomposable,
    hVector,
    shellingOrder
};

-------------------
-- Exported Code
-------------------

-- Returns all faces of a simplicial complex (except {})
allFaces = method(TypicalValue => List);
allFaces (SimplicialComplex) := (S) -> (
    flatten for i from 0 to dim S list flatten entries faces(i, S)
);

-- Face Deletion: Remove all faces of a complex containing the given face.
faceDelete = method(TypicalValue => SimplicialComplex);
faceDelete (SimplicialComplex, RingElement) := (S,F) -> (
    simplicialComplex monomialIdeal (ideal S + ideal(F))
);

-- Determines if a pure simplicial complex is k-decomposable.
iskDecomposable = method(TypicalValue => Boolean);
iskDecomposable (SimplicialComplex, ZZ) := (S, k) -> (
    -- any of {non-pure, negatives in hVector} imply not pure k-decomposable
    if not isPure S then return false;
    if any(hVector S, i -> i<0) then return false;

    -- base case: simplexes are k-decomposable for all nonnegative k
    if isSimplex S then return true;

    -- Find all possible relevant faces:
    L := flatten for i from 0 to k list flatten entries faces(i, S);
    -- Check for any shedding faces.
    for F in L do if isSheddingFace(S, F, k) then return true;
    false
);

-- Determines if a face is a shedding face of a pure simplicial complex.
isSheddingFace = method(TypicalValue => Boolean);
isSheddingFace (SimplicialComplex, RingElement) := (S, F) -> (
    isSheddingFace(S, F, first degree F - 1)
);
isSheddingFace (SimplicialComplex, RingElement, ZZ) := (S, F, k) -> (
    d := max(k, first degree F - 1);
    iskDecomposable(link(S, F), d) and iskDecomposable(faceDelete(S, F), d)
);

-- Determines if a pure simplicial complex is shellable.
isShellable = method(TypicalValue => Boolean, Options => {Strategy => "Recursive"});
isShellable (SimplicialComplex) := options -> (S) -> (
    if options.Strategy == "dDecomposable" then (
        -- S is (dim S)-decomposable if and only if S is shellable (see thm 2.8 is Provan-Billera)
        iskDecomposable(S, dim S)
    )
    else (
        -- otherwise pass the options to the shellingOrder routine
        shellingOrder(S, options) != {}
    )
);

-- Determines if a list of equidimensional faces is a shelling.
isShelling = method(TypicalValue => Boolean);
isShelling (List) := (L) -> (
    -- Check for (i) equidimensional, (ii) monomials, (iii) squarefree, and (iv) monic
    if #unique apply(L, degree) > 1 then return false;
    if any(apply(L, size), i->i!=1) then return false;
    if max flatten flatten apply(L, exponents) > 1 then return false;
    if any(flatten flatten apply(L, i->entries (coefficients i)_1), i->i!=1) then return false;
    
    -- Sets with zero or one face are always shellings 
    if #L <= 1 then return true;

    -- prime the loop
    f0 := ta := null;
    f1 := allFaces simplicialComplex take(L, 1);
    -- for each face in the list
    for i from 2 to #L do (
        -- copy the last step
        f0 = f1;
        -- update the newest
        f1 = allFaces simplicialComplex take(L, i);
        -- find the added faces & count their dimensions (+1)
        ta = tally flatten apply(toList(set f1 - set f0), degree);
        -- make sure the minimal face is unique
        if ta_(min keys ta) != 1 then return false;
    );
    true
);

-- Determines if a simplicial complex is (isomorphic to) a simplex.
isSimplex = method(TypicalValue => Boolean);
isSimplex (SimplicialComplex) := (S) -> (
    #flatten entries facets S <= 1
);

-- Determines if a simplicial complex is (pure) vertex decomposable.
isVertexDecomposable = method(TypicalValue => Boolean);
isVertexDecomposable (SimplicialComplex) := (S) -> (
    iskDecomposable(S, 0)
);

-- Determines the hVector of the Stanley-Reisner ideal of a simplicial complex.
hVector = method(TypicalValue => List);
hVector (SimplicialComplex) := (S) -> (
    flatten entries sub(last coefficients numerator reduceHilbert hilbertSeries ideal S, ZZ)
);

-- Attempts to find a shelling order of a pure simplicial complex.
shellingOrder = method(TypicalValue => List, Options => {Strategy => "Recursive"});
shellingOrder (SimplicialComplex) := options -> (S) -> (
    -- any of {non-pure, non-CM, negatives in hVector} imply not pure shellable
    if not isPure S then return {};
    --if not isCM quotient ideal S then return {};  -- good idea, but this is REALLY slow
    if any(hVector S, i -> i<0) then return {};

    if options.Strategy == "Naive" then (
        -- NAIVE: simply look at all facet permutations
        P := permutations first entries facets S;
        for L in P do if isShelling L then return L;
    )
    else (
        -- RECURSIVE: start the recursion
        O := recursiveShell({}, flatten entries facets S);
        if O != {} then return O;
    );
    {}
);

-------------------
-- Local-Only Code
-------------------

-- Build up a shelling recursively.  Called by shellingOrder with Strategy => "Recursive"
recursiveShell = method(TypicalValue => List);
recursiveShell (List, List) := (O, P) -> (
    -- if it's "obvious", then keep going
    OisShelling := true;
    if #O > 1 then (
        -- drop(O, -1) is already a shelling order, is O?
        f0 := allFaces simplicialComplex drop(O, -1);
        f1 := allFaces simplicialComplex O;
        ta := tally flatten apply(toList(set f1 - set f0), degree);
        OisShelling = (ta_(min keys ta) == 1);
    );
    if OisShelling then (
        -- Nothing else to add: we're done
        if P == {} then return O;
        -- Recurse until success, if possible
        Q := {};
        for i from 0 to #P - 1 do (
            Q = recursiveShell(append(O, P_i), drop(P, {i,i}));
            if Q != {} then return Q;
        );
    );
    {}
);

-------------------
-- Documentation
-------------------
beginDocumentation()
doc ///
    Key
        SimplicialDecomposability
    Headline
        Pure k-Decomposability for simplicial complexes.
    Description
        Text
            Determines pure k-Decomposability (including shellability) for simplicial complexes.
///

doc ///
    Key
        allFaces
        (allFaces, SimplicialComplex)
    Headline
        returns all faces of a simplicial complex
    Usage
        allFaces S
    Inputs
        S:SimplicialComplex
    Outputs
        L:List
            the list of all faces of {\tt S} (excluding the empty face)
    Description
        Example
            R = QQ[a,b,c,d,e];
            allFaces simplicialComplex {a*b*c*d*e};
    SeeAlso
        faces
        facets
///

doc ///
    Key
        faceDelete
        (faceDelete, SimplicialComplex, RingElement)
    Headline
        computes the face deletion for a simplicial complex
    Usage
        faceDelete(S, F)
    Inputs
        S:SimplicialComplex
        F:RingElement
            a face of {\tt S}
    Outputs
        T:SimplicialComplex
            the simplicial complex of all faces in {\tt S} not containing the face {\tt F}
    Description
        Text
            T = {G in S such that F is not contained in G}
        Example
            R = QQ[a,b,c,d,e];
            S = simplicialComplex {a*b*c*d*e};
            faceDelete(S, a)
            faceDelete(S, a*b*c)
            faceDelete(S, a*b*c*d*e) == boundary S
    SeeAlso
        link
///

doc ///
    Key
        iskDecomposable
        (iskDecomposable, SimplicialComplex, ZZ)
    Headline
        determines if a simplicial complex is (pure) k-decomposable or not
    Usage
        iskDecomposable(S,k)
    Inputs
        S:SimplicialComplex
        k:ZZ
    Outputs
        B:Boolean
            true if and only if {\tt S} is (pure) k-decomposable
    Description
        Text
            This function uses the recursive definition that a simplicial complex is k-decomoosable
            if either it is a simplex or there is a face of dimension at most k such that the face deletion
            and link of the face (from the simplicial complex) are also k-decomposable.

            See section two of J. S. Provan and L. J. Billera, "Decompositions of Simplicial Complexes Related to Diameters of Convex Polyhedra,"
            Mathematics of Operations Research, Vol. 5, No. 4 (Nov., 1980), pp. 576-594.
        Example
            R = QQ[a,b,c,d,e];
            iskDecomposable(simplicialComplex {a*b*c*d*e}, 0)
            iskDecomposable(simplicialComplex {a*b*c,c*d*e,c*d*e}, 2)
    SeeAlso
        isSheddingFace
        isShellable
        isVertexDecomposable
///

doc ///
    Key
        isSheddingFace
        (isSheddingFace, SimplicialComplex, RingElement)
        (isSheddingFace, SimplicialComplex, RingElement, ZZ)
    Headline
        determines if a face of a pure simplicial complex is a shedding face.
    Usage
        isSheddingFace(S, F)
        isSheddingFace(S, F, k)
    Inputs
        S:SimplicialComplex
        F:RingElement
            a face of {\tt S}
        k:ZZ
            the dimension of the shedding nature
    Outputs
        B:Boolean
            true if and only if {\tt F} is a shedding face of {\tt S} in dimension max(dim F, {\tt k})
    Description
        Text
            A face is a shedding face if both link(S, F) and faceDelete(S, F) are (pure) k-decomposable.
        Example
            R = QQ[a..d];
            S = simplicialComplex {a*b*c*d};
            isSheddingFace(S, a)
    SeeAlso
        isSheddingFace
        isShellable
        isVertexDecomposable
///

doc ///
    Key
        isShellable
        (isShellable, SimplicialComplex)
    Headline
        determines if a simplicial complex is (pure) shellable or not
    Usage
        isShellable S
    Inputs
        S:SimplicialComplex
    Outputs
        B:Boolean
            true if and only if {\tt S} is (pure) shellable
    Description
        Text
            This function takes an optional Strategy which determines how the method works.  The options are those available in
            shellingOrder (in particular, "Recursive" is the default) and also "dDecomposable" which checks if {\tt S} is 
            {\tt dim S}-decomposable.
        Example
            R = QQ[a,b,c,d,e];
            isShellable simplicialComplex {a*b*c*d*e}
            isShellable simplicialComplex {a*b*c,c*d*e}
            isShellable simplicialComplex {a*b*c,b*c*d,c*d*e}
    SeeAlso
        iskDecomposable
        isShelling
        isVertexDecomposable
        shellingOrder
///

doc ///
    Key
        isShelling
        (isShelling, List)
    Headline
        determines if a list of equidimensional faces is a shelling
    Usage
        isShelling L
    Inputs
        L:List
            a list of equidimensional faces (i.e., squarefree monic monomials)
    Outputs
        B:Boolean
            true if and only if {\tt L} is (pure) shelling
    Description
        Text
            Determines if a list of equidimensional faces is a shelling of the simplicial complex generated by the faces.
        Example
            R = QQ[a,b,c,d,e];
            isShelling {a*b*c, c*d*e}
            isShelling {a*b*c, b*c*d, c*d*e}
    SeeAlso
        isShellable
        shellingOrder
///

doc ///
    Key
        isSimplex
        (isSimplex, SimplicialComplex)
    Headline
        determines if a simplicial complex is simplex
    Usage
        isSimplex S
    Inputs
        S:SimplicialComplex
    Outputs
        B:Boolean
            true if and only if {\tt S} is simplex
    Description
        Example
            R = QQ[a,b,c,d];
            isSimplex simplicialComplex {a*b*c*d}
            isSimplex simplicialComplex {a*b}
            isSimplex simplicialComplex {a*b,c*d}
///

doc ///
    Key
        isVertexDecomposable
        (isVertexDecomposable, SimplicialComplex)
    Headline
        determines if a simplicial complex is (pure) vertex-decomposable or not
    Usage
        isVertexDecomposable S
    Inputs
        S:SimplicialComplex
    Outputs
        B:Boolean
            true if and only if {\tt S} is (pure) vertex-decomposable
    Description
        Text
            Vertex-decomposability is just 0-decomposability.
        Example
            R = QQ[a,b,c,d,e];
            isVertexDecomposable simplicialComplex {a*b*c*d*e}
            isVertexDecomposable boundary simplicialComplex {a*b*c*d*e}
            isVertexDecomposable simplicialComplex {a*b*c,c*d*e,c*d*e}
    SeeAlso
        isShellable
        iskDecomposable
///

doc ///
    Key
        hVector
        (hVector, SimplicialComplex)
    Headline
        determines the h-Vector of a simplicial complex
    Usage
        hVector S
    Inputs
        S:SimplicialComplex
    Outputs
        h:List
            the h-Vector of {\tt S}
    Description
        Example
            R = QQ[a,b,c,d];
            hVector simplicialComplex {a*b*c,d}
    SeeAlso
        fVector
///

doc ///
    Key
        shellingOrder
        (shellingOrder, SimplicialComplex)
    Headline
        finds a shelling of a pure simplicial complex, if one exists
    Usage
        L = shellingOrder S
    Inputs
        S:SimplicialComplex
    Outputs
        L:List
            a (pure) shelling order of the facets of {\tt S}, if one exists
    Description
        Text
            This method behaves differently depending on the Strategy passed as an option.  The default strategy is "Recursive" which
            attempts to recursively find a shelling order.  An alternate strategy is "Naive" which simply checks all permutations
            of the facets with the isShelling routine until one is found, if one exists.
        Example
            R = QQ[a,b,c,d,e];
            shellingOrder simplicialComplex {a*b*c*d*e}
            shellingOrder simplicialComplex {a*b*c, b*c*d, c*d*e}
            shellingOrder simplicialComplex {a*b*c, c*d*e}
    SeeAlso
        isShellable
        isShelling
///

-------------------
-- Tests
-------------------

-- Tests of isSimplex
TEST ///
R = QQ[a..d];
assert(isSimplex simplicialComplex {a*b*c*d});
assert(isSimplex simplicialComplex {a*b*c});
assert(isSimplex simplicialComplex {a*b});
assert(isSimplex simplicialComplex {a});
assert(isSimplex simplicialComplex monomialIdeal {a,b,c,d}); -- empty complex
assert(not isSimplex simplicialComplex {a*b, b*c, c*d});
assert(not isSimplex simplicialComplex {a*b, b*c*d});
assert(not isSimplex simplicialComplex {a*b, c});
assert(not isSimplex simplicialComplex {a, b, c, d});
///

-- Tests of isShelling
TEST ///
R = QQ[a..f];
assert(isShelling {a*b*c});
assert(isShelling {a*b*c, b*c*d});
assert(isShelling {a*b*c, b*c*d, c*d*e});
assert(isShelling {a*b*c, b*c*d, c*d*e, d*e*f});
assert(not isShelling {a*b*c, c*d*e});
assert(not isShelling {a*b*c, d*e*f});
///

-- Tests of isShellable (and hence shellingOrder by invocation): Strategy => "Recursive"
-- NB: shellingOrder can only be tested this way as a shelling order need not be unique.
TEST ///
R = QQ[a..e];
assert(isShellable simplicialComplex {a*b*c});
assert(isShellable simplicialComplex {a*b*c, b*c*d});
assert(isShellable simplicialComplex {a*b*c, b*c*d, c*d*e});
assert(isShellable simplicialComplex monomialIdeal {a,b,c,d,e}); -- empty complex
assert(not isShellable simplicialComplex {a*b*c, c*d*e});
assert(not isShellable simplicialComplex {a*b*c, b*c*d, e});
///

-- Tests of isShellable (and hence shellingOrder by invocation): Strategy => "Naive"
TEST ///
R = QQ[a..e];
assert(isShellable(simplicialComplex {a*b*c}, Strategy => "Naive"));
assert(isShellable(simplicialComplex {a*b*c, b*c*d}, Strategy => "Naive"));
assert(isShellable(simplicialComplex {a*b*c, b*c*d, c*d*e}, Strategy => "Naive"));
assert(isShellable(simplicialComplex monomialIdeal {a,b,c,d,e}, Strategy => "Naive")); -- empty complex
assert(not isShellable(simplicialComplex {a*b*c, c*d*e}, Strategy => "Naive"));
assert(not isShellable(simplicialComplex {a*b*c, b*c*d, e}, Strategy => "Naive"));
///

-- Tests of hVector
TEST ///
R = QQ[a..e];
assert(hVector simplicialComplex {a, b, c, d, e} === {1,4});
assert(hVector simplicialComplex {a*b*c*d*e} === {1});
assert(hVector simplicialComplex {a*b*c, b*c*d, c*d*e} === {1,2});
assert(hVector simplicialComplex {a*b, b*c, c*d, d*e, b*d} === {1,3,1});
assert(hVector simplicialComplex {a*b*c, c*d*e} === {1, 2, -1});
assert(hVector simplicialComplex {a, b*c, d*e} === {1, 3, -2});
///

-- Tests of faceDelete
TEST ///
R = QQ[a..e];
S = simplicialComplex {a*b*c*d*e};
assert(faceDelete(S, a) == simplicialComplex {b*c*d*e});
assert(faceDelete(S, a*b*c) == simplicialComplex {b*c*d*e, a*c*d*e, a*b*d*e});
assert(faceDelete(S, a*b*c*d*e) == boundary S)
///

-- Tests of iskDecomposable (and hence isVertexDecomposable)
TEST ///
R = QQ[a..e];
S = simplicialComplex {a*b*c*d*e};
assert(iskDecomposable(S, 0));
assert(iskDecomposable(S, 1));
assert(iskDecomposable(S, 2));
assert(iskDecomposable(S, 3));
assert(iskDecomposable(S, 4));
assert(iskDecomposable(boundary S, 0)); -- prop 2.2 in Provan-Billera
assert(iskDecomposable(simplicialComplex {a*b*c, b*c*d, c*d*e}, 2));
assert(not iskDecomposable(simplicialComplex {a*b*c, c*d*e}, 2));
///

-- Tests iskDecomposable (a second way)
-- See Example V6F10-{1,6,7} in S. Moriyama and F. Takeuchi, "Incremental construction properties in dimension two:
-- shellability, extendable shellability and vertex decomposability," Volume 263, Issue 1-3 (February 2003), 295-296.
TEST ///
R = QQ[a..f];
S1 = simplicialComplex {a*b*c, a*b*d, a*b*f, a*c*d, a*c*e, b*d*e, b*e*f, c*d*f, c*e*f, d*e*f};
S6 = simplicialComplex {a*b*c, a*b*d, a*b*e, a*c*d, a*c*f, b*d*e, b*e*f, c*d*f, c*e*f, d*e*f};
S7 = simplicialComplex {a*b*c, a*b*e, a*b*f, a*c*d, a*d*e, b*c*d, b*e*f, c*d*f, c*e*f, d*e*f};
assert(not isVertexDecomposable(S1));
assert(iskDecomposable(S1, 1));
assert(not isVertexDecomposable(S6));
assert(iskDecomposable(S6, 1));
assert(not isVertexDecomposable(S7));
assert(iskDecomposable(S7, 1));
///

-- Tests isSheddingFace
TEST ///
R = QQ[a..e];
S = simplicialComplex {a*b*c*d*e};
assert(isSheddingFace(S, a, 0));
assert(isSheddingFace(S, a, 3));
T = simplicialComplex {a*b*c, b*c*d, c*d*e};
assert(isSheddingFace(T, e, 2));
assert(not isSheddingFace(T, b*c*d, 2));
///

-- Tests allFaces
TEST ///
R = QQ[a,b,c];
assert(allFaces simplicialComplex {a*b*c} === {a, b, c, a*b, a*c, b*c, a*b*c});
assert(allFaces simplicialComplex {a*b} === {a, b, a*b});
assert(allFaces simplicialComplex {a, b*c} === {a, b, c, b*c});
///

end

-------------------
-- Demo Usage
-------------------
restart;
needsPackage "SimplicialDecomposability";
R = QQ[a..f];
S = simplicialComplex {a*b*c*d*e};
T = simplicialComplex {a*b*c, c*d*e};
U = simplicialComplex {a*b*c, b*c*d, c*d*e}; 
V = simplicialComplex {a*b*c, a*b*d, a*b*f, a*c*d, a*c*e, b*d*e, b*e*f, c*d*f, c*e*f, d*e*f};

-- We can delete faces
faceDelete(S, a)
faceDelete(S, a*b*c*d*e)
boundary(S)

-- We can ask if it's a simplex
isSimplex S
isSimplex T

-- We can get the hVector
hVector U
hVector V

-- We can ask if it's k-decomposable (& about shedding faces)
iskDecomposable(S, 0)
iskDecomposable(S, 1)
isSheddingFace(S, a*b*c)

iskDecomposable(V, 0)
iskDecomposable(V, 1)
isSheddingFace(V, d*e*f)

-- We can ask if it's shellable
isShellable T
isShellable U
shellingOrder U

-- We can do it in three ways: Recursively, Naively, and dDecomposability
time isShellable(V, Strategy => "Recursive") -- default
time isShellable(V, Strategy => "dDecomposable")
-- bad idea! 
--time isShellable(V, Strategy => "Naive")
(#flatten entries facets V)!
