-------------------
-- Package Header
-------------------
needsPackage "SimplicialComplexes";
newPackage (
    "SimplicialDecomposability",
    Version => "0.9.1",
    Date => "19. January 2010",
    Authors => {{Name => "David W. Cook II",
                 Email => "dcook@ms.uky.edu",
                 HomePage => "http://www.ms.uky.edu/~dcook"}},
    Headline => "various decomposability routines for simplicial complexes",
    DebuggingMode => true
);
needsPackage "SimplicialComplexes";

-------------------
-- Exports
-------------------
export {
    allFaces,
    faceDelete,
    iskDecomposable,
    isSheddingFace,
    isSheddingVertex,
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

-- Returns all faces of a simplicial complex (except {}, the (-1)-face) up to a
-- given dimension
allFaces = method(TypicalValue => List);
allFaces (SimplicialComplex) := (S) -> (
    allFaces(S, dim S)
);
allFaces (SimplicialComplex, ZZ) := (S, k) -> (
    flatten for i from 0 to min(k, dim S) list flatten entries faces(i, S)
);

-- Face Deletion: Remove all faces of a complex containing the given face.
faceDelete = method(TypicalValue => SimplicialComplex);
faceDelete (SimplicialComplex, RingElement) := (S,F) -> (
    simplicialComplex (monomialIdeal S + monomialIdeal F)
);

-- Determines whether or not a pure simplicial complex is k-decomposable
-- Uses definition 2.1 in [PB].
iskDecomposable = method(TypicalValue => Boolean);
iskDecomposable (SimplicialComplex, ZZ) := (S, k) -> (
    -- k must be nonnegative and S must be pure
    if k < 0 and not isPure S then return false;
    -- base case: simplexes are k-decomposable for all nonnegative k
    if isSimplex S then return true;
    -- negatives in the h-Vector imply not k-decomposable for pure complexes
    if any(hVector S, i -> i<0) then return false;
    -- Check for shedding faces
    any(allFaces(S, k), F -> isSheddingFace(S, F, k))
);

-- Determines whether or not a face is a shedding face of a pure simplicial complex.
-- Uses definition 2.1 in [PB].
isSheddingFace = method(TypicalValue => Boolean);
isSheddingFace (SimplicialComplex, RingElement) := (S, F) -> (
    isSheddingFace(S, F, first degree F - 1)
);
isSheddingFace (SimplicialComplex, RingElement, ZZ) := (S, F, k) -> (
    if not isPure S then return false;
    iskDecomposable(link(S, F), k) and iskDecomposable(faceDelete(S, F), k)
);

-- Determines whether or not a vertex is a shedding vertex of a simplicial complex.
-- Uses definition 11.1 in [BW-2].
isSheddingVertex = method(TypicalValue => Boolean);
isSheddingVertex (SimplicialComplex, RingElement) := (S, x) -> (
    d := dim S;
    L := link(S, x);
    D := faceDelete(S, x);
    #((set first entries facets L) * (set first entries facets D)) == 0 and isVertexDecomposable(D) and isVertexDecomposable(L)
);

-- Determines whether or not a simplicial complex is shellable.
isShellable = method(TypicalValue => Boolean, Options => {Strategy => "Recursive"});
isShellable (SimplicialComplex) := options -> (S) -> (
    if isPure S and options.Strategy == "dDecomposable" then (
        -- Theorem 2.8 in [PB]: If S is pure, then
        -- S is (dim S)-decomposable if and only if S is shellable
        iskDecomposable(S, dim S)
    )
    else (
        -- otherwise pass the options to the shellingOrder routine
        shellingOrder(S, options) != {}
    )
);

-- Determines whether or not a list of faces is a shelling.
-- Uses definition III.2.1 in [St] for pure shellability.
isShelling = method(TypicalValue => Boolean);
isShelling (List) := (L) -> (
    -- Check for (i) monomials, (ii) squarefree, and (iii) monic
    if any(apply(L, size), i->i!=1) then return false;
    if max flatten flatten apply(L, exponents) > 1 then return false;
    if any(flatten flatten apply(L, i->entries (coefficients i)_1), i->i!=1) then return false;

    -- Sets with zero or one face are always shellings--they are simplices!
    if #L <= 1 then return true;

    -- Uses definition 2.1 in [BW-1] for impure shellability.
    if #unique apply(L, degree) > 1 then (
        -- Lemma 2.2 in [BW-1] shows dim L_0 == dim L, if L is a shelling
        if (max flatten apply(L, degree)) != first degree L_0 then return false;
        -- prime the loop
        S := fi := I := null;
        fa := set apply(drop(subsets support L_0, {0,0}), product);
        -- for each face in the list
        for i from 1 to #L - 1 do (
            -- get the next set of faces
            fi = set apply(drop(subsets support L_i, {0,0}), product);
            -- find simplicial complex of the intersection
            I = toList(fa * fi);
            -- handle the empty intersection case separately
            if #I == 0 then (
                if first degree L_i != 1 then return false;
            )
            else (
                S = simplicialComplex I;
                -- check it is pure and properly dimensional
                if not isPure S or dim S != (first degree L_i - 2) then return false;
            );
            -- update the union new step
            fa = fa + fi;
        );
    )
    -- Uses definition III.2.1. in [St] for pure shellability.
    else (
        -- prime the loop
        f0 := ta := null;
        f1 := set allFaces simplicialComplex take(L, 1);
        -- for each face in the list
        for i from 2 to #L do (
            -- copy the last step
            f0 = f1;
            -- update with the new step
            f1 = set allFaces simplicialComplex take(L, i);
            -- find the added faces & count their dimensions (+1)
            ta = tally flatten apply(toList(f1 - f0), degree);
            -- make sure the minimal face is unique
            if ta_(min keys ta) != 1 then return false;
        );
    );
    true
);

-- Determines whether or not a simplicial complex is a simplex.
isSimplex = method(TypicalValue => Boolean);
isSimplex (SimplicialComplex) := (S) -> (
    #flatten entries facets S <= 1
);

-- Determines whether or not a simplicial complex is vertex (0-) decomposable.
-- Uses definition 2.1 in [PB] for pure complexes.
-- Uses definition 11.1 in [BW-2] for impure complexes.
isVertexDecomposable = method(TypicalValue => Boolean);
isVertexDecomposable (SimplicialComplex) := (S) -> (
    -- base case: simplexes are vertex decomposable
    if isSimplex S then return true;
    -- pure cases are handled otherwise
    if isPure S then return iskDecomposable(S, 0);
    -- Check for shedding vertices
    any(first entries faces(0, S), x -> isSheddingVertex(S, x))
);

-- Determines the hVector of the Stanley-Reisner ideal of a simplicial complex.
hVector = method(TypicalValue => List);
hVector (SimplicialComplex) := (S) -> (
    flatten entries sub(last coefficients numerator reduceHilbert hilbertSeries ideal S, ZZ)
);

-- Attempts to find a shelling order of a simplicial complex.
-- Uses definition III.2.1 in [St].
shellingOrder = method(TypicalValue => List, Options => {Strategy => "Recursive"});
shellingOrder (SimplicialComplex) := options -> (S) -> (
    -- negatives in the h-Vector imply not shellable for pure complexes
    if isPure S and any(hVector S, i -> i<0) then return {};

    if options.Strategy == "Naive" then (
        -- NAIVE: simply look at all facet permutations
        P := permutations first entries facets S;
        for L in P do if isShelling L then return L;
    )
    else (
        -- RECURSIVE: start the recursion
        L := flatten entries facets S;
        O := {};
        -- the pure case is easier, so separate it
        if isPure S then (
            O = recursivePureShell({}, L);
            if O != {} then return O;
        )
        else (
            d := 1 + dim S;
            for i from 0 to #L - 1 do (
                -- Lemma 2.2 in [BW-1] shows dim L_0 == dim L, if L is a shelling
                -- So we can restrict ourselves to look only at those.
                if first degree L_i == d then (
                    O = recursiveImpureShell({L_i}, drop(L, {i,i})); 
                    if O != {} then return O;
                );
            );
        );
    );
    {}
);

-------------------
-- Local-Only Code
-------------------

-- Build up a (impure) shelling recursively.
-- Called by shellingOrder with Strategy => "Recursive"
-- Uses definition 2.1 in [BW-1].
recursiveImpureShell = method(TypicalValue => List);
recursiveImpureShell (List, List) := (O, P) -> (
    -- if it's "obvious", then keep going
    OisShelling := true;
    if #O > 1 then (
        -- the previous step is a shelling, but is the newest step?
        fa := set allFaces simplicialComplex drop(O, -1);
        Oi := take(O, -1);
        fi := set allFaces simplicialComplex Oi;
        I := toList(fa * fi);
        -- handle the empty intersection case separately
        if #I == 0 then (
            OisShelling = (first degree first Oi == 1);
        )
        else (
            S := simplicialComplex toList(fa * fi);
            -- check it is pure and properly dimensional
            OisShelling = (isPure S and dim S == (first degree first Oi - 2));
        );
    );
    if OisShelling then (
        -- Nothing else to add: we're done
        if P == {} then return O;
        -- Recurse until success, if possible
        Q := {};
        for i from 0 to #P - 1 do (
            Q = recursiveImpureShell(append(O, P_i), drop(P, {i,i}));
            if Q != {} then return Q;
        );
    );
    {}
);

-- Build up a (pure) shelling recursively.
-- Called by shellingOrder with Strategy => "Recursive"
-- Uses definition III.2.1 in [St].
recursivePureShell = method(TypicalValue => List);
recursivePureShell (List, List) := (O, P) -> (
    -- if it's "obvious", then keep going
    OisShelling := true;
    if #O > 1 then (
        -- the previous step is a shelling, but is the newest step?
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
            Q = recursivePureShell(append(O, P_i), drop(P, {i,i}));
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
        various decomposability routines for simplicial complexes.
    Description
        Text
            This package includes routines for vertex decomposability and
            shellability for arbitrary simplicial complexes as well as routines
            for k-decomposability for pure simplicial complexes.  Moreover, it
            can find a shelling order for a shellable simplicial complex.

            References:
            
            [BW-1] A. Bjoerner and M. Wachs, "Shellable nonpure complexes and
            posets, I," Trans. of the AMS 348 (1996), 1299--1327.
            
            [BW-2] A. Bjoerner and M. Wachs, "Shellable nonpure complexes and
            posets, II," Trans. of the AMS 349 (1997), 3945--3975.
            
            [MT] S. Moriyama and F. Takeuchi, "Incremental construction
            properties in dimension two: shellability, extendable shellability
            and vertex decomposability," Discrete Math. 263 (2003), 295--296.
            
            [PB] J. S. Provan and L. J. Billera, "Decompositions of Simplicial
            Complexes Related to Diameters of Convex Polyhedra," Math. of
            Operations Research 5 (1980), 576--594.
            
            [St] R. Stanley, "Combinatorics and Commutative Algebra," 2nd
            edition.  Progress in Mathematics, 41. Birkhaeuser Boston, Inc.
            Boston, MA, 1996.
///

doc ///
    Key
        allFaces
        (allFaces, SimplicialComplex)
        (allFaces, SimplicialComplex, ZZ)
    Headline
        returns all faces of a simplicial complex, up to a given dimension
    Usage
        allFaces S
        allFaces(S, 2)
    Inputs
        S:SimplicialComplex
        k:ZZ
            the highest dimension to return (dim {\tt S} by default)
    Outputs
        L:List
            the list of all faces of {\tt S} (excluding the (-1)-dimensional face {})
            up to dimension {\tt k}
    Description
        Example
            R = QQ[a..e];
            S = simplicialComplex {a*b*c*d*e};
            allFaces S
            allFaces(S, 2)
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
            the simplicial complex of all faces in {\tt S} not containing the
            face {\tt F}
    Description
        Example
            R = QQ[a..e];
            S = simplicialComplex {a*b*c*d*e};
            faceDelete(S, a)
            faceDelete(S, a*b*c)
            faceDelete(S, a*b*c*d*e) == boundary S
    Caveat
        Do not confuse face deletion with normal deletion.
    SeeAlso
        link
///

doc ///
    Key
        iskDecomposable
        (iskDecomposable, SimplicialComplex, ZZ)
    Headline
        determines whether or not a pure simplicial complex is k-decomposable
    Usage
        iskDecomposable(S, k)
    Inputs
        S:SimplicialComplex
            which is pure
        k:ZZ
    Outputs
        B:Boolean
            true if and only if {\tt S} is {\tt k}-decomposable
    Description
        Text
            Definition 2.1 of [PB] states that a pure simplicial complex {\tt S}
            is {\tt k}-decomposable if {\tt S} is either a simplex or there
            exists a shedding face {\tt F} of {\tt S} of dimension at most
            {\tt k}.
        Example
            R = QQ[a..f];
            iskDecomposable(simplicialComplex {a*b*c*d*e*f}, 0)
            iskDecomposable(simplicialComplex {a*b*c, b*c*d, c*d*e}, 2)
    SeeAlso
        faceDelete
        isSheddingFace
        isShellable
        isVertexDecomposable
        link
///

doc ///
    Key
        isSheddingFace
        (isSheddingFace, SimplicialComplex, RingElement)
        (isSheddingFace, SimplicialComplex, RingElement, ZZ)
    Headline
        determines whether or not a face of a pure simplicial complex is a shedding face
    Usage
        isSheddingFace(S, F)
        isSheddingFace(S, F, k)
    Inputs
        S:SimplicialComplex
            which is pure
        F:RingElement
            a face of {\tt S}
        k:ZZ
            the dimension of the shedding nature
    Outputs
        B:Boolean
            true if and only if {\tt F} is a shedding face of {\tt S} in
            dimension {\tt k} (dim {\tt F}, if {\tt k} undefined)
    Description
        Text
            Definition 2.1 of [PB] states that:  A shedding face {\tt F} of
            a pure simplicial complex {\tt S} is a face such that the face 
            deletion and link of {\tt F} from {\tt S} are both {\tt k}-decomposable.
        Example
            R = QQ[a..d];
            S = simplicialComplex {a*b*c*d};
            isSheddingFace(S, a)
            isSheddingFace(S, a*b)
    SeeAlso
        faceDelete
        iskDecomposable
        isSheddingFace
        isShellable
        isVertexDecomposable
        link
///

doc ///
    Key
        isSheddingVertex
        (isSheddingVertex, SimplicialComplex, RingElement)
    Headline
        determines whether or not a vertex of a simplicial complex is a shedding vertex
    Usage
        isSheddingVertex(S, x)
    Inputs
        S:SimplicialComplex
        x:RingElement
            a vertex of {\tt S}
    Outputs
        B:Boolean
            true if and only if {\tt x} is a shedding vertex of {\tt S}
    Description
        Text
            Definition 11.1 of [BW-2] states that:  A shedding vertex {\tt x}
            of a simplicial complex {\tt S} is a vertex such that the link and
            face deletion of {\tt x} from {\tt S} are vertex decomposable and
            share no common facets.
        Example
            R = QQ[a..f];
            S = simplicialComplex {a*b*c, c*d, d*e, e*f, d*f};
            isSheddingVertex(S, a)
            isSheddingVertex(S, f)
    SeeAlso
        faceDelete
        isVertexDecomposable
        link
///

doc ///
    Key
        isShellable
        (isShellable, SimplicialComplex)
    Headline
        determines whether or not a simplicial complex is shellable
    Usage
        isShellable S
    Inputs
        S:SimplicialComplex
    Outputs
        B:Boolean
            true if and only if {\tt S} is shellable
    Description
        Text
            The pure and impure cases are handled separately.  If {\tt S} is
            pure, then definition III.2.1 in [St] is used.  That is, {\tt S} is
            shellable if its facets can be ordered {\tt F_1, .., F_n} so that
            the difference in the {\tt j}th and {\tt j-1}th subcomplex has a 
            unique minimal face, for {\tt 2 <= j <= n}.

            If {\tt S} is impure, then definition 2.1 in [BW-1] is used.  Namely:
            A simplicial complex {\tt S} is shellable if the facets of {\tt S}
            can be ordered {\tt F_1, .., F_n} such that the intersection of the
            faces of the first {\tt j-1} with the faces of the {\tt F_j} is
            pure and dim {\tt F_j - 1}-dimensional.

            This function takes an optional Strategy which determines how the
            method works.  The options are those available in shellingOrder           
            (in particular, "Recursive" is the default) and also, when {\tt S}
            is pure, then "dDecomposable" which checks if {\tt S} is
            {\tt dim S}-decomposable, after Theorem 2.8 in [PB].
        Example
            R = QQ[a..f];
            isShellable simplicialComplex {a*b*c*d*e}
            isShellable simplicialComplex {a*b*c, c*d*e}
            isShellable simplicialComplex {a*b*c, b*c*d, c*d*e}
            isShellable simplicialComplex {a*b*c, c*d, d*e, e*f, d*f}
            isShellable simplicialComplex {a*b*c, c*d, d*e*f}
    SeeAlso
        facets
        iskDecomposable
        isShelling
        shellingOrder
///

doc ///
    Key
        isShelling
        (isShelling, List)
    Headline
        determines whether or not a list of faces is a shelling
    Usage
        isShelling L
    Inputs
        L:List
            a list of faces (i.e., squarefree monic monomials)
    Outputs
        B:Boolean
            true if and only if {\tt L} is shelling
    Description
        Text
            Determines if a list of faces is a shelling order of the
            simplicial complex generated by the list.
        Example
            R = QQ[a..e];
            isShelling {a*b*c, b*c*d, c*d*e}
            isShelling {a*b*c, c*d*e, b*c*d}
    SeeAlso
        isShellable
        shellingOrder
///

doc ///
    Key
        isSimplex
        (isSimplex, SimplicialComplex)
    Headline
        determines whether or not a simplicial complex is simplex
    Usage
        isSimplex S
    Inputs
        S:SimplicialComplex
    Outputs
        B:Boolean
            true if and only if {\tt S} is simplex
    Description
        Example
            R = QQ[a..d];
            isSimplex simplicialComplex {a*b*c*d}
            isSimplex simplicialComplex {a*b}
            isSimplex simplicialComplex {a*b, c*d}
    SeeAlso
        facets
///

doc ///
    Key
        isVertexDecomposable
        (isVertexDecomposable, SimplicialComplex)
    Headline
        determines whether or not a simplicial complex is vertex-decomposable
    Usage
        isVertexDecomposable S
    Inputs
        S:SimplicialComplex
    Outputs
        B:Boolean
            true if and only if {\tt S} is vertex-decomposable
    Description
        Text
            Vertex-decomposability is just zero-decomposability when {\tt S} is
            pure, see [PB].  When {\tt S} is impure, [BW-2] gives an alternate
            definiton (which is equivalent for pure complexes):  A complex
            {\tt S} is vertex decomposable if it is either a simplex or there
            exists a shedding vertex.
        Example
            R = QQ[a..f];
            isVertexDecomposable simplicialComplex {a*b*c*d*e}
            isVertexDecomposable boundary simplicialComplex {a*b*c*d*e}
            isVertexDecomposable simplicialComplex {a*b*c, c*d*e}
            isVertexDecomposable simplicialComplex {a*b*c, c*d, d*e, e*f, d*f}
    SeeAlso
        iskDecomposable
        isSheddingVertex
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
            R = QQ[a..d];
            hVector simplicialComplex {a*b*c,d}
    SeeAlso
        fVector
///

doc ///
    Key
        shellingOrder
        (shellingOrder, SimplicialComplex)
    Headline
        finds a shelling of a simplicial complex, if one exists
    Usage
        L = shellingOrder S
    Inputs
        S:SimplicialComplex
    Outputs
        L:List
            a shelling order of the facets of {\tt S}, if one exists
    Description
        Text
            This method behaves differently depending on the Strategy passed
            as an option.  The default strategy is "Recursive" which attempts
            to recursively find a shelling order.  An alternate strategy is
            "Naive" which simply checks all permutations of the facets with
            the isShelling routine until one is found, if one exists.
        Example
            R = QQ[a..f];
            shellingOrder simplicialComplex {a*b*c*d*e}
            shellingOrder simplicialComplex {a*b*c, b*c*d, c*d*e}
            shellingOrder simplicialComplex {a*b*c, c*d*e}
            shellingOrder simplicialComplex {a*b*c, c*d, d*e, e*f, d*f}
            shellingOrder simplicialComplex {a*b*c, c*d, d*e*f}
    Caveat
        The Naive strategy uses a tremendous amount of memory, exponential
        in the number of facets.
    SeeAlso
        isShellable
        isShelling
///

-------------------
-- Tests
-------------------

-- Tests allFaces
TEST ///
R = QQ[a,b,c];
assert(allFaces simplicialComplex {a*b*c} === {a, b, c, a*b, a*c, b*c, a*b*c});
assert(allFaces simplicialComplex {a*b} === {a, b, a*b});
assert(allFaces simplicialComplex {a, b*c} === {a, b, c, b*c});
assert(allFaces(simplicialComplex {a*b*c}, 1) === {a, b, c, a*b, a*c, b*c});
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
assert(iskDecomposable(boundary S, 0)); -- prop 2.2 in Provan-Billera
assert(iskDecomposable(simplicialComplex {a*b*c, b*c*d, c*d*e}, 2));
assert(not iskDecomposable(simplicialComplex {a*b*c, c*d*e}, 2));
///

-- Tests iskDecomposable: see Examples V6F10-{1,6,7} in [MT].
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
assert(isSheddingFace(S, a*b, 3));
T = simplicialComplex {a*b*c, b*c*d, c*d*e};
assert(isSheddingFace(T, e, 2));
assert(not isSheddingFace(T, b*c*d, 2));
///

-- Tests isSheddingVertex
TEST ///
R = QQ[a..f];
S = simplicialComplex {a*b*c, c*d, d*e, e*f, d*f};
assert(not isSheddingVertex(S, a));
assert(isSheddingVertex(S, f));
///

-- Tests of isShellable (and hence shellingOrder by invocation): Strategy => "Recursive"
-- NB: shellingOrder can only be tested this way as a shelling order need not be unique.
TEST ///
R = QQ[a..f];
-- Extreme cases
assert(isShellable simplicialComplex {a*b*c*d*e});
assert(isShellable simplicialComplex monomialIdeal {a,b,c,d,e}); -- empty complex
-- The following are from [St], Example 2.2.
assert(isShellable simplicialComplex {a*b*c, b*c*d, c*d*e});
assert(not isShellable simplicialComplex {a*b*c, c*d*e});
-- The following are from [BW-1], Figure 1.
assert(isShellable simplicialComplex {a*b, c});
assert(not isShellable simplicialComplex {a*b, c*d});
assert(isShellable simplicialComplex {a*b*c, c*d, d*e, e*f, d*f});
assert(not isShellable simplicialComplex {a*b*c, c*d, d*e*f});
///

-- Tests of isShellable (and hence shellingOrder by invocation): Strategy => "Naive"
-- Only small cases are used, as Naive takes a tremendous amount of memory.
TEST ///
R = QQ[a..f];
-- Extreme cases
assert(isShellable(simplicialComplex {a*b*c*d*e}, Strategy => "Naive"));
-- The following are from [St], Example 2.2.
assert(isShellable(simplicialComplex {a*b*c, b*c*d, c*d*e}, Strategy => "Naive"));
assert(not isShellable(simplicialComplex {a*b*c, c*d*e}, Strategy => "Naive"));
-- The following are from [BW-1], Figure 1.
assert(isShellable(simplicialComplex {a*b, c}, Strategy => "Naive"));
assert(not isShellable(simplicialComplex {a*b*c, c*d, d*e*f}, Strategy => "Naive"));
///

-- Tests of isShelling
TEST ///
R = QQ[a..f];
assert(isShelling {a*b*c*d*e*f});
-- The following are from [St], Example 2.2.
assert(isShelling {a*b*c, b*c*d, c*d*e});
assert(not isShelling {a*b*c, c*d*e});
-- The following are from [BW-1], Figure 1.
assert(isShelling {a*b, c});
assert(not isShelling {a*b, c*d});
assert(isShelling {a*b*c, c*d, d*e, e*f, d*f});
assert(not isShelling {a*b*c, c*d, d*e*f});
///

-- Tests of isSimplex
TEST ///
R = QQ[a..d];
-- Simplices
assert(isSimplex simplicialComplex {a*b*c*d});
assert(isSimplex simplicialComplex {a*b*c});
assert(isSimplex simplicialComplex {a*b});
assert(isSimplex simplicialComplex {a});
assert(isSimplex simplicialComplex monomialIdeal {a,b,c,d}); -- empty complex
-- Non-simplices
assert(not isSimplex simplicialComplex {a*b, b*c, c*d});
assert(not isSimplex simplicialComplex {a*b, b*c*d});
assert(not isSimplex simplicialComplex {a*b, c});
assert(not isSimplex simplicialComplex {a, b, c, d});
///

-- Tests of isVertexDecomposable (for impure complexes only, pure complexes are checked
-- when checking iskDecomposable(-, 0)).
TEST ///
R = QQ[a..f];
-- The following are from [BW-1], Figure 1.
assert(isVertexDecomposable simplicialComplex {a*b, c});
assert(isVertexDecomposable simplicialComplex {a*b*c, c*d, d*e, e*f, d*f});
assert(not isVertexDecomposable simplicialComplex {a*b*c, c*d, d*e*f});
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

end
