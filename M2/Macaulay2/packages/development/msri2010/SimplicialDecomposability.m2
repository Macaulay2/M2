-------------------
-- Package Header
-------------------
needsPackage "Depth";
needsPackage "SimplicialComplexes";
newPackage (
    "SimplicialDecomposability",
    Version => "0.0.1",
    Date => "xx. January 2010",
    Authors => {{Name => "David W. Cook II", Email => "dcook@ms.uky.edu", HomePage => "http://www.ms.uky.edu/~dcook"}},
    Headline => "Pure k-Decomposability for simplicial complexes.",
    DebuggingMode => true
);
needsPackage "Depth";
needsPackage "SimplicialComplexes";

-------------------
-- Exports
-------------------
export {isShellable,
        isShelling,
        isSimplex,
        hVector,
        shellingOrder};

-------------------
-- Exported Code
-------------------

-- Determines if a pure simplicial complex is shellable.
isShellable = method(TypicalValue => Boolean);
isShellable (SimplicialComplex) := (S) -> (
    shellingOrder(S) != {}
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
    s0 := f0 := ta := null;
    s1 := simplicialComplex take(L, 1);
    di := dim s1;
    f1 := flatten for i from 0 to di list flatten entries faces(i, s1);
    -- for each face in the list
    for i from 2 to #L do (
        -- copy the last step
        s0 = s1;
        f0 = f1;
        -- update the newest
        s1 = simplicialComplex take(L, i);
        f1 = flatten for i from 0 to di list flatten entries faces(i, s1);
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

-- Determines the hVector of the Stanley-Reisner ideal of a simplicial complex.
hVector = method(TypicalValue => List);
hVector (SimplicialComplex) := (S) -> (
    flatten entries sub(last coefficients numerator reduceHilbert hilbertSeries ideal S, ZZ)
);

-- Attempts to find a shelling order of a pure simplicial complex.
shellingOrder = method(TypicalValue => List);
shellingOrder (SimplicialComplex) := (S) -> (
    --------------
    -- Easy checks
    --------------
    -- not pure => not pure shellable
    if not isPure S then return {};
    -- not CM => not shellable
    if not isCM quotient ideal S then return {};
    -- negatives in the h-Vector => not shellable
    if any(hVector S, i -> i<0) then return {};
    -- simplexes are nice
    if isSimplex S then return flatten entries facets S;

    --------------
    -- Naive build
    --------------
    -- TODO
    -- build up a shelling of S, pruning where possible
    -- remember that only the newest additions "shellosity"
    -- needs to be checked; i.e., it has a unique minimal
    -- element in the intersection
    {}
);

-------------------
-- Local-Only Code
-------------------

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
            This function currently uses the naive approach of checking all permutations of the facets.
        Example
            R = QQ[a,b,c,d,e];
            isShellable simplicialComplex {a*b*c*d*e}
            isShellable simplicialComplex {a*b*c,c*d*e}
            isShellable simplicialComplex {a*b*c,b*c*d,c*d*e}
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
            a shelling order of the facets of {\tt S}
    Description
        Example
            R = QQ[a,b,c,d,e];
            shellingOrder simplicialComplex {a*b*c*d*e}
            shellingOrder simplicialComplex {a*b*c, b*c*d, c*d*e}
            shellingOrder simplicialComplex {a*b*c, c*d*e}
///

end
-- Happy Happy Joy Joy!
