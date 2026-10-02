doc ///
Key
    flattenDegreeMap
    (flattenDegreeMap, RingMap)
Headline
    replace DegreeMap with identity
Usage
    f' = flattenDegreeMap f
Inputs
    f: RingMap
        $S \rightarrow R$
Outputs
    f': RingMap
        $S' \rightarrow R$
Description
    Text
        $S'$ is isomorphic to $S$ but with degrees and DegreeGroup updated to match the image of $S$ under $f$.
    Example
        kk = ZZ/3;
        S = kk[a];
        R = S[t];
        f = map(R, S);
        f' = flattenDegreeMap f
        assert(target f' === target f)
        S' = source f';
        describe S
        degreeGroup S
        describe S'
        degreeGroup S'
        assert(degreeGroup S' == degreeGroup R)
        assert(f'.cache.DegreeMap === identity)
        g = map(S, S')
        -- the canonical map S' -> S is an isomorphism
        g^-1
    Text
        Flattening the degree map in this way can be useful to preserve grading
        when computing push-forwards.
SeeAlso
    flattenRing
///