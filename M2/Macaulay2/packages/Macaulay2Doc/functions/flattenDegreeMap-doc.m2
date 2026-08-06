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
        kk = ZZ/3
        S = kk[a]
        R = S[t]
        f = map(R, S)
        f' = flattenDegreeMap f
        target f'
        assert(target f' == target f)
        S' = source f'
        degreeGroup S'
        assert(degreeGroup S' == degreeGroup R)
        f'.cache.DegreeMap
        g = map(S, source f')
        -- canonical map is invertible
        g^-1
    Text
        Flattening the degree map in this way can be useful to preserve grading
        when computing push-forwards. See @TO (pushFwd, RingMap, Module)@.
SeeAlso
    flattenRing
    pushFwd

///