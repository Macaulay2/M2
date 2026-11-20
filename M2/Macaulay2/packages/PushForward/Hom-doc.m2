doc ///
Key
    (Hom, RingMap, Module, Module)
    (Hom, RingMap, Module, Ideal)
    (Hom, RingMap, Module, Ring)
    (Hom, RingMap, Ideal, Module)
    (Hom, RingMap, Ideal, Ideal)
    (Hom, RingMap, Ideal, Ring)
    (Hom, RingMap, Ring, Module)
    (Hom, RingMap, Ring, Ideal)
    (Hom, RingMap, Ring, Ring)
Headline
    pushed-forward module of homomorphisms
Usage
    Hom(f, M, N)
Inputs
    f: RingMap
        $S \rightarrow R$.
    M:{Ring,Ideal,Module}
        $R$-module, an ideal in $R$, or the ring $R$.
    N:{Ring,Ideal,Module}
        $R$-module, an ideal in $R$, or the ring $R$.
Description
    Text
        Given a ring map $f \colon S \to R$, and $R$-modules $M$ and $N$,
        this computes the module of homomorphisms $\textrm{Hom}_R(M, N)$ as
        an $S$-module using the module structure induced by $f$.

        In cases when $\textrm{Hom}_R(M, N)$ is an $R$-module - this
        includes the case when $R$ is a commutative ring - the computed
        module is isomorphic as an $S$-module to the push-forward of
        $\textrm{Hom}_R(M, N)$ along the map $f$.
    Example
        kk = ZZ/7
        R = kk[a..c, SkewCommutative => true]
        f = map(R, kk)
        M = module ideal {a*b + c}
        Hom(f, M, M)
    Text
        Given a matrix with $R$-coefficients representing an $R$-linear
        homomorphism, you can recover the corresponding element of
        $\textrm{Hom}_R(M, N)$ using the @TO (homomorphism', RingMap,
        Matrix)@ method.
    Example
        id_M
        homomorphism'(f, id_M)
    Text
        You can recover the matrix corresponding to an element of one of
        these Hom-modules using the @TO homomorphism@ method just as in the
        usual Hom construction.
    Example
        H = Hom(f, M, M)
        homomorphism H_0
        homomorphism H_1
        homomorphism H_2
        homomorphism H_3
    Text
        Note that in this non-commutative case, one can get nonsensical results without pushing-forward.
    Example
        h = homomorphism' id_M
        -- id_M should not correspond to the zero element
        assert(h == 0)
    Text
        While it's main utility is in computing Hom over a non-commutative
        ring such as an exterior algebra, this method can be used in the
        commutative case as well.
    Example
        kk = ZZ/13
        R = kk[x, y] / ideal {y^2}
        S = kk[a]
        f = map(R, S, {x^2})
        I = ideal vars R
        Hom(f, I, I/I^3)
    Text
        In the commutative case, this module is isomorphic to the
        push-forward of the usual Hom-module along $f$.
    Example
        H = pushFwd(f, Hom(I, I/I^3));
        H' = Hom(f, I, I/I^3);
        -- map from the pushFwd module by passing through the corresponding representing matrices
        images = for i from 0 to numgens H - 1 list homomorphism'(f, homomorphism pushforward' H_{i});
        phi = map(H', H, matrix {images})
        assert(phi^-1 * phi == id_H)
    Text
        As an implementation detail, this method computes $\textrm{Hom}_R(M, N)$
        as a submodule of $\textrm{Hom}_S(f_*M, f_*N)$.
    Example
        kk = ZZ/7;
        R = kk[a..c, SkewCommutative => true];
        f = map(R, kk);
        M = module ideal {a + b};
        N = module ideal {a*b + c};
        H = Hom(pushFwd(f, M), pushFwd(f, N));
        inducedMap(H, Hom(f, M, N))
    Text
        This method can be used to produce random homomorphisms between modules over skew-rings.
    Example
        kk = ZZ/101;
        R = kk[a..d, SkewCommutative => true];
        f = map(R, kk);
        M = R^1/ ideal {a*b, c+d}
        homomorphism random Hom(f, R^1, M)
SeeAlso
    Hom
    pushFwd
    (homomorphism', RingMap, Matrix)
    (Ext, ZZ, RingMap, Module, Module)
///

doc ///
Key
    (Hom, RingMap, Matrix, Matrix)
    (Hom, RingMap, Matrix, Module)
    (Hom, RingMap, Module, Matrix)
Headline
    induced map on Hom
Usage
    Phi = Hom(f, F, G)
Inputs
    f:RingMap
        $S \rightarrow R$
    F:Matrix
    G:Matrix
Outputs
    Phi:Matrix
        $\textrm{Hom}(f, \textrm{target} F, \textrm{source} G) \rightarrow \textrm{Hom}(f, \textrm{source} F, \textrm{target} G)$
Description
    Text
        Compute the induced map on pushed-forward Hom modules. See @TO (Hom, RingMap, Module, Module)@.
    Example
        kk = ZZ/101
        R = kk[a..d, SkewCommutative => true]
        f = map(R, kk)
        M = module ideal {a*b, c+d}
        N = R^1 /ideal {a+b, c*d}

        -- pick some random homomorphisms using the pushFwd hom construction
        F = homomorphism random Hom(f, R^1, M)
        G = homomorphism random Hom(f, R^1, N)

        HomFG = Hom(f, F, G)
        assert(source HomFG == Hom(f, target F, source G))
        assert(target HomFG == Hom(f, source F, target G))
    Text
        If a module $M$ is provided as an argument, then the identity map on $M$ is used.
    Example
        HFG' = Hom(f, F, R^1)
        assert(source HFG' == Hom(f, target F, R^1))
        assert(target HFG' == Hom(f, source F, R^1))

        HFG'' = Hom(f, R^1, G)
        assert(source HFG'' == Hom(f, R^1, source G))
        assert(target HFG'' == Hom(f, R^1, target G))
SeeAlso
    Hom
    pushFwd
    (Hom, Matrix, Matrix)
    (Hom, RingMap, Module, Module)
///

doc ///
Key
    (End, RingMap, Module)
    (End, RingMap, Ideal)
    (End, RingMap, Ring)
Headline
    pushed-forward module of endomorphisms
Usage
    End(f, M)
Inputs
    f:RingMap
        $S \rightarrow R$
    M:{Module, Ideal, Ring}
        $R$-module, $R$-ideal or the ring $R$ itself
Description
    Text
        Compute the the abelian group of endomorphisms of $M$ as an $S$-module.
    Example
        kk = ZZ/101
        R = kk[a, b] / b^2
        S = kk[t]
        f = map(R, S, {a^2})
        I = ideal {a^2 + b, a}
        M = module I;
        H = End(f, M)
        homomorphism H_0
    Text
        Ideals can be passed directly to this method and are turned into modules implicitly.
    Example
        End(f, I)
    Text
        Passing the ring $R$ is equivalent to passing the unit ideal or the module $R^1$
    Example
        -- look closely this is different from End(f, I) above!
        End(f, R)
    Text
        Can compute End over skew-commutative rings.
    Example
        kk = ZZ/101
        R = kk[a..d, SkewCommutative => true]
        f = pullbackDegrees map(R, kk)
        M = module ideal {a*b, c + d}
        prune End(f, M)
SeeAlso
    Hom
    pushFwd
    (Hom, RingMap, Module, Module)
///



doc ///
Key
    (homomorphism', RingMap, Matrix)
Headline
    identify the element of Hom corresponding to a matrix
Usage
    h = homomorphism'(f, F)
Inputs
    f:RingMap
        $S \rightarrow R$
    F:Matrix
        $M \rightarrow N$
Outputs
    h:Matrix
        $S^1 \rightarrow Hom(M, N)$
Description
    Text
        Identify the element of @TT "Hom(f, M, N)"@ corresponding to $F$. See @TO (Hom, RingMap, Module, Module)@.
SeeAlso
    Hom
    pushFwd
    (Hom, RingMap, Module, Module)
    homomorphism'
///


doc ///
Key
    (Ext, ZZ, RingMap, Module, Module)
    (Ext, ZZ, RingMap, Module, Ideal)
    (Ext, ZZ, RingMap, Module, Ring)
    (Ext, ZZ, RingMap, Ideal, Module)
    (Ext, ZZ, RingMap, Ideal, Ideal)
    (Ext, ZZ, RingMap, Ideal, Ring)
    (Ext, ZZ, RingMap, Ring, Module)
    (Ext, ZZ, RingMap, Ring, Ideal)
    (Ext, ZZ, RingMap, Ring, Ring)
Headline
    pushed-forward Ext module
Usage
    Ext^i(f, M, N)
Inputs
    i: ZZ
    f: RingMap
        $S \rightarrow R$.
    M:{Ring,Ideal,Module}
        $R$-module, an ideal in $R$, or the ring $R$.
    N:{Ring,Ideal,Module}
        $R$-module, an ideal in $R$, or the ring $R$.
Description
    Text
        Given a ring map $f \colon S \to R$, and $R$-modules $M$ and $N$,
        this computes the Ext module $\textrm{Ext}^i_R(M, N)$ as
        an $S$-module using the module structure induced by $f$.

        In cases when $\textrm{Ext}^i_R(M, N)$ is an $R$-module - this
        includes the case when $R$ is a commutative ring - the computed
        module is isomorphic as an $S$-module to the push-forward of
        $\textrm{Ext}^i_R(M, N)$ along the map $f$.
    Example
        kk = ZZ/7
        R = kk[a..c, SkewCommutative => true]
        f = map(R, kk)
        M = module ideal {a*b + c}
        E = Ext^1(f, M, M)
        prune E
    Text
        You can recover the complex corresponding to an element of one of
        this Ext-module using the @TO yonedaExtension@ method just as in the
        usual Ext construction.
    Example
        E = Ext^1(f, M, M);
        yonedaExtension E_0
        yonedaExtension E_1
        yonedaExtension E_2
    Text
        Given an extension complex, you can recover the corresponding element of
        $\textrm{Ext}^i_R(M, N)$ using the @TO (yonedaExtension', RingMap, Complex)@ method.
    Example
        e = yonedaExtension E_0
        e' = yonedaExtension'(f, e)
        assert(E_0 == e')
    Text
        While it's main utility is in computing Ext over a non-commutative
        ring such an exterior algebra, this method can be used in the
        commutative case as well.
    Example
        kk = ZZ/13
        R = kk[x, y] / ideal {y^2}
        S = kk[a]
        f = map(R, S, {x^2})
        I = ideal vars R
        E = Ext^1(f, I, I/I^3)
        prune E
    Text
        In the commutative case, this module is isomorphic to the
        push-forward of the usual Ext-module along $f$.
    Example
        E = pushFwd(f, Ext^1(I, I/I^3));
        E' = Ext^1(f, I, I/I^3);
        -- map from the pushFwd module by passing through the corresponding extension complexes
        images = for i from 0 to numgens E - 1 list yonedaExtension'(f, yonedaExtension pushforward' E_{i});
        phi = map(E', E, matrix {images})
        assert(phi^-1 * phi == id_E)
SeeAlso
    Ext
    pushFwd
    (yonedaExtension', RingMap, Complex)
    (Hom, RingMap, Module, Module)
///

doc ///
Key
    (yonedaExtension', RingMap, Complex)
Headline
    identify the element of pushed-forward Ext corresponding to an extension
Usage
    h = yonedaExtension'(f, C)
Inputs
    f:RingMap
        $S \rightarrow R$
    C:Complex
        of $R$-modules which is exact of length $d$
Outputs
    h:Matrix
        $S^1 \rightarrow Ext^d(C_0, C_d)$
Description
    Text
        Identify the element of @TT "Ext^d(f, C_0, C_d)"@ corresponding to $C$.
        See @TO (Ext, ZZ, RingMap, Module, Module)@.
SeeAlso
    Ext
    pushFwd
    yonedaExtension'
    (Hom, RingMap, Module, Module)
    (Ext, ZZ, RingMap, Module, Module)
///

doc ///
Key
    (yonedaMap', RingMap, ComplexMap)
Headline
    identifies the element of Ext corresponding to a map of free resolutions
Usage
    e = yonedaMap'(f, G)
Inputs
    f:RingMap
        $S \rightarrow R$
    G:ComplexMap
        of degree $-d$ from the free resolution of $M$ to the free
        resolution of $N$
Outputs
    e:Matrix
        over $S$, from $S^1$ to $\operatorname{Ext}^d_R(f, M, N)$,
        which represents the corresponding element in the Ext module
Description
    Text
        This is analogous to @TO yonedaMap'@ without passing the ring map $f$.
        The difference is that the Ext class produced is in the $S$-module @TT
        "Ext^d(f, M, N)"@. Indeed, the comparison map produced below shows that
        these modules are isomorphic over $S$ in a way which is consistent with
        @TT "yonedaMap'"@ and @TT "yonedaMap"@.
    Example
        kk = ZZ/11
        R = kk[s, t]/ ideal {s^2, t^3}
        f = map(R, kk)
        I = ideal vars R
        M = R^1/I
        N = I / I^3
        E = Ext^1(f, M, N)
        m = map(E, E, matrix {for i from 0 to numgens E - 1 list yonedaMap'(f, yonedaMap(E_{i}, LengthLimit => 2))})
        phi = inducedMap(E, E, m, Verify => true)
        assert(phi == id_E)
    Text
        Works for computing Ext classes over skew-commutative rings.
    Example
        kk = ZZ/11
        R = kk[s, t, SkewCommutative => true]
        f = map(R, kk)
        I = ideal vars R
        M = R^1/I
        N = I / I^3
        E = Ext^1(f, M, N)
        m = map(E, E, matrix {for i from 0 to numgens E - 1 list yonedaMap'(f, yonedaMap(E_{i}, LengthLimit => 2))})
        phi = inducedMap(E, E, m, Verify => true)
        assert(phi == id_E)
SeeAlso
    "Working with Ext"
    (yonedaMap', ComplexMap)
    (Ext, ZZ, RingMap, Module, Module)
    (yonedaMap, Matrix)
    (yonedaExtension, Matrix)
    (yonedaExtension', RingMap, Complex)
///