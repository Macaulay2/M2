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
        (Hom, RingMap, Matrix, Module)
    Headline
        induced map on Hom
    Usage
        Phi = Hom(f, G, H)
    Inputs
        f:RingMap
            $S \rightarrow R$
        G:Matrix
        H:Matrix
    Outputs
        Phi:Matrix
            $\textrm{Hom}(\textrm{target} F, \textrm{source} G) \rightarrow \textrm{Hom}(\textrm{source} F, \textrm{target} G)$
    Description
        Text
            Compute the induced map on pushed-forward Hom modules. See @TO (Hom, RingMap, Module, Module)@.
    SeeAlso
        Hom
        pushFwd
        (Hom, Matrix, Matrix)
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