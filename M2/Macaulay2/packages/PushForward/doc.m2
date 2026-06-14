doc ///
    Key
        PushForward
    Headline
        methods for computing and working with pushforwards along a ring map
    Description
        Text
            Given a ring map $f \colon A \to B$, and a $B$-module $M$,
            $M$ has the structure of an $A$-module, and if this module is
            finitely generated over $A$, the routine @TO pushFwd@ in this package
            will compute such an $A$-module. This is also functorial, in that if a
            map of $B$-modules (both of which are finitely generated over $A$), then
            @TO (pushFwd, RingMap, Matrix)@ will return the induced map
            on $A$-modules.

            In an algebraic sense, this is really a pull back, but thinking geometrically,
            the functions here implement the push forward of a module (or sheaf).
    Contributors
        This package was originally implemented by Claudiu Raicu, some changes were
        introduced by Karl Schwede, and later by David Eisenbud and Mike Stillman.
    Subnodes
        (pushFwd, RingMap)
        (pushFwd, RingMap, Module)
        (pushFwd, RingMap, Matrix)
///

-------------
-- pushFwd --
-------------
doc ///
    Key
        (pushFwd, RingMap)
        (pushFwd, Ring)
    Headline
        push forward of a finite ring map
    Usage
        pushFwd f
        pushFwd B	
    Inputs
        f:RingMap
	    or a ring B, and the map is taken to be the natural map from coefficientRing B
    Outputs
        :Sequence
    Description
        Text
            If $f: A \to B$ is a ring map, and $B$ is finitely generated as an $A$-module,
            then the function returns a sequence $(M, g, pf)$ containing
            (1) $M \cong B^1$ as $A$-modules,
            (2) a 1-row matrix $g$ of elements of B whose entries generate B as A-module,
            (3) a function $pf$ that
            assigns to each element $b \in B$, a matrix $A^1 \to M$,
            where the image of 1 is the element $b \in M$.
        Example
            kk = QQ;
            S = kk[a..d];
            I = monomialCurveIdeal(S, {1,3,4})
            B = S/I
            A = kk[a,d];
            f = map(B,A)
            (M,g,pf) = pushFwd f;
            M
            g
            use B
	    pf(a*b - c^2)
    Caveat
        This function is meant to be internally used.
    SeeAlso
        (pushFwd, RingMap, Module)
        (pushFwd, RingMap, Matrix)
///

doc ///
    Key
        (pushFwd, RingMap, Module)
        (pushFwd, Module)
    Headline
        push forward of a module along a ring-map
    Usage
        N = pushFwd(f, M)
        N = pushFwd M
    Inputs
        f:RingMap
            from a ring $A$ to a ring $B$
	 	    or the natural map from coefficientRing B if f not specified
        M:Module
            a $B$-module, which via $f$ is a finite $A$-module
    Outputs
        N:Module
    Description
        Text
            Given a ring map $f: A \to B$, a $B$-module $M$ can be considered
            as a module over $A$.  $M$ is finite as an $A$-module if and only if
            the induced map $A \rightarrow B / \textrm{ann}(M)$ is a finite map.  In
            this case, this method returns the corresponding $A$-module.
        Example
            kk = QQ;
            A = kk[t];
            B = kk[x, y]/(x*y);
            use B;
            i = ideal(x)
            f = map(B, A, {x})
            pushFwd(f, module i)
        Text
            Supports modules over skew-commutative rings.
        Example
            kk = QQ;
            A = kk[a..d, SkewCommutative => true]
            N = module ideal vars A
            M = pushFwd N
    SeeAlso
        (pushFwd, Matrix)
///

doc ///
    Key
        (pushFwd, RingMap, Matrix)
        (pushFwd, Matrix)	
    Headline
        push forward of a module map via a finite ring map
    Usage
        gA = pushFwd(f, g)
        gA = pushFwd g
    Inputs
        f:RingMap
            from a ring $A$ to a ring $B$
    	 	 or (if not specified) the natural map from A = coefficientRing ring g
        g:Matrix
            (a matrix), $g : M_1 \to M_2$ of modules over $B$
    Outputs
        gA:Matrix
    Description
        Text
            If $M_1$ and $M_2$ are both finite generated as $A$-modules, via $f$, this returns the induced map
            on $A$-modules.
        Example
            kk = QQ
            A = kk[a,b]
            B = kk[z,t]
            f = map(B,A,{z^2,t^2})
            M = B^1/ideal(z^3,t^3)
            g = map(M,M,matrix{{z*t}})
            p = pushFwd(f,g)
            source p == pushFwd(f, source g)
            target p == pushFwd(f, target g)
            kerg = pushFwd(f,ker g)
            kerp = prune ker p
        Example
            k = ZZ/32003
            A = k[x,y]/(y^4-2*x^3*y^2-4*x^5*y+x^6-y^7)
            A = k[x,y]/(y^3-x^7)
            B = integralClosure(A, Keep =>{})
                describe B
            f = map(B^1, B^1, matrix{{w_(3,0)}})
            g = pushFwd(icMap A, f)
            pushFwd(icMap A, f^2) == g*g

            A = kk[x]
            B = A[y, Join => false]/(y^3-x^7)
            pushFwd B^1
            pushFwd matrix y
        Text
            Pushforward is linear and respects composition:
    	Example
            A = kk[x];
            B = A[y,z,Join => false]/(y^3 - x*z, z^3-y^7);
            fy = pushFwd matrix y;
            fz = pushFwd matrix z;
            fx = pushFwd matrix x_B;
            g = pushFwd matrix y*z -x_B*z^2;
            g == fy*fz-fx*fz^2
            fz^3-fy^7 == 0
    SeeAlso
        (pushFwd, Module)
///

doc ///
    Key
        pushFwd
    Headline
        push forward
    Description
        Text
            The push forward functor.
    SeeAlso
        (pushFwd, RingMap)
        (pushFwd, RingMap, Module)
        (pushFwd, RingMap, Matrix)
        pushforward
        pushforward'
///

-----------------
-- pushforward --
-----------------
doc ///
    Key
        pushforward
    Headline
        map elements from a module to it's push-forward
    Description
        Text
            When you have computed the pushforward of a module along a ring map, the
            @TT "pushforward"@ method allows you to translate elements to this new
            module.
        Example
            kk = ZZ/101;
            A = kk[a,b] / ideal {a^2, b^3};
            f = map(A, kk);
            N = module A;
            M = pushFwd(f, N);
            pushforward(f, N_0)
        Text
            You can pushforward a matrix of elements with one call.
        Example
            kk = ZZ/101;
            A = kk[a,b] / ideal {a^2, b^3};
            f = map(A, kk);
            N = A^2;
            M = pushFwd(f, N);
            gensN = matrix N_*
            pushforward(f, gensN)
        Text
            If you have not already computed the pushFwd of your module, it will
            be computed on the fly and cached for later use.
        Example
            kk = ZZ/101;
            A = kk[a,b] / ideal {a^2, b^3};
            f = map(A, kk);
            N = module A;

            m = pushforward(f, N_0)
            module target m
        Text
            If you have used different options to create more than one pushFwd
            along a ring map. Then you have to specify the target pushFwd
            module.
        Example
            kk = ZZ/101;
            A = kk[a,b];
            f = map(A, kk);
            I = ideal vars A;
            N = I/I^3
            M = pushFwd(N, NoPrune => true)
            M' = pushFwd(N, NoPrune => false)
            try(pushforward(f, N_0)) -- this raises an error
            pushforward(M, N_0)
            pushforward(M', N_0)
        Text
            You can omit the ring map in which case the natural inclusion of the
            coefficient ring is used.
        Example
            kk = ZZ/101;
            A = kk[a,b] / ideal {a^2, b^3};
            pushforward(1 + 2*a + 3*b + 4*a*b)
        Text
            @TT "pushforward"@ and @TT "pushforward'"@ are inverse bijections
            (modulo the representation of elements as matrices or vectors).
        Example
            kk = ZZ/101;
            A = kk[a,b];
            f = map(A, kk);
            I = ideal vars A;
            N = I/I^2;
            M = pushFwd N;
            (matrix N_0, pushforward' pushforward N_0)
            (matrix M_*, pushforward pushforward' matrix M_*)
    SeeAlso
        pushforward'
///

doc ///
    Key
        (pushforward, Matrix)
    Headline
        map element from a module to it's push-forward
    Inputs
        X:Matrix
            $X: R^n \rightarrow N$ representing $n$ elements of $N$
    Outputs
        Y:Matrix
            $Y$ gives $n$ elements of the push-forward module
    Usage
        Y = pushforward(X)
    Description
        Text
            Pushes along implicit inclusion of coefficient ring.
        Example
            kk = ZZ/101;
            A = kk[a] / ideal {a^2 + 2};
            N = A^3;
            X = matrix N_0;
            pushforward X
///

doc ///
    Key
        (pushforward, Vector)
    Headline
        map element from a module to it's push-forward
    Inputs
        v:Vector
    Outputs
        Y:Matrix
            $Y$ gives an element of the push-forward module
    Usage
        Y = pushforward(v)
    Description
        Text
            Pushes along implicit inclusion of coefficient ring.
        Example
            kk = ZZ/101;
            A = kk[a] / ideal {a^2 + 2};
            N = A^3;
            v = N_0;
            pushforward vector v
///

doc ///
    Key
        (pushforward, RingElement)
    Headline
        map elements from a module to it's push-forward
    Inputs
        r:RingElement
    Outputs
        Y:Matrix
            $Y$ gives an element of the push-forward module
    Usage
        Y = pushforward(r)
    Description
        Text
            Pushes along implicit inclusion of coefficient ring.
        Example
            kk = ZZ/101;
            A = kk[a] / ideal {a^2 + 2};
            r = a;
            pushforward a
///

doc ///
    Key
        (pushforward, Module, Matrix)
    Headline
        map elements from a module to it's push-forward to an explicit module
    Inputs
        M:Module
            target of push-forward map
        X:Matrix
            to push to $M$
    Outputs
        Y:Matrix
    Usage
        Y = pushforward(M, X)
    Description
        Text
            Can specify an explicit push-forward module.
        Example
            kk = ZZ/101;
            A = kk[a] / ideal {a^2 + 2};
            M = pushFwd module A;
            pushforward(M, matrix a)
        Text
            If multiple push-forward have been created with the same ring-map
            this is required.
        Example
            kk = ZZ/101;
            A = kk[a,b];
            f = map(A, kk);
            I = ideal vars A;
            N = I/I^3;
            M = pushFwd(N, NoPrune => true);
            M' = pushFwd(N, NoPrune => false);
            X = matrix N_0
            try(pushforward(f, X)) -- this raises an error
            pushforward(M, X)
            pushforward(M', X)
///

doc ///
    Key
        (pushforward, Module, Vector)
    Headline
        map elements from a module to it's push-forward to an explicit module
    Inputs
        M:Module
            target of push-forward map
        v:Vector
            to push to $M$
    Outputs
        Y:Matrix
    Usage
        Y = pushforward(M, v)
    Description
        Text
            Can specify an explicit push-forward module.
        Example
            kk = ZZ/101;
            A = kk[a] / ideal {a^2 + 2};
            M = pushFwd module A;
            pushforward(M, a)
        Text
            If multiple push-forward have been created with the same ring-map
            this is required.
        Example
            kk = ZZ/101;
            A = kk[a,b];
            f = map(A, kk);
            I = ideal vars A;
            N = I/I^3
            M = pushFwd(N, NoPrune => true)
            M' = pushFwd(N, NoPrune => false)
            v = vector N_0
            try(pushforward(f, v)) -- this raises an error
            pushforward(M, v)
            pushforward(M', v)
///

doc ///
    Key
        (pushforward, Module, RingElement)
    Headline
        map elements from a module to it's push-forward to an explicit module
    Inputs
        M:Module
            target of push-forward map
        r:RingElement
            to push to $M$
    Outputs
        Y:Matrix
    Usage
        Y = pushforward(M, r)
    Description
        Text
            Can specify an explicit push-forward module.
        Example
            kk = ZZ/101;
            A = kk[a] / ideal {a^2 + 2};
            M = pushFwd module A;
            pushforward(M, a)
        Text
            If multiple push-forward have been created with the same ring-map
            this is required.
        Example
            kk = ZZ/101;
            A = kk[a,b]/ ideal {a^2, b^2};
            f = map(A, kk);
            M = first pushFwd(f, NoPrune => true)
            M' = first pushFwd(f, NoPrune => false)
            r = a + b
            try(pushforward(f, r)) -- this raises an error
            pushforward(M, r)
            pushforward(M', r)
///

doc ///
    Key
        (pushforward, RingMap, Matrix)
    Headline
        map elements from a module to it's push-forward along an explicit ring map
    Inputs
        f:RingMap
        X:Matrix
    Outputs
        Y:Matrix
    Usage
        Y = pushforward(f, X)
    Description
        Text
            If you have computed the push-forward of your module along different
            ring maps then pass the map to compute the push-forward of an
            element.
        Example
            kk = ZZ/101
            A = kk[a..c] / ideal {b^2, c^2}
            T = kk[t]
            f = map(A, T, {a^2})
            g = map(A, T, {a^3})
            X = matrix a + b + c
            pushforward(f, X)
            pushforward(g, X)
///

doc ///
    Key
        (pushforward, RingMap, Vector)
    Headline
        map elements from a module to it's push-forward along an explicit ring map
    Inputs
        f:RingMap
        v:Vector
    Outputs
        Y:Matrix
    Usage
        Y = pushforward(f, v)
    Description
        Text
            If you have computed the push-forward of your module along different
            ring maps then pass the map to compute the push-forward of an
            element.
        Example
            kk = ZZ/101
            A = kk[a..c] / ideal {b^2, c^2}
            T = kk[t]
            f = map(A, T, {a^2})
            g = map(A, T, {a^3})
            v = vector(a + b + c)
            pushforward(f, v)
            pushforward(g, v)
///

doc ///
    Key
        (pushforward, RingMap, RingElement)
    Headline
        map elements from a module to it's push-forward along an explicit ring map
    Inputs
        f:RingMap
        r:RingElement
    Outputs
        Y:Matrix
    Usage
        Y = pushforward(f, r)
    Description
        Text
            If you have computed the push-forward of your module along different
            ring maps then pass the map to compute the push-forward of an
            element.
        Example
            kk = ZZ/101
            A = kk[a..c] / ideal {b^2, c^2}
            T = kk[t]
            f = map(A, T, {a^2})
            g = map(A, T, {a^3})
            r = a + b + c
            pushforward(f, r)
            pushforward(g, r)
///
------------------
-- pushforward' --
------------------
doc ///
    Key
        pushforward'
    Headline
        map elements of a push-forward module back to the original
    Description
        Text
            Given an element of a module that is the push-forward of a module
            along a ring map, this gives the corresponding element in the
            original module.
        Example
            kk = ZZ/101;
            A = kk[a, b];
            I = ideal {a^2, b^2};
            N = I/I^2
            M = pushFwd N
            pushforward' M_0
        Text
            You can apply @TT "pusforward'"@ to a matrix of elements with one call.
        Example
            kk = ZZ/101;
            A = kk[a, b];
            I = ideal {a^2, b^2};
            N = I/I^2;
            M = pushFwd N
            pushforward' matrix M_*
        Text
            @TT "pushforward"@ and @TT "pushforward'"@ are inverse bijections
            (modulo the representation of elements as matrices or vectors).
        Example
            kk = ZZ/101;
            A = kk[a,b];
            f = map(A, kk);
            I = ideal vars A;
            N = I/I^2;
            M = pushFwd N;
            (matrix N_0, pushforward' pushforward N_0)
            (matrix M_*, pushforward pushforward' matrix M_*)
        Text
            If the argument is not an element of a module that is a
            push-forward, then an error is raised.
        Example
            kk = ZZ/101;
            A = kk[a];
            try(pushforward' a) -- raises an error
        Text
            Get the elements in the original module corresponding to a set of
            generators of the push-forward.
        Example
            kk = QQ;
            A = kk[a..d, SkewCommutative => true]
            M = pushFwd module A;
            pushforward' matrix M_*
    SeeAlso
        pushforward
///

 doc ///
    Key
        (pushforward', Vector)
    Inputs
        v:Vector
    Outputs
        Y:Matrix
    Usage
        Y = pushforward'(X)
    Headline
        map elements of a push-forward module back to the original
    Description
        Example
            kk = ZZ/101;
            A = kk[a,b] / ideal {a^2, b^3};
            f = map(A, kk);
            N = module A;
            M = pushFwd(f, N);
            v = vector M_0
            pushforward' v
///

 doc ///
    Key
        (pushforward', Matrix)
    Inputs
        X:Matrix
    Outputs
        Y:Matrix
    Usage
        Y = pushforward'(v)
    Headline
        map elements of a push-forward module back to the original
    Description
        Example
            kk = ZZ/101;
            A = kk[a,b] / ideal {a^2, b^3};
            f = map(A, kk);
            N = module A;
            M = pushFwd(f, N);
            X = matrix M_*
            pushforward' X
///


--------------------
-- isModuleFinite --
--------------------
doc ///
    Key
        isModuleFinite
        (isModuleFinite, RingMap)
        (isModuleFinite, Ring)
    Headline
        whether the target of a ring map is finitely generated over source
    Usage
        isModuleFinite f
        isModuleFinite R
    Inputs
        f:RingMap
            or $R$ @ofClass Ring@
    Outputs
        :Boolean
    Description
        Text
            A ring map $f \colon A \to B$ makes $B$ into a module over $A$.
            This method returns true if and only if this module is a finitely generated
            $A$-module.
        Example
            kk = QQ;
            A = kk[t];
            C = kk[x,y];
            B = C/(y^2-x^3);
            f = map(A, B, {t^2, t^3})
            isWellDefined f
            isModuleFinite f
        Example
            f = map(kk[x,y], A, {x+y})
            assert not isModuleFinite f
        Text
            If a ring $R$ is given, this method returns true if and only if $R$
            is a finitely generated module over its coefficient ring.
        Example
            A = kk[x]
            B = A[y]/(y^3+x*y+3)
            isModuleFinite B
    SeeAlso
        pushFwd
///

-------------
-- Options --
-------------
doc ///
Key
  NoPrune
  [pushFwd,NoPrune]
Headline
  NoPrune option for pushFwd
Description
 Text
  This is an optional argument for the @TO pushFwd@ function. Its default value is {\tt false},
  which means that the presentation of a pushed forward module is pruned by default. If NoPrune
  is set to {\tt true}, then the prune calls in pushFwd are turned off.
 Example
  R5=QQ[a..e]
  R6=QQ[a..f]
  M=coker genericMatrix(R6,a,2,3)
  G=map(R6,R5,{a+b+c+d+e+f,b,c,d,e})
  notpruned = pushFwd(G,M,NoPrune => true)
  pruned = pushFwd(G,M)
///
