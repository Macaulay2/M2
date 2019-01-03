doc ///
    Key
        canonicalIdeal
        (canonicalIdeal, Ring)
        [canonicalIdeal, Attempts]
    Headline
        given a ring, produces an ideal isomorphic to the canonical module
    Usage
        canonicalIdeal(R)
    Inputs
        R:Ring
        Attempts => ZZ
            how many times the function should try to embed the canonical module as an ideal before giving up
    Outputs
        :Ideal
    Description
        Text
            Given a ring $R$, typically a domain, this produces an ideal isomorphic to the canonical module of $R$.  This will not always produce the same ideal, especially in a non-domain.  It uses the function {\tt embedAsIdeal} from {\tt Divisor.m2}.
        Example
            S = QQ[x,y,u,v];
            T = QQ[a,b];
            f = map(T, S, {a^3, a^2*b, a*b^2, b^3});
            R = S/(ker f);
            canonicalIdeal(R)
        Text
            Here's an example in a non-domain.
        Example
            R = ZZ/13[x,y,z]/ideal(x*y, x*z, y*z);
            canonicalIdeal(R)
        Text
            The option {\tt Attempts} is passed to an internal function which embeds the canonical module as an ideal.  This tells it how many times to try before giving up.
///

doc ///
    Key
        frobeniusTraceOnCanonicalModule
    Headline
        finds the u, which in a polynomial ring, determines the Frobenius trace on the canonical module of a quotient of that ring
    Usage
        frobeniusTraceOnCanonicalModule(canIdeal, defIdeal)
    Inputs
        canIdeal:Ideal
            a ring representing the canonical ideal
        defIdeal:Ideal
            the defining ideal of the ring
    Outputs
        :RingElement
    Description
        Text
            Given $R = S/I$, where $S$ is a polynomial ring, there is a map from the canonical module of $R$ back to itself, dual to the Frobenius: $\omega_R^{1/p^e} \to \omega_R$.
            By embedding $\omega_R$ as an ideal $J$ of $R$, one can interpret this map as a $p^e$-inverse linear map on $S$.  But every $p$ inverse linear map on $S$ is a premultiple of the dual to Frobenius on $S$, by some element $u$.  This function finds the $u$.
        Text
            However, because Macaulay2 does not always properly identify an ideal as principal (even though it is), sometimes we cannot find this single $u$ and instead find a list of $u$s, a linear combination of which is the desired $u$.
        Text
            Specifically, you pass this function two ideals.  First, an ideal that restricts to the canonical ideal $J \subseteq R$, and an ideal $I$ that defines the $R$ as a quotient of $S$.  The canonical ideal should be chosen so that it contains the defining ideal (if you do not do this, there may be unexpected behavior).
        Example
            S = ZZ/5[x,y,z,w];
            T = ZZ/5[a,b];
            f = map(T, S, {a^3, a^2*b, a*b^2, b^3});
            defIdeal = ker f;
            R = S/defIdeal;
            J = canonicalIdeal(R);
            canIdeal = sub(J, S) + defIdeal;
            frobeniusTraceOnCanonicalModule(canIdeal, defIdeal)
///

doc ///
    Key
        testModule
        (testModule, Ring)
        (testModule, Ring, Ideal)
        (testModule, Number, RingElement)
        (testModule, Number, RingElement, Ideal, List)
        (testModule, List, List)
        (testModule, List, List, Ideal, List)
        [testModule, AssumeDomain]
        [testModule, FrobeniusRootStrategy]
    Headline
        finds the parameter test module of a reduced ring
    Usage
        testModule(R)
        testModule(R, canIdeal)
        testModule(tt, ff)
        testModule(tt, ff, canIdeal, u1)
    Inputs
        R:Ring
        canIdeal:Ideal
            an ideal isomorphic to the canonical module
        tt:QQ
            the formal exponent that ff is raised to
        u1:List
            a list of elements describing the map on hte canonical module
        AssumeDomain => Boolean
            assume whether the ring passed is an integral domain
        FrobeniusRootStrategy => Symbol
            choose the strategy for internal frobeniusRoot calls
    Outputs
        :Sequence
    Description
        Text
            Computes the parameter test module (as a submodule of the canonical module).  The function returns three values, the parameter test submodule, the canonical module of which it is a subset, and the element $u$ (or $u$s) used to compute this ideal via the method @TO frobeniusTraceOnCanonicalModule@.
        Example
            R = ZZ/7[x,y,z]/ideal(x^3+y^3+z^3);
            testModule(R)
        Text
            The canonical module returned is always embedded as an ideal of $R$ (not of the ambient polynomial ring).  Likewise the parameter test submodule is then viewed as a subideal of that.
            With this in mind, because the example above is a Gorenstein ring, the ambient canonical module is the unit ideal.  The next example is not Gorenstein.
        Example
            S = ZZ/3[x,y,u,v];
            T = ZZ/3[a,b];
            f = map(T, S, {a^3, a^2*b, a*b^2, b^3});
            R = S/(ker f);
            testModule(R)
        Text
            Note that the output in this case has the parameter test module equal to the canonical module, as it should be.  Let's consider a non-Gorenstein example which is not F-rational.
        Example
            R = ZZ/5[x,y,z]/ideal(y*z, x*z, x*y);
            paraTestMod = testModule(R)
            (paraTestMod#0) : (paraTestMod#1)
        Text
            This function can be used to compute parameter test ideals in Cohen-Macaulay rings
        Example
            S=ZZ/2[X_1..X_5];
            E=matrix {{X_1,X_2,X_2,X_5},{X_4,X_4,X_3,X_1}};
            I=minors(2,E);
            tau=testModule(S/I);
            (tau#0):(tau#1)
        Text
            This function can also be used to compute the parameter test module of a pair $(R, f^t)$.
        Example
            R = ZZ/7[x,y];
            f = y^2 - x^3;
            testModule(5/6, f)
            testModule(5/7, f)
        Text
            This can also be used to compute $(R, f^s g^t)$.
        Example
            R = ZZ/7[x,y];
            f = y^2 - x^3;
            g = x^2 - y^3;
            testModule({1/2, 1/2}, {f, g})
        Text
            Sometimes you would like to specify the ambient canonical module (and choice of u) across multiple calls of testModule.  Those are what the $canIdeal$ or $u1$ can be used to specify.  Finally, the option {\tt FrobeniusRootStrategy} is passed to any calls of @TO frobeniusRoot@ and the option {\tt AssumeDomain} is used when computing a test element.
    SeeAlso
        testIdeal
///

doc ///
    Key
        parameterTestIdeal
        (parameterTestIdeal, Ring)
        [parameterTestIdeal, FrobeniusRootStrategy]
    Headline
        computes the parameter test ideal of a Cohen-Macaulay ring
    Usage
        parameterTestIdeal(R)
    Inputs
        R:Ring
        FrobeniusRootStrategy=>Symbol
            choose the strategy for internal frobeniusRoot calls
    Outputs
        :Ideal
    Description
        Text
            This computes the parameter test ideal of a Cohen-Macaulay ring.  Technically, it computes $\tau(\omega) : \omega$ where $\omega$ is a canonical module and $\tau(\omega)$ it the (parameter) testModule as computed by @TO testModule@.  For example, the following example is F-rational and so has trivial parameter test ideal.
        Example
            T = ZZ/5[x,y];
            S = ZZ/5[a,b,c,d];
            g = map(T, S, {x^3, x^2*y, x*y^2, y^3});
            R = S/(ker g);
            parameterTestIdeal(R)
        Text
            Consider now a non-F-rational Gorenstein ring where the @TO testIdeal@ and parameterTestIdeal coincide.
        Example
            R = ZZ/7[x,y,z]/ideal(x^3+y^3+z^3);
            parameterTestIdeal(R)
            testIdeal(R)
    SeeAlso
        testModule
        testIdeal
///


doc ///
    Key
        isCohenMacaulay
        (isCohenMacaulay, Ring)
        [isCohenMacaulay, IsLocal]
    Headline
        determines if a ring is Cohen-Macaulay
    Usage
        isCohenMacaulay(R)
    Inputs
        R:Ring
        IsLocal => Boolean
            instead call the isCM function from the Depth package which checks if the ring is Cohen-Macaulay only at the origin.
    Outputs
        :Boolean
    Description
        Text
            Determines if a ring is Cohen-Macaulay.  If you pass the {\tt IsLocal parameter}, this will simply call the @TO isCM@ function in the {\tt Depth} package, which checks whether the ring is Cohen-Macaulay at the origin.  This function checks the Cohen-Macaulay property globally and sometimes is much faster than the local computation.
        Example
            T = ZZ/5[x,y];
            S = ZZ/5[a,b,c,d];
            g = map(T, S, {x^3, x^2*y, x*y^2, y^3});
            R = S/(ker g);
            isCohenMacaulay(R)
        Example
            R = QQ[x,y,u,v]/(ideal(x*u, x*v, y*u, y*v));
            isCohenMacaulay(R)
        Text
            The function works as follows.  It considers $R$ as a quotient of an ambient polynomial ring, $R = S/I$.  It takes a resolution of $I$.  If the resolution has length equal to dim $R$ - dim $S$, then it is Cohen-Macaulay.  If the resolution has a different length, and $I$ is homogeneous, then it is not Cohen-Macaulay.  Finally, if the resolution has a different length and I is not homogeneous, the function looks at the $Ext$ groups which compute the depth.
    Caveat
        Warning, this function assumes that Spec $R$ is connected.  In particular, if you pass it a non-equidimensional Cohen-Macaulay ring (for example, if Spec $R$ has two connected components of different dimensions), this function will return false.
///

doc ///
    Key
        IsLocal
    Headline
        an option used to specify whether to only work locally
    Description
        Text
            If true, then the function will only try to work at the origin (the ideal generated by the variables).
///

doc ///
    Key
        isFRational
        (isFRational, Ring)
        [isFRational, IsLocal]
        [isFRational, AssumeCM]
        [isFRational, AssumeDomain]
        [isFRational, FrobeniusRootStrategy]
    Headline
        whether a ring is F-rational
    Usage
        isFRational(R)
    Inputs
        R:Ring
        IsLocal => Boolean
            check F-rationality only at the origin and call the isCM command from the depth package
        AssumeCM => Boolean
            assume whether the ring is Cohen-Macaulay
        AssumeDomain => Boolean
            assume whether the ring is an integral domain
        FrobeniusRootStrategy=>Symbol
            choose the strategy for internal frobeniusRoot calls
    Outputs
        :Boolean
    Description
        Text
            Determines if a ring is F-rational.  If you pass it {\tt IsLocal=>true}, it will only check if the ring is F-rational at the origin (this can be slower).  If you pass it {\tt AssumeCM=>true}, it will not verify that the ring is Cohen-Macaulay.
        Example
            T = ZZ/5[x,y];
            S = ZZ/5[a,b,c,d];
            g = map(T, S, {x^3, x^2*y, x*y^2, y^3});
            R = S/(ker g);
            isFRational(R)
        Example
            R = ZZ/7[x,y,z]/ideal(x^3+y^3+z^3);
            isFRational(R)
        Text
            We conclude with a more interesting example of a ring that is F-rational but not F-regular.  This example first appeared in A. K. Singh's work on deformation of F-regularity.
        Example
             S = ZZ/3[a,b,c,d,t];
             m = 4;
             n = 3;
             M = matrix{ {a^2 + t^m, b, d}, {c, a^2, b^n-d} };
             I = minors(2, M);
             R = S/I;
             isFRational(R)
        Text
            The option {\tt AssumeDomain} is used when computing a test element.  The option {\tt FrobeniusRootStrategy} is passed to internal @TO frobeniusRoot@ calls.
    Caveat
        Warning, this function assumes that Spec R is connected.  Like {\tt isCohenMacaulay}, if you pass it a non-equidimensional F-rational ring (for example, if Spec R has two connected components of different dimensions), this function will return false.
///
