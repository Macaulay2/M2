doc ///
    Key
        canonicalIdeal
        (canonicalIdeal, Ring)
    Headline
        given a ring, produces an ideal isomorphic to the canonical module
    Usage
        canonicalIdeal(R)
    Inputs
        R:Ring  
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
///

doc ///
    Key
        [canonicalIdeal, MTries]
    Headline
        how many times to try to embed a canonical module as an ideal
    Usage
        canonicalIdeal(..., MTries=>n)
    Inputs
        n:ZZ
    Outputs
        :Ideal
    Description
        Text
            The option MTries is passed to embedAsIdeal.
///      

doc ///
    Key
        MTries
    Headline
        an option to pass through to embedAsIdeal
    Description
        Text
            Used when embedding a module (such as the canonical module) as an ideal.  This is passed through to {\tt embedAsIdeal} from the Divisor package.
///            

doc ///
    Key
        frobeniusTraceOnCanonicalModule
    Headline
        finds the u, which in a polynomail ring, determines the Frobenius trace on canonical module of a quotient of that ring
    Usage
        frobeniusTraceOnCanonicalModule(canIdeal, defIdeal)
    Inputs
        canIdeal:Ideal
        defIdeal:Ideal
    Outputs
        :RingElement
    Description
        Text
            Given $R = S/I$, where $S$ is a polynomial ring, there is a map from the canonical module of $R$ back to itself, dual to the Frobenius on $R$.  This map comes from a $p$ inverse linear map on $S$, restricted appropriately.  But every $p$ inverse linear map on $S$ is a premultiple of the Grothendieck dual by some element $u$.  This function finds the $u$, or at least finds some elements, some linear combination of them is the actual $u$.  We note that Macaulay2 doesn't always properly identify an ideal as principal (even though it is).  Thus we need a list of u's.
        Text
            Specifically, you need to pass this two ideals.  An ideal that restricts to the canonical ideal, and an ideal that defines the variety.  Normally the canonical ideal should be chosen so that it contains the defining ideal (if you do not do this, there may be unexpected behavior).
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
        (testModule, QQ, RingElement)
        (testModule, ZZ, RingElement)
        (testModule, QQ, RingElement, Ideal, List)
        (testModule, List, List)
        (testModule, List, List, Ideal, List)
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
        tt:QQ
        tt:ZZ
        u1:List
    Outputs
        :Sequence
    Description
        Text
            Computes the parameter test module (as a submodule of the canonical module).  The function returns three values, the parameter test submodule, the canonical module of which it is a subset, and the element $u$ (or $u$s) used to compute this ideal via the method @TO frobeniusTraceOnCanonicalModule@.  
        Example
            R = ZZ/7[x,y,z]/ideal(x^3+y^3+z^3);
            testModule(R)
        Text
            Note this is a Gorenstein ring and so the ambient canonical module is the unit ideal.
        Example
            S = ZZ/3[x,y,u,v];
            T = ZZ/3[a,b];
            f = map(T, S, {a^3, a^2*b, a*b^2, b^3});
            R = S/(ker f);
            testModule(R)
        Text
            Note that the output in this case has the parameter test module equal to the canonical module, as it should be.  Let's a non-Gorenstein example which is not F-rational.
        Example
            R = ZZ/5[x,y,z]/ideal(y*z, x*z, x*y);
            paraTestMod = testModule(R)
            (paraTestMod#0) : (paraTestMod#1)
        Text
            This function can be used to compute parameter test ideals in Cohen-Macaulay rings
        Example
            R=ZZ/2[X_1..X_5]; 
            E=matrix {{X_1,X_2,X_2,X_5},{X_4,X_4,X_3,X_1}};
            I=minors(2,E);
            tau=testModule(R/I);
            substitute( (tau#0):(tau#1),R)
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
            Finally, sometimes you would like to specify the ambient canonical module (and choice of u) across multiple calls of testModule.  Those are what the $canIdeal$ or $u1$ can be used to specify.
///

doc ///
    Key
        parameterTestIdeal
        (parameterTestIdeal, Ring)
    Headline
        computes the parameter test ideal of a Cohen-Macaulay ring
    Usage
        parameterTestIdeal(R)
    Inputs
        R:Ring
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
    Outputs
        :Boolean
    Description
        Text
            Determines if a ring is Cohen-Macaulay.  If you pass the {\tt IsLocal parameter}, this will simply call the @TO isCM@ function in the {\tt Depth} package, which checks whether the ring is Cohen-Macaulay at the origin.  
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
        Text
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
        isFrational
        (isFrational, Ring)
        [isFrational, IsLocal]
        [isFrational, AssumeCM]
    Headline
        whether a ring is F-rational
    Usage
        isFrational(R)
    Inputs
        R:Ring
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
            isFrational(R)
        Example
            R = ZZ/7[x,y,z]/ideal(x^3+y^3+z^3);
            isFrational(R)
        Text
            We conclude with a more interesting example of a ring that is F-rational but not F-regular.  This came up in A. K. Singh's work on deformation of F-regularity.
        Example
             S = ZZ/3[a,b,c,d,t];
             m = 4; 
             n = 3;
             M = matrix{ {a^2 + t^m, b, d}, {c, a^2, b^n-d} };
             I = minors(2, M);
             R = S/I;
             isFrational(R)
        Text
            Warning, this function assumes that Spec R is connected.  Like {\tt isCohenMacaulay}, if you pass it a non-equidimensional F-rational ring (for example, if Spec R has two connected components of different dimensions), this function will return false.  
///
