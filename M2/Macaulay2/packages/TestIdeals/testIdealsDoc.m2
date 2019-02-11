doc ///
    Key
        QGorensteinGenerator
        (QGorensteinGenerator, ZZ, Ring)
        (QGorensteinGenerator, Ring)
    Headline
        finds an element representing the Frobenius trace map of a Q-Gorenstein ring
    Usage
        QGorensteinGenerator(e, R)
        QGorensteinGenerator(R)
    Inputs
        e: ZZ
            the degree in which to search
        R: Ring
            the Q-Gorenstein ring
    Outputs
        :RingElement
    Description
        Text
            Suppose that $R$ is a ring such that $(p^e-1)K_R$ is linearly equivalent to zero (for example, if $R$ is $Q$-Gorenstein with index not divisible by $p$).
            Then if we write $R = S/I$ where $S$ is a polynomial ring, we have that $I^{[p^e]} : I = u S + I^{[p^e]}$ for some $u \in S$.
            By Fedder's criterion, this element $u$ represents the generator of the $R^{1/p^e}$-module $Hom(R^{1/p^e}, R)$.
            For example if $I = (f)$ is principal, then $u = f^{p^e-1}$ works.
        Text
            This function produces the element $f$ described above.  If the user does not specify an integer e, it assumes e = 1.
        Example
            S = ZZ/3[x,y,z];
            f = x^2*y - z^2;
            I = ideal(f);
            R = S/I;
            u = QGorensteinGenerator(1, R)
            u%I^3 == f^2%I^3
        Text
            If Macaulay2 does not recognize that $I^{[p^e]} : I / I^{[p^e]}$ is principal, an error is thrown, which will also happen if R is not Q-Gorenstein of the appropriate index.  Note in the nongraded case, Macaulay2 is not guaranteed to find minimal generators of principally generated modules.
///

doc ///
    Key
        testElement
        (testElement, Ring)
        [testElement, AssumeDomain]
    Headline
        finds a test element of a ring
    Usage
        testElement(R)
    Inputs
        R: Ring
            the ring to find a test element in
        AssumeDomain => Boolean
            assume the ring is a domain
    Outputs
        :RingElement
    Description
        Text
            Given $R = S/I$ where $S$ is a polynomial ring, this finds an element of $S$ that restricts to a test element of $R$.  This does this by finding a minor of the Jacobian whose determinant is not in any minimal prime of the defining ideal of $R$.  This funtion considers random minors until one is found, instead of computing all minors.  Thus, repeated calls will not always produce the same answer.
        Example
            R = ZZ/5[x,y,z]/(x^3+y^3+z^3);
            testElement(R)
            testElement(R)
            testElement(R)
        Text
            If {\tt AssumeDomain => true} then testElement does not to compute the minimal primes of the ring.  This can result in a substantial speedup in some cases.  The default value is {\tt false}.
///

doc ///
    Key
        AssumeDomain
    Headline
        an option to assume a ring is a domain
///

doc ///
    Key
        MaxCartierIndex
    Headline
        an option to specify the maximum number to consider when computing the Cartier index of a divisor
    Description
        Text
                Some functions need to find the smallest value $N$ such that $N$ times a divisor (usually the canonical divisor) is Cartier. By specifying this value, the user controls what the maximal possible Cartier index to consider is.
///


doc ///
    Key
        QGorensteinIndex
    Headline
        an option to specify the index of the canonical divisor, if known
    Description
        Text
             When working in a $Q$-Gorenstein ring frequently we must find an $N$ such that $N * K_R$ is Cartier. This option lets the user skip this search if this integer $N$ is already known by setting {\tt QGorensteinIndex => N}.
///

doc ///
    Key
        DepthOfSearch
    Headline
        an option to specify how hard to search for something
    Description
        Text
             This option is used to tell certain functions how hard to look for an answer.  Increasing it too much can make functions take a lot of time and resources.  Making it too small may mean that an incorrect or incomplete answer is given.  See the documentation for each function.
///

doc ///
    Key
        testIdeal
        (testIdeal, Ring)
        (testIdeal, Number, RingElement)
        (testIdeal, Number, RingElement, Ring)
        (testIdeal, List, List)
        (testIdeal, List, List, Ring)
        [testIdeal, AssumeDomain]
        [testIdeal, FrobeniusRootStrategy]
        [testIdeal, MaxCartierIndex]
        [testIdeal, QGorensteinIndex]
    Headline
        computes the test ideal of f^t in a Q-Gorenstein ring
    Usage
        testIdeal(t, f)
        testIdeal(t, f, R)
        testIdeal(Lexp, Lelts)
        testIdeal(Lexp, Lelts, R)
    Inputs
        R: Ring
        t: QQ
            a formal exponent for f
        f: RingElement
            the element to compute the test ideal of
        Lexp: List
            a list of formal exponents
        Lelts: List
            a list of elements to compute the test ideal of
        AssumeDomain => Boolean
            assume the ring is an integral domain
        FrobeniusRootStrategy => Symbol
            choose the strategy for internal frobeniusRoot calls
        MaxCartierIndex => ZZ
            sets the maximum Gorenstein index to search for when working with a Q-Gorenstein ambient ring
        QGorensteinIndex => ZZ
            specifies the Q-Gorenstein index of the ring
    Outputs
        :Ideal
    Description
        Text
            Given a normal Q-Gorenstein ring $R$, passing the ring simply computes the test ideal $\tau(R)$.
        Example
            R = ZZ/5[x,y,z]/ideal(x^3+y^3+z^3);
            testIdeal(R)
        Example
            S = ZZ/5[x,y,z,w];
            T = ZZ/5[a,b];
            f = map(T, S, {a^3, a^2*b, a*b^2, b^3});
            R = S/(ker f);
            testIdeal(R)
        Text
            Given a normal Q-Gorenstein ring $R$, a rational number $t \geq 0$ and a ring element $f \in R$, we can also compute the test ideal $\tau(R, f^t)$.
        Example
            R = ZZ/5[x,y,z];
            f = y^2 - x^3;
            testIdeal(1/2, f)
            testIdeal(5/6, f)
            testIdeal(4/5, f)
            testIdeal(1, f)
        Example
            R = ZZ/7[x,y,z];
            f = y^2 - x^3;
            testIdeal(1/2, f)
            testIdeal(5/6, f)
            testIdeal(4/5, f)
            testIdeal(1, f)
        Text
            It even works if the ambient ring is not a polynomial ring.
        Example
            R = ZZ/11[x,y,z]/ideal(x^2-y*z);
            f = y;
            testIdeal(1/2, f)
            testIdeal(1/3, f)
        Text
            Alternately, you can instead pass a list of rational numbers $\{t1, t2, ...\}$ and a list of ring elements $\{f1, f2, ...\}$.  In this case it will compute the test ideal $\tau(R, f1^{t1} f2^{t2} ...)$.
        Example
            R = ZZ/7[x,y];
            L = {x,y,(x+y)};
            f = x*y*(x+y);
            testIdeal({1/2,1/2,1/2}, L)
            testIdeal(1/2, f)
            testIdeal({2/3,2/3,2/3}, L)
            testIdeal(2/3, f)
            time testIdeal({3/4,2/3,3/5}, L)
            time testIdeal(1/60, x^45*y^40*(x+y)^36)
        Text
            As above, frequently passing a list will be faster (as opposed to finding a common denominator and passing a single element) since the {\tt testIdeal} can do things in a more intelligent way for such a list.
        Text
            The option {\tt AssumeDomain => true} is used when finding a test element.  The default value is {\tt false}.  The option {\tt FrobeniusRootStrategy} is passed to internal @TO frobeniusRoot@ calls.
        Text
            When working in a Q-Gorenstein ring this function finds an $N$ such that $N * K_R$ is Cartier.  This option controls the maximum value of $N$ to consider.  The default value is $100$.  If you pass this function a ring such that the smallest such $N$ is less that MaxCartierIndex, then the function will throw an error.  This value is ignored if the user specifies the option {\tt QGorensteinIndex}.  In particular, specifying the {\tt QGorensteinIndex} will let the user skip the search for the value $N$.
    SeeAlso
        testModule
        isFRegular
///



doc ///
    Key
        isFRegular
        (isFRegular, Ring)
        (isFRegular, Number, RingElement)
        (isFRegular, List, List)
        [isFRegular, AssumeDomain]
        [isFRegular, FrobeniusRootStrategy]
        [isFRegular, MaxCartierIndex]
        [isFRegular, QGorensteinIndex]
        [isFRegular, IsLocal]
        [isFRegular, DepthOfSearch]
    Headline
        whether a ring or pair is strongly F-regular
    Usage
        isFRegular(R)
        isFRegular(t, f)
        isFRegular(Lexp, Lelts)
    Inputs
        R: Ring
        t: QQ
            a formal exponent for f
        f: RingElement
            the element for the pair, to compute F-regularity
        Lexp: List
            a list of formal exponents
        Lelts: List
            a list of elements for the tuple, to compute F-regularity
        AssumeDomain => Boolean
            assume the ring is an integral domain
        FrobeniusRootStrategy => Symbol
            choose the strategy for internal frobeniusRoot calls
        MaxCartierIndex => ZZ
            sets the maximum Gorenstein index to search for when working with a Q-Gorenstein ambient ring
        QGorensteinIndex => ZZ
            specifies the Q-Gorenstein index of the ring
        IsLocal => Boolean
            specifies whether to check F-regularity just at the origin
        DepthOfSearch => ZZ
            specifies how hard to try to prove a non-Q-Gorenstein ring is F-regular
    Outputs
        :Boolean
    Description
        Text
            Given a normal Q-Gorenstein ring $R$, this computes whether the ring is strongly F-regular.  It can also prove that a non-Q-Gorenstein ring is F-regular (but cannot show it is not).  See below for how to access this functionality.
        Example
            R = ZZ/5[x,y,z]/ideal(x^2+y*z);
            isFRegular(R)
        Example
            R = ZZ/7[x,y,z]/ideal(x^3+y^3+z^3);
            isFRegular(R)
        Text
            It can also do the same computation for a pair.
        Example
            R = ZZ/5[x,y];
            f = y^2-x^3;
            isFRegular(1/2, f)
            isFRegular(5/6, f)
            isFRegular(4/5, f)
            isFRegular(4/5-1/100000, f)
        Text
            When checking whether a ring or pair is strongly F-regular, the option IsLocal determines if this is checked at the origin or everywhere (default is {\tt false}, which corresponds to everywhere).  If you set {\tt IsLocal=>true}, it will only check this at the origin.
        Example
            R = ZZ/7[x,y,z]/ideal((x-1)^3+(y+1)^3+z^3);
            isFRegular(R)
            isFRegular(R, IsLocal=>true)
            S = ZZ/13[x,y,z]/ideal(x^3+y^3+z^3);
            isFRegular(S)
            isFRegular(S, IsLocal=>true)
        Text
            Here is an example of {\tt IsLocal} behavior with a pair.
        Example
            R = ZZ/13[x,y];
            f = (y-2)^2 - (x-3)^3;
            isFRegular(5/6, f)
            isFRegular(5/6, f, IsLocal=>true)
            g = y^2 - x^3;
            isFRegular(5/6, g)
            isFRegular(5/6, g, IsLocal=>true)
        Text
            The option {\tt AssumeDomain => true} is used when finding a test element.  The default value is {\tt false}.  The option {\tt FrobeniusRootStrategy} is passed to internal @TO frobeniusRoot@ calls.
        Text
            When working in a Q-Gorenstein ring this function finds an $N$ such that $N * K_R$ is Cartier.  This option controls the maximum value of $N$ to consider.  The default value is $100$.  If you pass this function a ring such that the smallest such $N$ is less that MaxCartierIndex, then the function will throw an error.  This value is ignored if the user specifies the option {\tt QGorensteinIndex}.  In particular, specifying the {\tt QGorensteinIndex} will let the user skip the search for the value $N$.
        Text
            You can also show that rings that are {\bf NOT} Q-Gorenstein are F-regular (it cannot show that such a ring is {\bf not} F-regular).  To do this, set the option {\tt QGorensteinIndex=>infinity}.  One may change the option {\tt DepthOfSearch} to increase the depth of search.
        Example
            S = ZZ/7[x,y,z,u,v,w];
            I = minors(2, matrix{{x,y,z},{u,v,w}});
            debugLevel = 1;
            time isFRegular(S/I, QGorensteinIndex=>infinity, DepthOfSearch=>1)
            time isFRegular(S/I, QGorensteinIndex=>infinity, DepthOfSearch=>2)
            debugLevel = 0;
    SeeAlso
        testIdeal
        isFRational
///

doc ///
    Key
        isFPure
        (isFPure, Ring)
        (isFPure, Ideal)
        [isFPure, IsLocal]
        [isFPure, FrobeniusRootStrategy]
    Headline
        whether a ring is F-pure
    Usage
        isFPure(R)
        isFPure(I)
    Inputs
        R: Ring
            the ring to check F-purity of
        I: Ideal
            the defining ideal of the ring to check F-purity of
        IsLocal => Boolean
            whether the F-purity is checked at the origin or everwhere
        FrobeniusRootStrategy => Symbol
            choose the strategy for internal frobeniusRoot calls
    Outputs
        :Boolean
    Description
        Text
            Given a ring $R$, this computes whether the ring is F-pure using Fedder's criterion (by applying @TO frobeniusRoot@ to $I^{[p]} : I$).
        Example
            R = ZZ/5[x,y,z]/ideal(x^2+y*z);
            isFPure(R)
        Example
            R = ZZ/7[x,y,z]/ideal(x^3+y^3+z^3);
            isFPure(R)
        Example
            R = ZZ/5[x,y,z]/ideal(x^3+y^3+z^3);
            isFPure(R)
        Text
            Alternately, one may pass it the defining ideal of a ring.
        Example
            S = ZZ/2[x,y,z];
            isFPure(ideal(y^2-x^3))
            isFPure(ideal(z^2-x*y*z+x*y^2+x^2*y))
        Text
            The option {\tt IsLocal} controls whether F-purity is checked at the origin or everywhere.  If you set {\tt IsLocal=>true} (default is {\tt false}), it will only check this at the origin.
        Example
            R = ZZ/5[x,y,z]/ideal((x-1)^3+(y-2)^3+z^3);
            isFPure(R)
            isFPure(R, IsLocal=>true)
            S = ZZ/13[x,y,z]/ideal(x^3+y^3+z^3);
            isFPure(S)
            isFPure(S, IsLocal=>true)
        Text
            Note there is a difference in the strategy for the local or non-local computations.  In fact, checking it everywhere can sometimes be faster than checking the origin.  If {\tt IsLocal=>false} then the function computes @TO frobeniusRoot@ applied to $I^{[p]} : I$, if {\tt IsLocal=>true} then the function checks wheter or not $I^{[p^e]} : I$ is contained in $m^{[p^e]}$ where $m$ is the maximal ideal generated by the variables.
        Text
            The option {\tt FrobeniusRootStrategy} is passed to internal @TO frobeniusRoot@ calls.
    SeeAlso
        isFRegular
        isFInjective
///
