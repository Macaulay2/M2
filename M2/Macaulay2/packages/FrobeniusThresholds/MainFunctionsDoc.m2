doc ///
    Key
        Bounds
    Headline
        an option for the function fpt specifying lower and upper bounds for the F-pure threshold
    Description
        Text
            An option for the function @TO fpt@ specifying bounds for the $F$-pure threshold.
            {\tt Bounds} accepts lists consisting of two numbers, namely a lower and an upper bound for the $F$-pure threshold being computed.
            This useful feature allows the user to refine bounds obtained in previous computations, as illustrated below.
        Example
            R = ZZ/5[x,y];
            f = x^7*y^5*(x + y)^5*(x^2 + y^3)^4;
            fpt(f, DepthOfSearch => 3, Attempts => 5)
            fpt(f, DepthOfSearch => 3, Attempts => 5, Bounds => oo)
///

doc ///
    Key
        compareFPT
        (compareFPT, Number, RingElement)
        [compareFPT, MaxCartierIndex]
        [compareFPT, FrobeniusRootStrategy]
        [compareFPT, AssumeDomain]
        [compareFPT, QGorensteinIndex]
        [compareFPT, AtOrigin]
        [compareFPT, Verbose]
    Headline
        determine whether a number is less than, greater than, or equal to the F-pure threshold
    Usage
        compareFPT(t, f)
    Inputs
        t:Number
            a rational number to compare to the $F$-pure threshold
        f:RingElement
            in a $\mathbb{Q}$-Gorenstein ring of positive characteristic $p$, whose index is not divisible by $p$
        AssumeDomain => Boolean
            indicates whether the ambient ring of {\tt f}  is an integral domain
        FrobeniusRootStrategy => Symbol
            passed to computations in the @TO TestIdeals@ package
        AtOrigin => Boolean
            tells the function whether to consider only the behavior at the origin
        MaxCartierIndex => ZZ
            sets the maximum $\mathbb{Q}$-Gorenstein index to search for
        QGorensteinIndex => ZZ
            specifies the $\mathbb{Q}$-Gorenstein index of the ring
        Verbose => Boolean
            whether the output is to be verbose
    Outputs
        :ZZ
            namely {\tt -1}, {\tt 1}, or {\tt 0}, according as {\tt t} is less than, greater than, or equal to the $F$-pure threshold of {\tt f}
    Description
        Text
            Let $f$ be an element of a $\mathbb{Q}$-Gorenstein ring of positive characteristic $p$, whose index is not divisible by $p$.
            Given a rational number $t$, the command {\tt compareFPT(t, f)} returns {\tt -1} if $t$ is less than the $F$-pure threshold of $f$, {\tt 1} if $t$ is greater than the $F$-pure threshold, or {\tt 0} if $t$ equals the $F$-pure threshold.
        Example
            R = ZZ/7[x,y];
            f =  x^3 - y^2;
            compareFPT(1/2, f)
            compareFPT(5/6, f)
            compareFPT(6/7, f)
        Text
            As noted, this function can be used in a singular ring of characteristic $p>0$ that is strongly $F$-regular, as long as $p$ does not divide the $\mathbb{Q}$-Gorenstein index.
            For instance, in the following example, $x$ defines a Cartier divisor that is twice one of the rulings of the cone.
        Example
             R = ZZ/5[x,y,z]/(x*y - z^2);
             compareFPT(1/3, x)
             compareFPT(1/2, x)
             compareFPT(13/25, x)
        Text
           Consider a Veronese subring (whose inclusion in the ambient polynomial ring is étale in codimension 1),
            so the $F$-pure threshold of the given polynomial (in this case 19/125) should be independent of which ring we are in.
        Example
            T = ZZ/5[a,b];
            S = ZZ/5[x,y,z,w];
            f = map(T, S, {a^3, a^2*b, a*b^2, b^3});
            R = S/ker f;
            g = x^7 - w^3;
            h = f(sub(g, S))
            compareFPT(19/125, g)
            compareFPT(19/125, h)
            compareFPT(19/124, g)
            compareFPT(19/125 - 1/1000, g)
        Text
            Setting the {\tt AtOrigin} option to {\tt true} (its default value is {\tt false}) will tell the function to consider the $F$-pure threshold at the origin.
        Example
            R = ZZ/7[x,y];
            f = (x + 1)^3 - (y + 3)^2;
            compareFPT(5/6, f)
            compareFPT(5/6, f, AtOrigin => true)
        Text
            If the ambient ring $R$ is not a domain, the option {\tt AssumeDomain} should be set to {\tt false}.
            We assume that the ring is a domain by default, in order to speed up the computation.

            If the Gorenstein index of $R$ is known, the user should set the option {\tt QGorensteinIndex} to this value.
            Otherwise, the function attempts to find the Gorenstein index of $R$, assuming it is between 1 and the value passed to the option {\tt MaxCartierIndex} (default value {\tt 10}).

            The option {\tt FrobeniusRootStrategy} is passed to internal calls of functions from the @TO TestIdeals@ package.
            The two valid values for {\tt FrobeniusRootStrategy} are {\tt Substitution} and {\tt MonomialBasis}.

            Setting the option {\tt Verbose} (default value {\tt false}) to {\tt true} produces verbose output.
    SeeAlso
        fpt
        isFPT
        isFJumpingExponent
///

doc ///
    Key
        ContainmentTest
    Headline
        an option for the function frobeniusNu specifying the type of containment of powers of ideals to test
    Description
        Text
            An option for the function @TO frobeniusNu@ specifying which type of containment test to apply.
            The valid values are {\tt StandardPower}, {\tt FrobeniusRoot}, {\tt GlobalFrobeniusRoot}, {\tt FrobeniusPower}, and {\tt null}.
            When the option @TO AtOrigin@ is set to {\tt true}, the default value for {\tt ContainmentTest}, {\tt null}, is replaced with {\tt FrobeniusRoot} when the second argument passed to @TO frobeniusNu@ is a ring element, and {\tt StandardPower} when that argument is an ideal.
            When @TO AtOrigin@ is set to {\tt false}, the value {\tt GlobalFrobeniusRoot} is used.
///

doc ///
    Key
        FinalAttempt
    Headline
        an option for the function fpt to perform a final check attempting find an F-pure threshold
    Description
        Text
            An option for the function @TO fpt@, specifying whether the convexity of the $F$-signature function and a secant line argument are to be used as an attempt to refine the interval containing the $F$-pure threshold, or to find its exact value.
            Takes on {\tt Boolean} values; default value is {\tt false}.
///

doc ///
    Key
        fpt
        (fpt, RingElement)
        (fpt, List, List)
        [fpt, Attempts]
        [fpt, Bounds]
        [fpt, DepthOfSearch]
        [fpt, FinalAttempt]
        [fpt, GuessStrategy]
        [fpt, AtOrigin]
        [fpt, UseSpecialAlgorithms]
        [fpt, Verbose]
    Headline
        attempt to compute the F-pure threshold of a polynomial at the origin or globally
    Usage
        fpt(f)
        fpt(L, m)
    Inputs
        f:RingElement
            a polynomial with coefficients in a finite field
        L:List
            containing linear forms in two variables
        m:List
            containing positive integers
        Attempts => ZZ
            specifies the number of "guess and check" attempts to make
        Bounds => List
            consisting of two numbers, known to be lower and upper bounds, respectively, of the $F$-pure threshold of {\tt f}
        DepthOfSearch => ZZ
            specifies the power of the characteristic to be used in a search for the $F$-pure threshold
        FinalAttempt => Boolean
            specifies whether to use the $F$-signature function and a secant line argument to attempt to improve the $F$-pure threshold estimate
        GuessStrategy => Function
            specifies a function to be used to rank numbers to be tested
        GuessStrategy => List
            specifies weights to be used to rank numbers to be tested
        AtOrigin => Boolean
            specifies whether to compute the $F$-pure threshold at the origin or the global $F$-pure threshold
        UseSpecialAlgorithms => Boolean
            specifies whether to check if {\tt f} is a diagonal polynomial, binomial, a standard-graded homogeneous polynomial in two variables, or a product of factors in simple normal crossing, and then apply appropriate algorithms or formulas
        Verbose => Boolean
            requests verbose feedback
    Outputs
       :List
           containing lower and upper bounds for the $F$-pure threshold of {\tt f}
       :QQ
           the $F$-pure threshold of {\tt f}
       :InfiniteNumber
           the $F$-pure threshold of {\tt f}, if {\tt f} does not vanish at the origin (or anywhere, if {\tt AtOrigin => false})
    Description
         Text
             Given a polynomial $f$ with coefficients in a finite field, the function {\tt fpt} attempts to find the exact value for the $F$-pure threshold of $f$ at the origin, and returns that value, if possible.
             Otherwise, it returns lower and upper bounds for the $F$-pure threshold.
        Example
             ZZ/5[x,y,z];
             fpt(x^3 + y^3 + z^3 + x*y*z)
             fpt(x^5 + y^6 + z^7 + (x*y*z)^3)
        Text
             When the option {\tt UseSpecialAlgorithms} is set to {\tt true} (the default value), {\tt fpt} first checks whether $f$ is a diagonal polynomial, a binomial, a form in two variables, or a product of factors in simple normal crossing.
             If $f$ is either a diagonal polynomial, a binomial, or a form in two variables, then algorithms of Hernández, or Hernández and Teixeira, are executed to compute the $F$-pure threshold of $f$.  
             If $f$ is a product of factors in simple normal crossing, the $F$-pure threshold is easily computed.
        Example
            fpt(x^17 + y^20 + z^24) -- a diagonal polynomial
            fpt(x^2*y^6*z^10 + x^10*y^5*z^3) -- a binomial
            fpt(x^5*(x + y)^7*(x + y + z)^10) -- SNC
            ZZ/5[x,y];
            fpt(x^2*y^6*(x + y)^9*(x + 3*y)^10) -- a form in two variables
        Text
            The computation of the $F$-pure threshold of a form $f$ in two variables requires factoring $f$ into linear forms, and can sometimes hang when attempting that factorization.
            For this reason, when a factorization is already known, the user can pass to {\tt fpt} a list containing all the pairwise prime linear factors of $f$ and a list containing their respective multiplicities.
        Example
            fpt({x, y, x + y, x + 3*y}, {2, 6, 9, 10}) == oo
        Text
            When no special algorithm is available or {\tt UseSpecialAlgorithms} is set to {\tt false}, {\tt fpt} computes $\nu$ = @TO frobeniusNu@{\tt (e,f)}, where $e$ is the value of the option {\tt DepthOfSearch}, which conservatively defaults to {\tt 1}.
            At this point, we know that the $F$-pure threshold of $f$ lies in the closed interval [$\nu/(p^e-1),(\nu+1)/p^e$], and the subroutine {\tt guessFPT} is called to make some "educated guesses" in an attempt to find the $F$-pure threshold, or at least narrow down the above interval.
            The number of "guesses" is controlled by the option {\tt Attempts}, which conservatively defaults to {\tt 3}.
            If {\tt Attempts} is set to {\tt 0}, {\tt guessFPT} is bypassed.
            If {\tt Attempts} is set to at least {\tt 1}, then a first check is run to verify whether the right-hand endpoint $(\nu+1)/p^e$ of the above interval is the $F$-pure threshold.
        Example
            f = x^2*(x + y)^3*(x + 3*y^2)^5;
            fpt(f, Attempts => 0) -- a bad estimate
            fpt(f, Attempts => 0, DepthOfSearch => 3) -- a better estimate
            fpt(f, Attempts => 1, DepthOfSearch => 3) -- the right-hand endpoint (ν+1)/p^e is the fpt
        Text
            If {\tt Attempts} is set to at least {\tt 2} and the right-hand endpoint $(\nu+1)/p^e$ is not the $F$-pure threshold, a second check is run to verify whether the left-hand endpoint $\nu/(p^e-1)$ is the $F$-pure threshold.
        Example
            f = x^6*y^4 + x^4*y^9 + (x^2 + y^3)^3;
            fpt(f, Attempts => 1, DepthOfSearch => 3)
            fpt(f, Attempts => 2, DepthOfSearch => 3) -- the left-hand endpoint ν/(p^e-1) is the fpt
        Text
            If neither endpoint is the $F$-pure threshold and {\tt Attempts} is set to more than {\tt 2}, then additional checks are performed at numbers in the interval.
            A number in the interval is selected, according to criteria specified by the option @TO GuessStrategy@ (see its documentation for details), and @TO compareFPT@ is used to test that number.
            If that "guess" is correct, its value is returned; otherwise, the information returned by @TO compareFPT@ is used to narrow down the interval, and this process is repeated as many times as specified by {\tt Attempts}.
        Example
            f = x^3*y^11*(x + y)^8*(x^2 + y^3)^8;
            fpt(f, DepthOfSearch => 3, Attempts => 4)
            fpt(f, DepthOfSearch => 3, Attempts => 6)
            fpt(f, DepthOfSearch => 3, Attempts => 8)
        Text
            The option {\tt Bounds} allows the user to specify known lower and upper bounds for the $F$-pure threshold of $f$, in order to speed up computations or to refine previously obtained estimates.
        Example
            f = x^7*y^5*(x + y)^5*(x^2 + y^3)^4;
            fpt(f, DepthOfSearch => 3, Attempts => 5)
            fpt(f, DepthOfSearch => 3, Attempts => 5, Bounds => oo)
        Text
            If {\tt guessFPT} is unsuccessful and {\tt FinalAttempt} is set to {\tt true}, the function {\tt fpt} proceeds to use the convexity of the $F$-signature function and a secant line argument to attempt to narrow down the interval bounding the $F$-pure threshold.
            If successful, the new lower bound may coincide with the upper bound, in which case we can conclude that it is the desired $F$-pure threshold.
            If that is not the case, an $F$-regularity check is done at the new lower bound, to verify if it is the $F$-pure threshold.
        Example
            f = 2*x^10*y^8+x^4*y^7-2*x^3*y^8;
            numeric fpt(f, DepthOfSearch => 3)
            numeric fpt(f, DepthOfSearch => 3, FinalAttempt => true) -- FinalAttempt improves the estimate slightly
        Text
            The computations performed when {\tt FinalAttempt} is set to {\tt true} are often slow, and often fail to improve the estimate, and for this reason, this option should be used sparingly.
            It is often more effective to increase the values of {\tt Attempts} or {\tt DepthOfSearch}, instead.
        Example
            time numeric fpt(f, DepthOfSearch => 3, FinalAttempt => true)
            time fpt(f, DepthOfSearch => 3, Attempts => 7)
            time fpt(f, DepthOfSearch => 4)
        Text
            As seen in several examples above, when the exact answer is not found, a list containing the endpoints of an interval containing the $F$-pure threshold of $f$ is returned.
            Whether that interval is open, closed, or a mixed interval depends on the options passed (it will be open whenever {\tt Attempts} is set to at least {\tt 3}); if the option {\tt Verbose} is set to {\tt true}, the precise interval will be printed.
        Example
            fpt(f, DepthOfSearch => 3, FinalAttempt => true, Verbose => true)
        Text
            Setting the option {\tt AtOrigin} (default value {\tt true}) to {\tt false} can be used to tell the
            function to compute the $F$-pure threshold globally.  In other words, it computes
            the minimum of the $F$-pure threshold at all maximal ideals.
        Example
            R = ZZ/7[x,y];
            f = x*(y - 1)^2 - y*(x - 1)^3;
            fpt(f)
            fpt(f, AtOrigin => false)
        Text
            In this case, most features enabled by {\tt UseSpecialAlgorithms => true} are ignored, except for the check for simple normal crossings;  
            {\tt FinalAttempt => true} is also ignored. 
             
            Consider a simple normal crossings case.
        Example
            f = x*y^2*(x - 1)^3*(y - 1)^4;
            fpt(f)
            fpt(f, AtOrigin => false)
    SeeAlso
        compareFPT
        isFPT
        frobeniusNu
///

doc ///
    Key
        FrobeniusPower
    Headline
        a valid value for the option ContainmentTest
    Description
        Text
            A valid value for the option @TO ContainmentTest@ specifying that Frobenius powers be used when verifying containments of powers of ideals.
    SeeAlso
        ContainmentTest
        frobeniusNu
///

doc ///
    Key
        FrobeniusRoot
    Headline
        a valid value for the option ContainmentTest
    Description
        Text
            A valid value for the option @TO ContainmentTest@ specifying that Frobenius roots be used when verifying containments of powers of ideals.
    SeeAlso
        ContainmentTest
        frobeniusNu
///

doc ///
    Key
        GlobalFrobeniusRoot
    Headline
        a valid value for the option ContainmentTest
    Description
        Text
            A valid value for the option @TO ContainmentTest@ specifying that Frobenius roots be used, and not localized, when verifying containments of powers of ideals.
            This is turned on by default if {\tt AtOrigin} is set to {\tt false} in @TO frobeniusNu@.
    SeeAlso
        ContainmentTest
        frobeniusNu
///

doc ///
    Key
        GuessStrategy
    Headline
        an option for the function fpt to specify the criterion used for selecting numbers to check
    Description
        Text
            In the computation of the $F$-pure threshold of a polynomial $f$, in nontrivial cases and when no special algorithm is used, the function @TO fpt@ uses @TO frobeniusNu@ to find a closed interval [$A$, $B$] that contains the $F$-pure threshold of $f$.
            The subroutine {\tt guessFPT} is then called, to first check whether one of the endpoints $B$ or $A$ is the $F$-pure threshold, and then to select rational numbers in the interval, and check how they are positioned in relation to the $F$-pure threshold, using the function @TO compareFPT@.
            The option {\tt GuessStrategy} controls how this selection of numbers is done.

            We start by describing what happens when {\tt GuessStrategy} is set to {\tt null}, its default value.
            First, a list of several rational numbers in the interval ($A$, $B$) is created and, using the function @TO decomposeFraction@ from the @TO TestIdeals@ package, each number $t$ in that list is written in the form $t = a$ /($p^b$ ($p^c$ - 1)), where $p$ is the characteristic of the ambient ring.
            That list of candidates is then sorted based on

            1. Increasing "computational cost" $w_aa + w_bb + w_cc$, for certain weights $w_a$, $w_b$, and $w_c$,

            and then refined by

            2. Increasing distance from the midpoint of the interval ($A$, $B$).

            Once this sorting is done, the first number in the list is selected, @TO compareFPT@ is called, and the result of that comparison is used to trim the list of candidates and narrow down the interval ($A$, $B$).
            This process is iterated as many times as requested by the user, via the option {\tt Attempts}.
            If the supply of candidates runs low, more are produced.

            The default weights currently used in Criterion 1 are $w_a =$ 0, $w_b =$ 1, and $w_c =$ 1.5.
            With these choices, we believe Criterion 1 is likely to prioritize numbers for which the computation of @TO compareFPT@ is most efficient.
            Criterion 2, on the other hand, aims at partitioning the interval as evenly as possible.

            The option {\tt GuessStrategy} allows the user to choose his or her own weights for Criterion 1.
            In that case, the list is sorted based on Criterion 1 with the user's weights, and then Criterion 1 with default weights and Criterion 2, respectively, are used as tie breakers.
            For instance, if the user suspects that the (minimal) denominator of the $F$-pure threshold is prime to the characteristic $p$, then weights $w_a =$ 0, $w_b =$ 1, and $w_c =$ 0 might be a reasonable choice to try to find that $F$-pure threshold with fewer trials.
        Example                
            R = ZZ/11[x,y];
            f = 6*x^6*y^7 + 8*x^4*y^7 + 8*x^3*y^7 + 6*x^6*y^3 + 5*x^5*y^4 + 4*x^3*y^6 +4*x^3*y^5
            fpt(f, Attempts => 5, DepthOfSearch => 3)
            fpt(f, Attempts => 5, DepthOfSearch => 3, GuessStrategy => {0, 1, 0})        
        Text
            The user may also pass his or her own "cost" functions, that may take as input any of the following: the candidate rational number $t$, the pair ($p$, $t$), where $p$ is the characteristic of the ambient ring, or ($p$, $a$, $b$, $c$), where the integers $a$, $b$, and $c$ are as described above.
            The list of candidates is then sorted first by increasing values of that function, and Criteria 1 and 2, respectively, are used as tie breakers.            
            For instance, if the user suspects the $F$-pure threshold has a small denominator, then passing the function @TO denominator@ may help find the answer in fewer trials.
        Example
            R = ZZ/5[x,y];
            f = x^3*y^11*(x + y)^8*(x^2 + y^3)^8;
            fpt(f, DepthOfSearch => 3, Attempts => 7)
            fpt(f, DepthOfSearch => 3, Attempts => 4, GuessStrategy => denominator)
///

doc ///
    Key
        isFJumpingExponent
        (isFJumpingExponent, Number, RingElement)
        [isFJumpingExponent, AssumeDomain]
        [isFJumpingExponent, FrobeniusRootStrategy]
        [isFJumpingExponent, MaxCartierIndex]
        [isFJumpingExponent, QGorensteinIndex]
        [isFJumpingExponent, AtOrigin]
        [isFJumpingExponent, Verbose]
    Headline
        whether a given number is an F-jumping exponent
    Usage
        isFJumpingExponent(t, f)
    Inputs
        t:Number
            a rational number candidate for $F$-jumping exponent of {\tt f}
        f:RingElement
            in a $\mathbb{Q}$-Gorenstein ring of positive characteristic $p$, whose index is not divisible by $p$
        AssumeDomain => Boolean
            indicates whether the ambient ring of {\tt f}  is an integral domain
        FrobeniusRootStrategy => Symbol
            passed to computations in the @TO TestIdeals@ package
        AtOrigin => Boolean
            tells the function whether to consider only the behavior at the origin
        MaxCartierIndex => ZZ
            sets the maximum $\mathbb{Q}$-Gorenstein index to search for
        QGorensteinIndex => ZZ
            specifies the $\mathbb{Q}$-Gorenstein index of the ring
        Verbose => Boolean
            whether the output is to be verbose
    Outputs
        :Boolean
            reporting whether {\tt t} is an $F$-jumping exponent of {\tt f}
    Description
        Text
            Consider a $\mathbb{Q}$-Gorenstein ring $R$ of characteristic $p>0$, of index not divisible by $p$. Given an element $f$ of $R$ and a rational number $t$, {\tt isFJumpingExponent(t, f)} returns {\tt true} if $t$ is an $F$-jumping exponent of $f$,
            and otherwise it returns {\tt false}.
        Example
            R = ZZ/5[x,y];
            f =  x^4 + y^3 + x^2*y^2;
            isFJumpingExponent(7/12, f)
            isFJumpingExponent(4/5, f)
            isFJumpingExponent(5/6, f)
            isFJumpingExponent(11/12, f)
        Text
            The ring $R$ below is singular, and the jumping numbers of $f$ in the open unit interval are 1/4, 1/2 and 3/4.
        Example
            R = ZZ/11[x,y,z]/(x*y - z^2);
            f = x^2;
            isFJumpingExponent(1/4, f)
            isFJumpingExponent(3/8, f)
            isFJumpingExponent(1/2, f)
            isFJumpingExponent(2/3, f)
            isFJumpingExponent(3/4, f)
        Text
            Setting the option {\tt AtOrigin} to {\tt true} (its default value is {\tt false}) tells the function to consider only $F$-jumping exponents at the origin.
            The following example considers a polynomial that looks locally analytically like two lines at the origin, and four lines at (2,0).
        Example
            R = ZZ/13[x,y];
            f = y*((y + 1) - (x - 1)^2)*(x - 2)*(x + y - 2);
            isFJumpingExponent(3/4, f)
            isFJumpingExponent(3/4, f, AtOrigin => true)
        Text
            If the ambient ring $R$ is not a domain, the option {\tt AssumeDomain} should be set to {\tt false}.
            We assume that the ring is a domain by default, in order to speed up the computation.

            If the Gorenstein index of $R$ is known, the user should set the option {\tt QGorensteinIndex} to this value.
            Otherwise, the function attempts to find the Gorenstein index of $R$, assuming it is between 1 and the value passed to the option {\tt MaxCartierIndex} (default value {\tt 10}).

            The option {\tt FrobeniusRootStrategy} is passed to internal calls of functions from the @TO TestIdeals@ package.
            The two valid values of {\tt FrobeniusRootStrategy} are {\tt Substitution} and {\tt MonomialBasis}.

            Setting the option {\tt Verbose} (default value {\tt false}) to {\tt true} produces verbose output.
    SeeAlso
        compareFPT
        isFPT
        fpt
///

doc ///
    Key
        isFPT
        (isFPT, Number, RingElement)
        [isFPT, AssumeDomain]
        [isFPT, FrobeniusRootStrategy]
        [isFPT, MaxCartierIndex]
        [isFPT, QGorensteinIndex]
        [isFPT, AtOrigin]
        [isFPT, Verbose]
    Headline
        checks whether a given rational number is the F-pure threshold
    Usage
        isFPT(t, f)
    Inputs
        t:Number
            a rational number candidate for the $F$-pure threshold of {\tt f}
        f:RingElement
            in a $\mathbb{Q}$-Gorenstein ring of positive characteristic $p$, whose index is not divisible by $p$
        AssumeDomain => Boolean
            indicates whether the ambient ring of {\tt f} is an integral domain
        FrobeniusRootStrategy => Symbol
            passed to computations in the @TO TestIdeals@ package
        AtOrigin => Boolean
            tells the function whether to consider only the behavior at the origin
        MaxCartierIndex => ZZ
            sets the maximum $\mathbb{Q}$-Gorenstein index to search for
        QGorensteinIndex => ZZ
            specifies the $\mathbb{Q}$-Gorenstein index of the ring
        Verbose => Boolean
            whether the output is to be verbose
    Outputs
        :Boolean
            reporting whether {\tt t} is the $F$-pure threshold of {\tt f}
    Description
        Text
            Consider an element $f$ of a $\mathbb{Q}$-Gorenstein ring of positive characteristic $p$ (of $\mathbb{Q}$-Gorenstein index not divisible by $p$), and a rational number $t$.
            If $t$ is the $F$-pure threshold of $f$, then the command {\tt isFPT(t, f)} outputs {\tt true}, and otherwise, it outputs {\tt false}.
        Example
            R = ZZ/11[x,y];
            f = x^3 + y^2;
            isFPT(9/11, f)
            isFPT(9/12, f)
        Text
            We also include an example in a singular ambient ring.
        Example
            T = ZZ/7[a,b];
            S = ZZ/7[x,y,z,w];
            f = map(T, S, {a^3, a^2*b, a*b^2, b^3});
            R = S/ker f;
            isFPT(1/3, x)
            isFPT(1/3 + 1/10000, x)
            isFPT(1/3 - 1/10000, x)
        Text
            Setting the {\tt AtOrigin} option to {\tt true} (its default value is {\tt false}) tells the function to consider the $F$-pure threshold at the origin.
        Example
            R = ZZ/11[x,y,z]/(x^2 - y*(z - 1));
            isFPT(1/2, z - 1)
            isFPT(1/2, z - 1, AtOrigin => true)
        Text
            If the ambient ring $R$ is not a domain, the option {\tt AssumeDomain} should be set to {\tt false}.
            We assume that the ring is a domain by default, in order to speed up the computation.

            If the Gorenstein index of $R$ is known, the user should set the option {\tt QGorensteinIndex} to this value.
            Otherwise, the function attempts to find the Gorenstein index of $R$, assuming it is between 1 and the value passed to the option {\tt MaxCartierIndex} (default value {\tt 10}).

            The option {\tt FrobeniusRootStrategy} is passed to internal calls of functions from the @TO TestIdeals@ package.
            The two valid values of {\tt FrobeniusRootStrategy} are {\tt Substitution} and {\tt MonomialBasis}.

            Setting the option {\tt Verbose} (default value {\tt false}) to {\tt true} produces verbose output.
    SeeAlso
        compareFPT
        fpt
        isFJumpingExponent
///

doc ///
    Key
        frobeniusNu
        (frobeniusNu, ZZ, Ideal, Ideal)
        (frobeniusNu, ZZ, Ideal)
        (frobeniusNu, ZZ, RingElement, Ideal)
        (frobeniusNu, ZZ, RingElement)
        [frobeniusNu, ContainmentTest]
        [frobeniusNu, AtOrigin]
        [frobeniusNu, ReturnList]
        [frobeniusNu, Search]
        [frobeniusNu, UseSpecialAlgorithms]
        [frobeniusNu, Verbose]
    Headline
        computes the largest power of an ideal not contained in a specified Frobenius power
    Usage
        frobeniusNu(e, I, J)
        frobeniusNu(e, I)
        frobeniusNu(e, f, J)
        frobeniusNu(e, f)
    Inputs
        e:ZZ
            the order of the Frobenius power to consider
        I:Ideal
            in a polynomial ring $R$ over a finite field of characteristic $p$
        f:RingElement
            in the polynomial ring $R$
        J:Ideal
            in the polynomial ring $R$; if omitted, {\tt J} is assumed to be the ideal generated by the variables of $R$
        ContainmentTest => Symbol
            specifies the manner in which to verify the containment of powers of {\tt I} or {\tt f} in {\tt J^{[p^e]}}
        AtOrigin => Boolean
            if {\tt false}, tells the function to take the minimum value over all possible maximal ideals J
        ReturnList => Boolean
            specifies whether to return the list {\tt \{\nu(1),\ldots,\nu(p^e)\}}, as opposed to just {\tt \nu(p^e)}
        Search => Symbol
            specifies the strategy to be used in the search for the largest power of {\tt I} or {\tt f} not contained in {\tt J^{[p^e]}}
        UseSpecialAlgorithms => Boolean
            specifies whether to use special algorithms to compute the $F$-pure threshold of {\tt f}, for certain special types of polynomials
        Verbose => Boolean
            requests verbose feedback, where {\tt \nu(1)}, {\tt \nu(p)}, {\tt \nu(p^2)}, etc., are printed as they are computed
    Outputs
        :ZZ
            the largest integer {\tt \nu\ = \nu(p^e)} such that {\tt I^{\nu}} (or {\tt f^{\nu}}, or {\tt I^{[\nu]}}, depending on the arguments and options passed) is not contained in {\tt J^{[p^e]}}
        :InfiniteNumber
            if {\tt I} or {\tt f} is not contained in the radical of $J$, or when {\tt AtOrigin => false}, if {\tt I} or {\tt f} is a unit ideal
        :List
            containing {\tt \nu(p^i)}, for {\tt i = 0,\ldots,e}
    Description
        Text
            Consider an element $f$ of a polynomial ring $R$ over a finite field of characteristic $p$, and an ideal $J$ of this ring.
            If $f$ is contained in the radical of $J$, then the command {\tt frobeniusNu(e,f,J)} outputs the maximal exponent $n$ such that $f^{ n}$ is not contained in the $p^e$-th Frobenius power of $J$.
            More generally, if $I$ is an ideal contained in the radical of $J$, then {\tt frobeniusNu(e,I,J)} outputs the maximal integer exponent $n$ such that $I^n$ is not contained in the $p^e$-th Frobenius power of $J$.

            These numbers are denoted $\nu_f^J(p^e)$ and $\nu_I^J(p^e)$, respectively, in the literature, and were originally defined in the paper {\it $F$-thresholds and Bernstein-Sato Polynomials}, by Mustaţă, Takagi, and Watanabe.
        Example
            R = ZZ/11[x,y];
            I = ideal(x^2 + y^3, x*y);
            J = ideal(x^2, y^3);
            frobeniusNu(1, I, J)
            f = x*y*(x^2 + y^2);
            frobeniusNu(3, f, J)
        Text
            If $f$ or $I$ is zero, then {\tt frobeniusNu} returns {\tt 0}; if $f$ or $I$ is not contained in the radical of $J$, {\tt frobeniusNu} returns infinity.
        Example
            frobeniusNu(1, 0_R, J)
            frobeniusNu(1, 1_R, J)
        Text
            When the third argument is omitted, the ideal $J$ is assumed to be the homogeneous maximal ideal of $R$.
        Example
            R = ZZ/17[x,y,z];
            f = x^3 + y^4 + z^5;
            frobeniusNu(2, f)
            frobeniusNu(2, f, ideal(x, y, z))
        Text
            It is well known that if $q=p^e$ for some nonnegative integer $e$, then $\nu_I^J(qp) = \nu_I^J(q) p + L$, where the error term $L$ is nonnegative, and can be explicitly bounded from above in terms of $p$ and the number of generators of $I$ and $J$ (e.g., $L$ is at most $p-1$ when $I$ is principal).
            This implies that when searching for {\tt frobeniusNu(e,I,J)}, it is always safe to start at $p$ times {\tt frobeniusNu(e-1,I,J)}, and one need not search too far past this number, and suggests that the most efficient way to compute {\tt frobeniusNu(e,I,J)} is to compute, successively, {\tt frobeniusNu(i,I,J)}, for {\tt i = 0,\ldots,e}.
            This is indeed how the computation is done in most cases.

            If $M$ is the homogeneous maximal ideal of $R$ and $f$ is an element of $R$, the numbers $\nu_f^M(p^e)$ determine and are determined by the $F$-pure threshold of $f$ at the origin.
            Indeed, $\nu_f^M(p^e)$ is $p^e$ times the truncation of the non-terminating base $p$ expansion of fpt($f$) at its $e$^{th} spot.
            This fact is used to speed up the computations for certain polynomials whose $F$-pure thresholds can be quickly computed via special algorithms, namely diagonal polynomials, binomials, forms in two variables, and polynomials whose factors are in simple normal crossing.
            This feature can be disabled by setting the option {\tt UseSpecialAlgorithms} (default value {\tt true}) to {\tt false}.
        Example
            R = ZZ/17[x,y,z];
            f = x^3 + y^4 + z^5; -- a diagonal polynomial
            time frobeniusNu(3, f)
            time frobeniusNu(3, f, UseSpecialAlgorithms => false)
        Text
            The valid values for the option {\tt ContainmentTest} are {\tt FrobeniusPower}, {\tt FrobeniusRoot}, and {\tt StandardPower}.
            The default value of this option depends on what is passed to {\tt frobeniusNu}.
            Indeed, by default, {\tt ContainmentTest} is set to {\tt FrobeniusRoot} if {\tt frobeniusNu} is passed a ring element $f$, and is set to {\tt StandardPower} if {\tt frobeniusNu} is passed an ideal $I$.
            We describe the consequences of setting {\tt ContainmentTest} to each of these values below.

            First, if {\tt ContainmentTest} is set to {\tt StandardPower}, then the ideal containments checked when computing {\tt frobeniusNu(e,I,J)} are verified directly.
            That is, the standard power $I^n$ is first computed, and a check is then run to see if it is contained in the $p^e$-th Frobenius power of $J$.

            Alternately, if {\tt ContainmentTest} is set to {\tt FrobeniusRoot}, then the ideal containments are verified using Frobenius Roots.
            That is, the $p^e$-th Frobenius root of $I^n$ is first computed, and a check is then run to see if it is contained in $J$.
            The output is unaffected, but this option often speeds up computations, specially when a polynomial or principal ideal is passed as the second argument.
        Example
            R = ZZ/5[x,y,z];
            f = x^3 + y^3 + z^3 + x*y*z;
            time frobeniusNu(4, f) -- ContainmentTest is set to FrobeniusRoot, by default
            time frobeniusNu(4, f, ContainmentTest => StandardPower)
        Text
            Finally, when {\tt ContainmentTest} is set to {\tt FrobeniusPower}, then instead of producing the invariant $\nu_I^J(p^e)$ as defined above, {\tt frobeniusNu} instead outputs the maximal integer $n$ such that the $n$^{th} (generalized) Frobenius power of $I$ is not contained in the $p^e$-th Frobenius power of $J$.
            Here, the $n$^{th} Frobenius power of $I$, when $n$ is a nonnegative integer, is as defined in the paper {\it Frobenius Powers} by Hernández, Teixeira, and Witt, which can be computed with the function @TO frobeniusPower@, from the @TO TestIdeals@ package.
            In particular, {\tt frobeniusNu(e,I,J)} and {\tt frobeniusNu(e,I,J,ContainmentTest=>FrobeniusPower)} need not agree.
            However, they will agree when $I$ is a principal ideal.
        Example
            R = ZZ/3[x,y];
            M = ideal(x, y);
            frobeniusNu(3, M^5)
            frobeniusNu(3, M^5, ContainmentTest => FrobeniusPower)
        Text
            The function {\tt frobeniusNu} works by searching through the list of potential integers $n$ and checking containments of $I^n$ in the specified Frobenius power of $J$.
            The way this search is approached is specified by the option {\tt Search}, which can be set to {\tt Binary} (the default value) or {\tt Linear}.
        Example
            R = ZZ/5[x,y,z];
            f = x^2*y^4 + y^2*z^7 + z^2*x^8;
            time frobeniusNu(5, f) -- uses binary search (default)
            time frobeniusNu(5, f, Search => Linear)
            M = ideal(x, y, z);
            time frobeniusNu(2, M, M^2) -- uses binary search (default)
            time frobeniusNu(2, M, M^2, Search => Linear) -- but linear search gets luckier
        Text
            The option {\tt AtOrigin} (default value {\tt true}) can be turned off to tell {\tt frobeniusNu} to effectively do the computation over all possible maximal ideals $J$ and take the minimum.
        Example
            R = ZZ/7[x,y];
            f = (x - 1)^3 - (y - 2)^2;
            frobeniusNu(1, f)
            frobeniusNu(1, f, AtOrigin => false)
        Text
            The option {\tt ReturnList} (default value {\tt false}) can be used to request that the output be not only $\nu_I^J(p^e)$, but a list containing $\nu_I^J(p^i)$, for $i=0,\ldots,e$.
        Example
            R = ZZ/5[x,y,z];
            f = x^2*y^4 + y^2*z^7 + z^2*x^8;
            frobeniusNu(5, f, ReturnList => true)
        Text
            Alternatively, the option {\tt Verbose} (default value {\tt false}) can be used to request that the values $\nu_I^J(p^i)$ ($i=0,\ldots,e$) be printed as they are computed, to monitor the progress of the computation.
        Example
            frobeniusNu(5, f, Verbose => true)
    SeeAlso
            fpt
///

doc ///
    Key
        ReturnList
    Headline
        an option for the function frobeniusNu to return a list of successive nu values
    Description
        Text
            An option for the function @TO frobeniusNu@ specifying whether to return all nu values up to the given order.
            Takes on {\tt Boolean} values; default value is {\tt false}.
///

doc ///
    Key
        Search
    Headline
        an option for the function frobeniusNu to specify the search method for testing containments of powers of ideals
    Description
        Text
            An option for the function @TO frobeniusNu@ that specifies the search algorithm to be used.
            Valid values are {\tt Binary} and {\tt Linear}; default value is {\tt Binary}.
///

doc ///
    Key
        StandardPower
    Headline
        a valid value for the option ContainmentTest
    Description
        Text
            A valid value for the option @TO ContainmentTest@, specifying that when verifying containments of powers of ideals, to check whether the standard power of an ideal is contained in the Frobenius power of another ideal.
    SeeAlso
        ContainmentTest
        frobeniusNu
///

doc ///
    Key
        UseSpecialAlgorithms
    Headline
        an option for the functions fpt and frobeniusNu to use special algorithms to speed up computations
    Description
        Text
            An option for the functions @TO fpt@ and @TO frobeniusNu@.
            Takes on {\tt Boolean} values; default value is {\tt true}, in both functions.

            If this option is set to {\tt true} in @TO fpt@, that function checks whether the input is a diagonal polynomial, a binomial, a product of factors in simple normal crossing, or a form in two variables, in which case specialized algorithms or formulas are used.

            If set to {\tt true} in @TO frobeniusNu@, that function checks whether the input is one of the above special types of polynomials, in which case it computes the $F$-pure threshold of the polynomial, and uses the $F$-pure threshold to compute the nu invariant.
///
