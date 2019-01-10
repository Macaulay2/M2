--***********************************************
--***********************************************
--Documentation for BasicFunctions.m2
--***********************************************
--***********************************************

doc ///
    Key
        adicDigit
        (adicDigit, ZZ, ZZ, ZZ)
        (adicDigit, ZZ, ZZ, QQ)
        (adicDigit, ZZ, ZZ, List)
    Headline
        digit of the non-terminating expansion of a number in [0,1] in a given base
    Usage
        d=adicDigit(p,e,x)
        D=adicDigit(p,e,L)
    Inputs
        p:ZZ
            greater than 1; the desired base
        e:ZZ
            positive, which specifies which digit is desired
        x:QQ
            in the interval [0,1]; the number whose digit is to be computed
        L:List
            a list of rational numbers in the interval [0,1]
    Outputs
        d:ZZ
            the $e$-th digit of the base $p$ expansion of $x$
        D:List
            consisting of the $e$-th digits of the base $p$ expansion of the elements of $L$
    Description
        Text
            The command {\tt adicDigit(p,e,0)} returns 0.  If $x\in (0,1]$,
            then {\tt adicDigit(p,e,x)} returns the coefficient of $p^{-e}$ in
            the non-terminating base $p$ expansion of $x$.
        Example
            adicDigit(5,4,1/3)
        Text
            If $L$ is a list
            of rational numbers in the unit interval,  {\tt adicDigit(p,e,L)}
            returns a list where this function is applied to each
            element of $L$.
        Example
            adicDigit(5,4,{1/3,1/7,2/3})
    SeeAlso
        adicExpansion
        adicTruncation
///

doc ///
    Key
        adicExpansion
        (adicExpansion, ZZ, ZZ)
        (adicExpansion, ZZ, ZZ, QQ)
        (adicExpansion, ZZ, ZZ, ZZ)
    Headline
        compute adic expansion
    Usage
        L1 = adicExpansion(p,N)
        L2 = adicExpansion(p,e,x)
    Inputs
        p:ZZ
	    greater than 1; the desired base
        N:ZZ
	    positive, whose base $p$ expansion is to be computed
        e:ZZ
	    positive, which specifies how many digits are to be computed
        x:QQ
	    in the interval [0,1], whose base $p$ expansion is to be computed
    Outputs
        L1:List
            consisting of all digits of the terminating base $p$ expansion of $N$
        L2:List
            consisting of the first $e$ digits of the {\em non}-terminating base
            $p$ expansion of $x$
    Description
        Text
            {\tt adicExpansion(p,0)} returns $\{ 0 \}$.
            If $N$ is nonzero, then {\tt adicExpansion(p,N)} returns a list in
            which the $i$th element is the coefficient of $p^i$ in the base $p$
            expansion of $N$.
        Example
            5==1*2^0+0*2^1+1*2^2
            adicExpansion(2,5)
        Text
            {\tt adicExpansion(p,e,0)} returns a list with $e$ elements, all of which
            are zero. If $x$ is nonzero, then {\tt adicExpansion(p,e,x)} returns a
            list of size $e$ in which the $i$th element is the coefficient of
            $p^{-i-1}$ in the unique nonterminating base $p$ expansion of $x$.
            For example, the non-terminating base $2$ expansion of $1/2$ is
            $1/2 = 0/2 + 1/4 + 1/8 + 1/16 + \cdots$, and so
        Example
            adicExpansion(2,4,1/2)
    SeeAlso
        adicDigit
        adicTruncation
///

doc ///
    Key
        adicTruncation
        (adicTruncation, ZZ, ZZ, ZZ)
        (adicTruncation, ZZ, ZZ, QQ)
        (adicTruncation, ZZ, ZZ, List)
    Headline
        truncation of a non-terminating adic expansion
    Usage
        t=adicTruncation(p, e, r)
        T=adicTruncation(p, e, L)
    Inputs
        p:ZZ
            greater than 1; the desired base
        e:ZZ
            nonnegative, which specifies where to truncate
        r:QQ
            nonnegative; the number whose truncation is to be computed
        L:List
            containing nonnegative rational numbers to compute the truncation of
    Outputs
        t:QQ
            the {\tt e}-th truncation of x (base {\tt p})
        T:List
            containing the {\tt e}-th truncations (base {\tt p})
            of the elements of {\tt L}
    Description
        Text
            This function computes the $e$-th truncation of the $p$-adic expansion of
            a rational number.
        Example
            adicTruncation(5, 2, 1/100)
            adicTruncation(5, 4, 1/100)
            adicTruncation(5, 5, 1/1000)
        Text
            If you pass it zero, it returns zero.
        Example
            adicTruncation(4,2,0)
        Text
            You can also pass it a list of numbers, in which case it returns the
            list of the truncations.
        Example
            adicTruncation(5, 5, {1/100, 1/1000})
    SeeAlso
        adicExpansion
        adicTruncation
///


doc ///
    Key
        floorLog
        (floorLog, ZZ, ZZ)
    Headline
        floor of a logarithm
    Usage
     	floorLog(b,x)
    Inputs
        b:ZZ
            greater than 1; the base of the logarithm
        x:ZZ
	    positive
    Outputs
        :ZZ
    Description
        Text
            {\tt floorLog(b,x)} computes {\tt floor(log_b(x))}, correcting occasional
            errors due to rounding.
        Example
            floor( log_3 3^5 )
            floorLog( 3, 3^5 )
///

doc ///
    Key
        multiplicativeOrder
        (multiplicativeOrder, ZZ, ZZ)
    Headline
        multiplicative order of an integer modulo another
    Usage
        multiplicativeOrder(a,b)
    Inputs
        a:ZZ
            the number whose multiplicative order is to be computed
        b:ZZ
            prime to $a$; the modulus
    Outputs
        :ZZ
            the multiplicative order of $a$ mod $b$.
    Description
        Text
            {\tt multiplicativeOrder(a,b)} computes the multiplicative order
            of $a$ modulo $b$.
        Example
            multiplicativeOrder(2, 11^2)
            multiplicativeOrder(3, 11^2)
            multiplicativeOrder(4, 11^2)
        Text
            If $a$ and $b$ are not relatively prime,  {\tt multiplicativeOrder(a,b)}
            returns an error.
///

doc ///
    Key
        decomposeFraction
        (decomposeFraction, ZZ, Number)
        [decomposeFraction, NoZeroC]
    Headline
        decompose a rational number into a/(p^b(p^c-1))
    Usage
        L = decomposeFraction(p,t)
    Inputs
        p:ZZ
            a prime
        t:QQ
            the fraction to be decomposed
        NoZeroC => Boolean
            forces the returned c to not be zero
    Outputs
        L:List
    Description
        Text
            Given a rational number $t$ and a prime $p$, {\tt decomposeFraction(p,t)}
            returns a list {\tt \{a,b,c\}} of integers, with $b$ and $c$ nonnegative,
            such that $t = a/(p^b(p^c-1))$.
        Example
            decomposeFraction( 3, 4/45 )
            4/45 == 64/( 3^2 * ( 3^4 -1 ) )
        Text
            If our number is of the form $a/p^b$ then there is no valid value of $c$ and the
            function returns $c = 0$. Setting the option {\tt NoZeroC => true}
            forces the third entry of the output list to be nonzero, even if
            that means increasing the first entry.
        Example
            decomposeFraction( 3, 4/27)
            decomposeFraction( 3, 4/27, NoZeroC => true )
            4/27 == 8/( 3^3 * ( 3 - 1 ) )
///


doc ///
    Key
        NoZeroC
    Headline
        an option for decomposeFraction
    Description
        Text
            Valid values are {\tt true} or {\tt false}.
    SeeAlso
        decomposeFraction
///
