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
        compute a digit of the non-terminating expansion of a number in the unit interval in a given base
    Usage
        d = adicDigit(p, e, x)
        D = adicDigit(p, e, L)
    Inputs
        p:ZZ
            greater than 1; the desired base
        e:ZZ
            positive, which specifies which digit is desired
        x:QQ
            in the interval [0,1]; the number whose digit is to be computed
        L:List
            consisting of rational numbers in the interval [0,1] whose digits are to be computed
    Outputs
        d:ZZ
            the {\tt e^{th}} digit of the base {\tt p} expansion of {\tt x}
        D:List
            consisting of the {\tt e^{th}} digits of the base {\tt p} expansions of the elements of {\tt L}
    Description
        Text
            The command {\tt adicDigit(p, e, 0)} returns 0.  
	    If $x$ is a rational number in the interval (0,1], then {\tt adicDigit(p, e, x)} returns the coefficient of $p^{-e}$ in
            the non-terminating base $p$ expansion of $x$.
        Example
            adicDigit(5, 4, 1/3)
        Text
            If $L$ is a list of rational numbers in the unit interval, {\tt adicDigit(p, e, L)} returns a list containing the $e^{th}$ digits (base $p$) of the elements of $L$.
        Example
            adicDigit(5, 4, {1/3, 1/7, 2/3})
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
        L1 = adicExpansion(p, N)
        L2 = adicExpansion(p, e, x)
    Inputs
        p:ZZ
	    greater than 1; the desired base
        N:ZZ
	    positive; the number whose base $p$ expansion is to be computed
        e:ZZ
	    positive, which specifies how many digits are to be computed
        x:QQ
	    in the interval [0,1]; the number whose base {\tt p} expansion is to be computed
    Outputs
        L1:List
            consisting of all digits of the terminating base {\tt p} expansion of {\tt N}
        L2:List
            consisting of the first {\tt e} digits of the {\em non-terminating} base {\tt p} expansion of {\tt x}
    Description
        Text
            {\tt adicExpansion(p, 0)} returns {\tt \{0\}}.
            If $N$ is nonzero, then {\tt adicExpansion(p, N)} returns a list in which the $i^{th}$ element is the coefficient of $p^{i-1}$ in the base $p$ expansion of $N$.
        Example
            38 == 3*5^0 + 2*5^1 + 1*5^2
            adicExpansion(5, 38)
        Text
            {\tt adicExpansion(p, e, 0)} returns a list with $e$ elements, all of which are zero. 
	    If $x$ is nonzero, then {\tt adicExpansion(p, e, x)} returns a list with $e$ elements in which the $i^{th}$ element is the coefficient of $p^{-i}$ in the unique nonterminating base $p$ expansion of $x$.
            For example, the non-terminating base $3$ expansion of $1/5$ is $1/5 = 0/3 + 1/9 + 2/27 + 1/81 + 0/243 + 1/729 + \cdots$, and so {\tt adicExpansion(3, 6, 1/5)} returns the digits $0$, $1$, $2$, $1$, $0$, and $1$.
        Example
            adicExpansion(3, 6, 1/5)
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
        t = adicTruncation(p, e, r)
        T = adicTruncation(p, e, L)
    Inputs
        p:ZZ
            greater than 1; the desired base
        e:ZZ
            nonnegative, which specifies where to truncate
        r:QQ
            nonnegative; the number whose truncation is to be computed
        L:List
            containing nonnegative rational numbers whose truncations are to be computed
    Outputs
        t:QQ
            the {\tt e^{th}} truncation of {\tt r} (base {\tt p})
        T:List
            containing the {\tt e^{th}} truncations (base {\tt p}) of the elements of {\tt L}
    Description
        Text
            This function computes the $e^{th}$ truncation of the unique non-terminating $p$-adic expansion of a positive rational number $r$.
        Example
            adicTruncation(5, 2, 1/100)
            adicTruncation(5, 4, 1/100)
            adicTruncation(5, 5, 1/1000)
        Text
            If $r = 0$, {\tt adicTruncation} returns zero.
        Example
            adicTruncation(4, 2, 0)
        Text
            If a list $L$ of nonnegative rational numbers is passed, {\tt adicTruncation(p, e, L)} returns a list containing the $e^{th}$ truncations (base $p$) of those numbers.
        Example
            adicTruncation(5, 5, {1/100, 1/1000})
    SeeAlso
        adicExpansion
        adicDigit
///


doc ///
    Key
        floorLog
        (floorLog, Number, Number)
    Headline
        floor of a logarithm
    Usage
     	floorLog(b, x)
    Inputs
        b:Number
            greater than 1; the base of the logarithm
        x:Number
	    positive
    Outputs
        :ZZ
	    the floor of {\tt log_b(x)}
    Description
        Text
            {\tt floorLog(b, x)} computes {\tt floor(log_b x)}, correcting occasional
            errors due to rounding.
        Example
            floor(log_3 3^5)
            floorLog(3, 3^5)
///

doc ///
    Key
        multiplicativeOrder
        (multiplicativeOrder, ZZ, ZZ)
    Headline
        multiplicative order of an integer modulo another
    Usage
        multiplicativeOrder(a, b)
    Inputs
        a:ZZ
            whose multiplicative order is to be computed
        b:ZZ
            prime to {\tt a}; the modulus
    Outputs
        :ZZ
            the multiplicative order of {\tt a} mod {\tt b}
    Description
        Text
            {\tt multiplicativeOrder(a,b)} computes the multiplicative order of $a$ modulo $b$.
        Example
            multiplicativeOrder(2, 11^2)
            multiplicativeOrder(3, 11^2)
            multiplicativeOrder(4, 11^2)
        Text
            If $a$ and $b$ are not relatively prime,  {\tt multiplicativeOrder(a, b)}
            returns an error.
///

doc ///
    Key
        decomposeFraction
        (decomposeFraction, ZZ, QQ)
        (decomposeFraction, ZZ, ZZ)
        [decomposeFraction, NoZeroC]
    Headline
        decompose a rational number
    Usage
        (a, b, c) = decomposeFraction(p, t)
    Inputs
        p:ZZ
            a prime
        t:QQ
            the fraction to be decomposed
        NoZeroC => Boolean
            forces the returned {\tt c} to not be zero
    Outputs
        :Sequence
	    containing integers {\tt a}, {\tt b}, and {\tt c}, with {\tt b} and {\tt c} nonnegative, such that {\tt t = a/(p^b(p^c-1))}
    Description
        Text
            Given a rational number $t$ and a prime $p$, {\tt decomposeFraction(p, t)}
            returns a sequence ($a$,$b$,$c$) of integers, with $b$ and $c$ nonnegative,
            such that $t = a/(p^b(p^c-1))$.
        Example
            (a, b, c) = decomposeFraction(3, 4/45)
            4/45 == a/(3^b * (3^c - 1))
        Text
            If the number $t$ is of the form $a/p^b$, then the function returns ($a$,$b$,$0$). 
	    Setting the option {\tt NoZeroC => true} forces the third entry of the output sequence to be nonzero, even if that means increasing the first entry.
        Example
            decomposeFraction(3, 4/27)
            (a, b, c) = decomposeFraction(3, 4/27, NoZeroC => true)
            4/27 == a/(3^b * (3^c - 1))
///


doc ///
    Key
        NoZeroC
    Headline
        an option for decomposeFraction
    Description
        Text
            Valid values are {\tt true} or {\tt false}.
	    If {\tt true}, the third entry in the output of {\tt decomposeFraction} is required to be nonzero.
///
