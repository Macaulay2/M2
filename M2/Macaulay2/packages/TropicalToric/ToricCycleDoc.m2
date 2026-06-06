doc ///
  Key
    isTransverse
    (isTransverse, ToricDivisor, List)
  Headline
    checks transversality of toric divisors and cones
  Usage
    isTransverse(D,C)
  Inputs
    D: ToricDivisor
    C: List
  Outputs
    : Boolean
  Description
    Text
        This function tests transversality of toric divisors and cones. Toric
        divisors are represented using the ToricDivisor class and cones are represented
        using lists.
    Example
        rayList={{1,0},{0,1},{-1,-1},{0,-1}}
        coneList={{0,1},{1,2},{2,3},{3,0}}
        X = normalToricVariety(rayList,coneList)
        D = X_3
        E = {0,3}
        F = {1,2}
        isTransverse(D,E)
        isTransverse(D,F)
///

doc ///
  Key
    makeTransverse
    (makeTransverse, ToricDivisor, List)
  Headline
    Find a divisor linearly equivalent to a toric divisor D that is transverse to a given cone C
  Usage
    makeTransverse(D,C)
  Inputs
    D: ToricDivisor
    C: List
  Outputs
    D: ToricDivisor
  Description
    Text
        This function calculates a linearly equivalent divisor of a given divisor D
        that is transverse to a given cone C.
    Example
        rayList={{1,0},{1,1},{0,1},{-1,-1}};
        coneList={{0,1},{1,2},{2,3},{3,0}};
        X = normalToricVariety fan (transpose matrix rayList,coneList);
        D = X_0+X_1;
        C = {1,2};
        isTransverse(D,C)
        makeTransverse(D,C)
///

doc ///
  Key
    ToricCycle
    (expression,ToricCycle)
    (net,ToricCycle)
  Headline
    the class of a toric cycle on a NormalToricVariety
  Description
    Text
        A toric cycle on X is a finite formal sum of closed torus orbits corresponding to cones in the fan of X.
    Text
        Examples can be found on the toricCycle constructor page.
  SeeAlso
    toricCycle
///

doc ///
  Key
    toricCycle
    (toricCycle,List,NormalToricVariety)
    (toricCycle,List,List,NormalToricVariety)
  Headline
    Creates a ToricCycle
  Usage
    toricCycle(L,X)
    toricCycle(D)
  Inputs
    L: List
      a list of options (sigma => d) where d is the coefficient of the cycle associated to the cone sigma
    X: NormalToricVariety
      the variety the cycle lives on
  Outputs
    : ToricCycle
  Description
    Text
        Toric cycles can be created with the constructor, and as they form an abelian group under addition,
        arithmetic can be done with them.
    Example
        rayList={{1,0},{0,1},{-1,-1},{0,-1}}
        coneList={{0,1},{1,2},{2,3},{3,0}}
        X = normalToricVariety(rayList,coneList)
        cyc = toricCycle({{2,3} =>1,{3,0} => 4},X)
        altcyc = (-2)*cyc
        cyc + altcyc
        cyc - altcyc
        -cyc
  SeeAlso
    (symbol +, ToricCycle, ToricCycle)
///

doc ///
  Key
    (toricCycle,ToricDivisor)
    (toricCycle,ToricCycle)
  Headline
    Creates a ToricCycle from a ToricDivisor
  Usage
    toricCycle(D)
  Inputs
    D: ToricDivisor
  Outputs
    : ToricCycle
  Description
    Text
        This method converts a ToricDivisor into a ToricCycle.
    Example
        X = toricProjectiveSpace 3;
        toricCycle(X_0)
        toricCycle(X_1+X_2)
  SeeAlso
    (symbol +, ToricCycle, ToricCycle)
///

doc ///
  Key
    (support,ToricCycle)
  Headline
    Get the list of cones with non-zero coefficients in the cycle
  Usage
    support C
  Inputs
    C: ToricCycle
  Outputs
    : List
      a list of integer vectors describing cones in the
      fan of variety(C)
  Description
    Text
        This function returns the list of cones, corresponding
        to torus-invariant cycles, appearing in the support of
        a ToricCycle.
    Example
        X = toricProjectiveSpace 4
        Z1 = toricCycle({{0,1} => 3,{0,2} => 7,{1,2} => 82},X)
        Z2 = toricCycle({{0,1} => 4,{0,2} => 5},X)
        support Z1
        support Z2
///

doc ///
  Key
    (variety,ToricCycle)
  Headline
    Get the ambient variety of the cycle
  Usage
    variety C
  Inputs
    C: ToricCycle
  Outputs
    : Variety
  Description
    Text
        This function returns the underlying toric variety of
        the toric cycle.
    Example
        X = toricProjectiveSpace 2
        Z = toricCycle({{0,1} => 3,{0,2} => 7,{1,2} => 82},X)
        variety Z
///

doc ///
  Key
    (symbol *, ToricDivisor, List)
  Headline
    restriction of a Cartier toric divisor to the orbit closure of a cone
  Usage
    D*C
  Inputs
    D: ToricDivisor
    C: List
  Outputs
    : ToricCycle
      Returns the toric cycle given by restricting a Cartier toric divisor to the
      orbit closure of the given cone
  Description
    Example
        rayList={{1,0},{0,1},{-1,-1},{0,-1}}
        coneList={{0,1},{1,2},{2,3},{3,0}}
        X = normalToricVariety(rayList,coneList)
        D = X_3
    Text
        The only cone containing rays 2 and 3 is the cone {2,3}. There is no cone
        containing rows 1 and 3.
    Example
        D*{2}
        D*{1}
    Text
        This can also compute more complicated sums.
    Example
        D = X_0 + 2*X_1 + 3*X_2 + 4*X_3
        C = (orbits X)#1#0
        D*C
///

doc ///
  Key
    (symbol == , ToricCycle, ToricCycle)
  Headline
    equality of toric cycles
  Usage
    C == D
  Inputs
    C: ToricCycle
    D: ToricCycle
  Description
    Text
        This function checks whether the varieties of both cycles are the same,
        and whether the coefficients in each orbit of the toric variety are the
        same in both toric cycles.
    Example
        rayList={{1,0},{0,1},{-1,-1},{0,-1}}
        coneList={{0,1},{1,2},{2,3},{3,0}}
        X = normalToricVariety(rayList,coneList)
        D = X_3
        D*{2} == toricCycle({{2,3} => 1},X)
        D*{1} == toricCycle({},X)
    Text
        The elements of the list in the constructor of toric cycle must be in order. For example,
        the following example has {3,0} instead of {0,3}.
    Example
        D*{0} == toricCycle({{0,3} => 1},X)
  SeeAlso
    (symbol *, ToricDivisor, List)
///

doc ///
  Key
    (symbol *, ToricDivisor, ToricCycle)
    (symbol *, ToricCycle, ToricDivisor)
  Headline
    intersection product of a ToricDivisor and ToricCycle
  Usage
    D*C
    C*D
  Inputs
    D: ToricDivisor
    C: ToricCycle
  Description
    Text
        Computes the intersection product of a ToricDivisor with a ToricCycle.
    Example
        X = toricProjectiveSpace 4
        D = X_0+2*X_1+3*X_2+4*X_3
        C = X_{2,3}
        D*C
    Text
        Self intersection of the exceptional divisor.
    Example
        X = toricProjectiveSpace 2
        Y = toricBlowup({0,1},X)
        D = Y_3
        C = Y_{3}
        D*C
///

doc ///
  Key
    (symbol *, ToricCycle, ToricCycle)
  Headline
    intersection product of two toric cycles
  Usage
    D*C
  Inputs
    D: ToricCycle
    C: ToricCycle
  Description
    Text
        Computes the intersection product of a ToricCycle with another ToricCycle.
    Example
        X = toricProjectiveSpace 5
        D = X_{0,1}+5*X_{1,3}-X_{2}+2*X_{3}
        C = X_{2,3}
        D*C
    Text
        Self intersection of the exceptional divisor.
    Example
        X = toricProjectiveSpace 2
        Y = toricBlowup({0,1},X)
        D = Y_{3}
        D*D
///

doc ///
  Key
    (symbol +, ToricCycle, ToricCycle)
    (symbol -, ToricCycle, ToricCycle)
    (symbol -, ToricCycle)
    (symbol +, ToricCycle, ToricDivisor)
    (symbol -, ToricCycle, ToricDivisor)
    (symbol +, ToricDivisor, ToricCycle)
    (symbol -, ToricDivisor, ToricCycle)

    (symbol *, QQ, ToricCycle)
    (symbol *, ZZ, ToricCycle)
    (symbol *, QQ, ToricDivisor)

  Headline
    perform arithmetic on toric cycles
  Usage
    C1 + C2
    C1 - C2
    5*C1
    (1/2)*C1
    -C1
  Inputs
    C1:ToricCycle
    C2:ToricCycle
  Description
    Text
        The set of torus-invariant cycles forms an abelian group
        under addition.  The basic operations arising from this structure,
        including addition, subtraction, negation, and scalar
        multiplication by integers, are available.
    Text
        We illustrate a few of the possibilities on one variety.
    Example
        rayList={{1,0},{0,1},{-1,-1},{0,-1}}
        coneList={{0,1},{1,2},{2,3},{3,0}}
        X = normalToricVariety(rayList,coneList)
        cyc = toricCycle({{2,3} => 1,{3,0} => 4},X)
        altcyc = (-2)*cyc
        cyc + altcyc
        cyc - altcyc
        -cyc
        X_{0} + X_{1}
        2*X_{0,1} - X_{0,2}
///

doc ///
  Key
    (symbol _, NormalToricVariety, List)
  Headline
    Create toric cycles
  Usage
    X_L
  Inputs
    X: NormalToricVariety
    L: List
  Description
    Text
        Create a toric cycle directly from the toric variety
    Example
        rayList={{1,0},{0,1},{-1,-1},{0,-1}}
        coneList={{0,1},{1,2},{2,3},{3,0}}
        X = normalToricVariety(rayList,coneList)
        X_{1}
        X_{0,2}
        2 * X_{2} - X_{3}
  SeeAlso
    (symbol +, ToricCycle, ToricCycle)
///

doc ///
  Key
    (symbol _, ToricCycle, List)
    (symbol _, ToricCycle, ZZ)
  Headline
    Coefficients of toric cycle
  Usage
    C_L
  Inputs
    C: ToricCycle
    L: List
  Description
    Text
        Another way to get the coefficients of a cycle C
    Example
        rayList={{1,0},{0,1},{-1,-1},{0,-1}}
        coneList={{0,1},{1,2},{2,3},{3,0}}
        X = normalToricVariety(rayList,coneList)
        C = X_{0,1} + X_{0,2} - 2*X_{0,3} + X_{0}
        C_{0,1}
        C_{0,2}
        C_{0,3}
        C_0
  SeeAlso
    (symbol +, ToricCycle, ToricCycle)
///

doc ///
  Key
    degCycle
    (degCycle,ToricCycle)
  Headline
    Compute the degree of a cycle of maximal codimension
  Usage
    degCycle(C)
  Inputs
    C: ToricCycle
  Outputs
    d: ZZ
  Description
    Text
        This function computes the sum of the coefficients of a cycle of maximal codimension
    Example
        rayList={{1,0},{1,1},{0,1},{-1,-1}};
        coneList={{0,1},{1,2},{2,3},{3,0}};
        X = normalToricVariety(rayList,coneList);
        C = X_{0,1} + X_{1,2} - 3*X_{2,3}
        degCycle(C)
///

doc ///
  Key
    toricDivisorFromCycle
    (toricDivisorFromCycle,ToricCycle)
  Headline
    Convert a codimension one toric cycle into a toric divisor
  Usage
    degCycle(C)
  Inputs
    C: ToricCycle
  Outputs
    d: ZZ
  Description
    Example
        X = toricProjectiveSpace 2;
        D = X_{0} + X_{1};
        toricDivisorFromCycle D
///
