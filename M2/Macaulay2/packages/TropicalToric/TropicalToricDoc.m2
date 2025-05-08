doc ///
    Key
    	TropicalToric
    Headline
    	Macaulay2 package for toric intersection theory using tropical geometry.
    Description
    	Text
          This package implements the computations of classes in the
          Chow ring of a toric variety using tropical geometry.
          Part of this code is based on a package previously written by
          Diane Maclagan, Sameera Vemulapalli, Corey Harris, Erika Pirnes and Ritvik Ramkumar.
///

doc ///
  Key
    refineMultiplicity
    (refineMultiplicity,TropicalCycle,NormalToricVariety)
    (refineMultiplicity,List,Fan,NormalToricVariety)
  Headline
    Compute the multiplicities of a refinement of a tropical variety
  Usage
    refineMultiplicity(T,X)
  Inputs
    T: TropicalCycle
    X: NormalToricVariety
      which fan refines T
  Outputs
    mult: List
      list of the multiplicities, indexed by cones of X of the same dimension of T
  Description
    Text
        This function calculate the multiplicities of a refinement of a tropical variety T.
        The output is a list of integers indexed by orbits(X,n-k), where k = dim T, n = dim X
        and X is the normal toric variety of the refinement.
    Example
        X = toricProjectiveSpace 3;
        R = QQ[x_1 .. x_3];
        f = x_1*x_2*x_3 + x_1*x_2 + x_1*x_3 + x_2*x_3;
        T = tropicalVariety(ideal f);
        F = gfanFanCommonRefinement(fan X, fan T);
        X' = makeSimplicial (normalToricVariety F);
        multiplicities T
        refineMultiplicity(T,X')
///

doc ///
  Key
    pushforwardMultiplicity
    (pushforwardMultiplicity,NormalToricVariety,NormalToricVariety,List,ZZ)
  Headline
    pushforward of Minkowski weights
  Usage
    pushforwardMultiplicity(X,X',mult,k)
  Inputs
    X: NormalToricVariety
    X': NormalToricVariety
    mult: List
    k: ZZ
  Outputs
    prod: List
      list of intersection products
  Description
    Text
        The list mult is the list of multiplicities of the tropicalization of
        the intersection of a subvariety Y of X with the torus. The tropicalization has the fan
        structure of the fan of X', and k is the dimension of Y. This function calculates the intersection
        products of Y and V(sigma), where sigma is a cone of X of dimension k. This is done by calculating
        the pullback of V(sigma) in X', and substituting V(sigma') with m(sigma'), where sigma' is a cone
        of dimension k in X'. This is the same as taking the class in X' corresponding to the Minkowski weights
        given by Y and computing the Minkowski weights corresponding to the pushforward of this class on X.
    Example
        X = toricProjectiveSpace 3;
        R = QQ[x_1 .. x_3];
        f = x_1*x_2*x_3 + x_1*x_2 + x_1*x_3 + x_2*x_3;
        T = tropicalVariety(ideal f);
        F = gfanFanCommonRefinement(fan X, fan T);
        X' = makeSimplicial (normalToricVariety F);
        mult = refineMultiplicity(T,X');
        pushforwardMultiplicity(X,X',mult,dim T)
///

doc ///
  Key
    poincareDuality
    (poincareDuality,List,NormalToricVariety,ZZ)
    poincareMatrix
    (poincareMatrix,NormalToricVariety,ZZ)
    (symbol PoincareMatrix)
  Headline
    Apply the isomorphism Hom(A^k(X),ZZ) = A_k(X)
  Usage
    poincareDuality(l, X, k)
  Inputs
    l: List
    X: NormalToricVariety
    k: ZZ
  Outputs
    Y: List
  Description
    Text
        This function calculates the list of coefficients of a toric cycle corresponding to a list
        of Minkowski weights.
    Example
        X = toricProjectiveSpace 3;
        k = 2;
        l = toList(#orbits(X,1):3)
        poincareDuality(l, X, k)
        poincareMatrix(X,2)
///

doc ///
  Key
    classFromTropical
    (classFromTropical,NormalToricVariety,Ideal)
  Headline
    compute a toric cycle of X which class is the same as a given subvariety of X
  Usage
    classFromTropical(X,I)
  Inputs
    X: NormalToricVariety
    I: Ideal
  Outputs
    D: ToricCycle
  Description
    Text
        This function calculates a toric cycle which class is the same of a subvariety of a
        normal toric variety X, under the hypothesis that X is simplicial. The subvariety is given
        by the ideal of its intersection with the torus inside X.
    Example
        X = toricProjectiveSpace 3;
        R = QQ[x_1 .. x_3];
        I = ideal(x_1+x_2+x_3+1,x_1+2*x_2-x_3+4);
        classFromTropical(X,I)
///

doc ///
  Key
    classWonderfulCompactification
    (classWonderfulCompactification,NormalToricVariety,Ideal,Ideal)
    (classWonderfulCompactification,NormalToricVariety,Ideal,RingElement)
    (classWonderfulCompactification,Ideal,Ideal)
    (classWonderfulCompactification,Ideal,RingElement)
  Headline
    compute a toric cycle of X which class is the same as a given subvariety of X
  Usage
    classWonderfulCompactification(X,I,J)
  Inputs
    X: NormalToricVariety
    I: Ideal
    J: Ideal
  Outputs
    D: ToricCycle
  Description
    Text
        Given a hyperplane inside the torus, given by a linear ideal I, let X be a normal toric variety which fan
        is supported on the tropicalization of V(I). This function calculates a toric cycle which class is the same
        of a subvariety of V(I). The subvariety Y of V(I) is given by an ideal J such that Y = V(I+J), that is
        Y is the intersection of V(J) and V(I) inside the torus of X. The purpose of this function is
        essentially the same as classFromTropical, but it is optimized to this particular setting. Note that
        the closure of V(I) inside X is a wonderful compactification of V(I).
    Text
        It is possible to explicitly input the toric variety, and this allows to implicitly specify the building set
        of the wonderful compactification.
    Text
        A remarkable example among these is the moduli space of n-marked genus 0 curves M_0,n.
        Below, we use this function on M_0,6 to compute one of the 15 Keel-Vermeire divisors.
    Example
        R = QQ[x_0..x_8];
        I = ideal {-x_0+x_3+x_4, -x_1+x_3+x_5,-x_2+x_3+x_6, -x_0+x_2+x_7, -x_1+x_2+x_8, -x_0+x_1+1};
        X = normalToricVariety fan tropicalVariety I;
        f = x_0*x_1-x_2*x_3;
        D = classWonderfulCompactification(X,I,f)
///

doc ///
  Key
    torusIntersection
    (torusIntersection,NormalToricVariety,RingElement,Ring)
    (torusIntersection,NormalToricVariety,RingElement)
    (torusIntersection,NormalToricVariety,Ideal,Ring)
    (torusIntersection,NormalToricVariety,Ideal)
  Headline
    compute the ideal of the intersection of a subvariety of a toric variety with the torus
  Usage
    torusIntersection(X,I)
  Inputs
    X: NormalToricVariety
    I: Ideal
        inside the Cox ring of X
  Outputs
    J: Ideal
  Description
    Text
        This function calculates the ideal of the intersection of subavariety of a toric variety X
        with the torus of X. The output is a ideal (saturated with respect to the product of variables)
        inside the Laurent ring of the torus. The subvariety of X is given by its ideal in the Cox ring.
        It applies the isomorphism of the Laurent polynomials with the degree zero part
        of the localization with respect to the product of variables of the Cox ring.
    Example
        X = NormalToricVarieties$cartesianProduct apply((1,1), i-> toricProjectiveSpace i);
        S = ring X;
        f = 2 * S_1 + 3 * S_3 + S_0;
        torusIntersection(X,f)
        torusIntersection(X,ideal(f))
    Example
        X = toricProjectiveSpace 3;
        S = ring X;
        f = S_0*S_1*S_2+S_0*S_1*S_3+S_0*S_2*S_3+S_1*S_2*S_3;
        torusIntersection(X,f)
        I = ideal((S_1+S_2)*S_0, (S_1+S_2)*(S_0+S_1));
        torusIntersection(X,I)
///


doc ///
  Key
    classFromTropicalCox
    (classFromTropicalCox,NormalToricVariety,Ideal)
  Headline
    compute a toric cycle of X which class is the same as a given subvariety of X
  Usage
    classFromTropicalCox(X,I)
  Inputs
    X: NormalToricVariety
    I: Ideal
  Outputs
    D: ToricCycle
  Description
    Text
        This function calculates a toric cycle which class is the same of a subvariety of a
        normal toric variety X, under the hypothesis that X is smooth. The subvariety is given
        by its homogeneous ideal in the Cox ring of X.
    Example
        X = toricProjectiveSpace 3;
        S = ring X;
        I = ideal(S_0+S_1+S_2+1,S_0+2*S_1-S_2+4);
        classFromTropicalCox(X,I)
///

-------------------------------------------------
--
-- doc ///
--   Key
--     name of the type / method
--   Headline
--     headline
--   Usage
--     function(input)
--   Inputs
--     input: type of the input
--       some text
--   Outputs
--     output: type of the output
--       some text
--   Description
--     Text
--        Some text
--     Example
--        examples
-- ///
