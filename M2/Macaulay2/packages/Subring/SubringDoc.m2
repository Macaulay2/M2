-*

   Copyright 2023, Oliver Clarke, Francesca Gandini,
   Casey Hill, Trevor Karn, Miranda Moore, Chris O'Neill
    
   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
*-


beginDocumentation()

doc ///
 Node
  Key
   Subrings
  Headline
   a package to deal with subrings
  Description
   Text
     {\em Subrings} is a package to give basic subroutines for subrings.
   Example
     needsPackage("Subrings")
   Text
     In particular, this package allows one to do computations in a subring
     and pass back and forth between the ambient ring and the subring.
     It also allows one to check if two subrings are secretly the same.
  Caveat
     There are other subring flavor things out there.
     For example, the SubalgebraBases package contains a subring. -- How do I reference other packages?
     That subring will eventually be deprecated in favor of this one.
  Subnodes
    Subring
    subring
    presentationIdeal
    presentationRing
    presentationMap
    isSubringElement
    toQuotientRing
    subringGenerators
    (symbol ==, Subring, Subring)
    (ambient, Subring)
    (net, Subring)
///

doc ///
    Key
    	Subring
    Headline
    	the class of finitely generated subrings of polynomial rings
    Description
	Text
	    To see how to specify a subring, see @TO subring @.

            A subring of @ ofClass{PolynomialRing} @ is a ring with unity
	    contained inside of another ambient ring that is closed
	    under the operations of the ambient ring.

	    The user can specify either @ ofClass{List} @ or @ ofClass{Matrix} @
	    whose entries are the generators.

    	Example
            R = QQ[x,y];
	    L = {x^2, y^2};
	    S = subring L

	    M = matrix({{x^2, y^2}})
	    S = subring M
///


doc ///
    Key
        subring
        (subring, Matrix)
        (subring, List)
    Headline
         Construct a subring of a polynomial ring
    Usage
        S = subring M
        S = subring L
    Inputs
        M:Matrix
            of generators for a subring of a @ ofClass{PolynomialRing} @
        L:List
            of generators for a subring of a @ ofClass{PolynomialRing} @
    Outputs
        S:Subring
            the subring of the polynomial ring
    Description
        Text
            An easy way to specify a subring is to specify the ambient ring and the generators of a desired subring as a matrix. The ambient ring is implicit in the constructor function.
        Example
            R = QQ[x,y]
            M = matrix(R, {{x^2, x*y, y^2}})
            S = subring M
        Text
            This function also accepts a list of elements of a polynomial ring as an input.
        Example
            R = QQ[x,y]
            L = {x^2, x*y, y^2}
            S = subring L
///


doc ///
    Key
        presentationIdeal
        (presentationIdeal, Subring)
    Headline
        Compute the presentation ideal of a subring
    Usage
        I = presentationIdeal S
    Inputs
        S:Subring
    Outputs
        I:Ideal
            the presentation ideal of the subring
    Description
        Text
            This function finds the presentation ideal of the subring, which is defined to
	    be the presentation ring modulo the presentation map.
        Example
            R = QQ[x,y]
            S = subring {x^2, x*y, y^2}
            I = presentationIdeal S
///

doc ///
  Key
   presentationRing
   (presentationRing, Subring)
  Headline
   a polynomial ring with a variable for each subring generator
  Usage
   P = presentationRing S
  Inputs
   S:Subring
  Outputs
   P:PolynomialRing
       a polynomial ring with one variable for each generator of the subring
  Description
   Text
    The {\tt presentationRing} of a subring {\tt S} is a polynomial ring
    with a variable for each generator of {\tt S}.
   Example
    R = QQ[x,y];
    g = {x^2, x*y, y^2};
    S = subring g;
    numgens presentationRing S
   Text
    This should not be confused with @TO toQuotientRing @,
    which yields a quotient of the presentation ring. In this example,
    even though there is an algebraic relation
    between the three generators, the presentation ring doesn't know
    that.
   Example
    P = presentationRing S;
    P_0*P_2 - P_1^2
///

doc ///
  Key
   presentationMap
   (presentationMap, Subring)
  Headline
   the map from the presentation ring into a subring
  Usage
   f = presentationMap S
  Inputs
   S:Subring
  Outputs
   f:RingMap
         the map sending polynomial ring generators to subring generators
  Description
   Text
    There exists a map sending each generator of the presentation ring to
    the corresponding element of the ambient ring. This is that map.
   Example
    R = QQ[x,y];
    g = {x^2, x*y, y^2};
    S = subring g;
    f = presentationMap S
    P = presentationRing S
    p = P_0 * P_2 - P_1^2
    f(p)
///


doc ///
  Key
   isSubringElement
   (isSubringElement, RingElement, Subring)
  Headline
   query if an element of ambient ring is in the subring
  Usage
   isSubringElement(x, S)
  Inputs
   x:RingElement
        an element of the ambient ring
   S:Subring
        the subring which may or may not contain x
  Description
   Text
    This determines if an element is in the subring.
    In the first example, it is.
   Example
    R = QQ[x,y];
    g = {x^2, x*y, y^2};
    S = subring g;
    isSubringElement(x^2 + x*y, S)
   Text
    In this example the element is not in the subring.
   Example
    isSubringElement(x, S)
///

doc ///
  Key
   toQuotientRing
   (toQuotientRing, Subring)
  Headline
   create a quotient ring isomorphic to the subring
  Usage
   Q = toQuotientRing S
  Inputs
   S:Subring
  Outputs
   Q:PolynomialRing
        isomorphic to the subring
  Description
   Text
    The subring is isomorphic to a quotient of a polynomial ring
    with a variable corresponding to each generator. This
    function returns such a quotient ring.
   Example
    R = QQ[x,y];
    g = {x^2, x*y, y^2};
    S = subring g;
    toQuotientRing(S)
   Text
    This should not be confused with @TO presentationRing@,
    which gives only the ambient ring of {\tt Q}.
///

doc ///
  Key
   subringGenerators
   (subringGenerators, Subring)
   (generators, Subring)
  Headline
   return the matrix of generators for the subring
  Usage
   M = subringGenerators S
   M = generators S
   M = gens S
  Inputs
   S:Subring
  Outputs
   M:Matrix
        containing the generators of {\tt S}.
  Description
   Text
    The subring is generated by elements of the ambient ring. This function
    returns a matrix (over the ambient ring) of these generators.
   Example
    R = QQ[x,y];
    g = {x^2, x*y, y^2};
    S = subring g;
    toQuotientRing(S)
   Text
    The generators are as given by the user. In particular, they may not
    be a minimal generating set if the generating set used to specify
    the ring is itself nonminimal.
   Example
    R = QQ[x,y];
    g = {x, x*y, y, y^2};
    S = subring g;
    subringGenerators(S)
   Text
    This method is also accessable through the generators method,
    and thus through the shortened gens method.
   Example
    R = QQ[x,y];
    g = {x^2, x*y, y^2};
    S = subring g;
    generators S
    gens S
///

doc ///
  Key
   (symbol ==, Subring, Subring)
  Headline
   check equality of subrings
  Usage
   S1 == S2
  Description
   Text
    Two different subrings may be equal despite being specified by
    different generating sets. This binary operator provides a way
    to check whether or not two subrings are the same by checking
    containments.
   Example
    R = QQ[x,y];
    g1 = {x^2, x*y, y^2};
    S1 = subring g1
    g2 = {x^2, x*y, y^2, x^2*y^2};
    S2 = subring g2
    S1 == S2
   Text
    The ambient ring must be literally the same for two
    subrings to be considered the same.
   Example
    R1 = QQ[x,y];
    g1 = {x^2, y^2};
    R2 = QQ[x,y];
    g2 = {x^2, y^2};
    R1 === R2
    subring(g1) == subring(g2)
   Text
    Things could also go wrong if the ambient rings have different
    numbers of generators.
   Example
    R3 = QQ[x,y,z];
    g3 = {x^2, y^2};
    subring(g1) == subring(g3)
    subring(g2) == subring(g3)
///

doc ///
  Key
   (ambient, Subring)
  Headline
   get the ambient ring
  Usage
   R = ambient S
  Inputs
   S:Subring
  Outputs
   R:PolynomialRing
        of which {\tt S} is understood to be a subset
  Description
   Text
    A subring is by definition a subset of another ring.
    This method returns that other ring.
   Example
    R = QQ[x,y];
    g = {x^2, x*y, y^2};
    S = subring g
    R === ambient S
///

doc ///
  Key
   (net, Subring)
  Headline
   format for printing the subring
  Usage
   m = net S
  Inputs
   S:Subring
  Outputs
   m:Net
        a succinct description of S
  Description
   Text
    This provides the expression for printing a `Subring`.
   Example
    R = QQ[x,y];
    g = {x^2, x*y, y^2};
    S = subring g
///
