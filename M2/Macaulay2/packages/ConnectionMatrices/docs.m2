--end--

doc ///
Key
    normalForm
    (normalForm, RingElement, RingElement)
    (normalForm, RingElement, List)
Headline
    computes normal form of an element with respect to a Groebner basis
Usage
    normalForm(f,g)
    normalForm(f,G)
Inputs
    f:RingElement
        in the Weyl algebra
    g:RingElement
        in the Weyl algebra
    G:List
        of elements in the Weyl algbra
Outputs
    normalForm:RingElement
        reduced form with respect to a list of elements in the Weyl algebra
Description
  Text
    This method computes the normal form of an elment f in the Weyl algebra D with repsect to another element in the D, or a whole list of such elements.
    Usually this list is a Groebner basis for an left D-ideal.
  Example
    w = {0,0,1,1};
    D = makeWA(QQ[x,y],w);
    f = dx^2 ; g = x*dx+1;
    normalForm(f, g)
Caveat
  Currently the output lives in the fraction field of the variables, but this is not the correctly implemented rational Weyl algebra
SeeAlso

///

doc ///
Key
    gaugeMatrix
    (gaugeMatrix, List, List, List)
Headline
    computes change of basis matrix from old standard monomial basis to a new one
Usage
    gaugeMatrix(G, B1, B2)
Inputs
    G:List
      of Groebner basis elements
    B1:List
      of standard monomial basis
    B2:List
      of standard monomial basis
Outputs
    M:Matrix
      encoding the change of basis from B1 to B2
Description
  Text
    Let I be a D-module, a Groebner basis G with respect to some monomial ordering on D and a basis of standard monomials B1 for the quotient ring in the rational Weyl algebra.
    Then this methods computes the change of basis matrix M over the rational Weyl algbra for another basis of the quotient.
  Example
    w1 = {0,0,2,1}; D1 = makeWeylAlgebra(QQ[x,y],w1);
    I = sub(ideal(x*dx^2-y*dy^2+2*dx-2*dy,x*dx+y*dy+1),D1);
    SM1 = stdMon(I);
    w2 = {0,0,1,2}; D2 = makeWeylAlgebra(QQ[x,y],w2);
    SM2 = stdMon(sub(I,D2));
    G = flatten entries gens gb I;
    gaugeMatrix(G,SM1,SM2)
Caveat
  Currently the output lives in the fraction field of the variables, but this is not the correctly implemented rational Weyl algebra
SeeAlso

///

doc ///
Key
    gaugeTransform
    (gaugeTransform, Matrix, List, PolynomialRing)
Headline
    computes the Gauge transform of a Pfaffian system
Usage
    gaugeTransform(M, P, R)
Inputs
    M:Matrix
      change of basis matrix
    P:List
      Pfaffian system, i.e. a list of matrices
    R:Ring
      the Weyl algebra
Outputs
    L:List
      the Pfaffian system P after the Gauge transform for each entry
Description
  Text
    This method computes the Gauge transform for a Pfaffian system given a change of basis.
  Example
    D = makeWeylAlgebra(QQ[x,y]);
    I = ideal(x*dx^2-y*dy^2+2*dx-2*dy, x*dx+y*dy+1); P = pfaffians(I);
    M = matrix{{x,0},{0,y}};
    gaugeTransform(M,P,D)
Caveat
  Currently the output lives in the fraction field of the variables adjoin differentials, but this is not the correctly implemented rational Weyl algebra
SeeAlso

///

doc ///
Key
    diffConnectionMatrix
    (diffConnectionMatrix, Ideal)
    (diffConnectionMatrix, List)
Headline
    computes the differential connection matrix
Usage
    diffConnectionMatrix(I)
    diffConnectionMatrix(P)
Inputs
    I:Ideal
      of the Weyl algebra
    P:List
      Pfaffian system, i.e. a list of connection matrices
Outputs
    M:Matrix
      the differential connection matrix
Description
  Text
    This method encodes the Pfaffian system, that is, its connection matrices in a single square matrix of size holonomic rank of the Pfaffian system.
    Its entries are differential one forms in the variables of the Weyl algebra.
    We can obtain the regular connection matrices by multiplying with a differential.
  Example
    D = makeWeylAlgebra(QQ[x,y]);
    I = ideal(x*dx^2-y*dy^2+2*dx-2*dy, x*dx+y*dy+1); P = pfaffians(I);
    diffConnectionMatrix(P)
    diffConnectionMatrix(I)
Caveat
  Currently the output lives in the fraction field of the variables adjoin differentials, but this is not the correctly implemented rational Weyl algebra
SeeAlso

///

doc ///
Key
    stdMon
    (stdMon, Ideal)
Headline
    computes the standard monomials for a D-ideal
Usage
    stdMon(I)
Inputs
    I:Ideal
      of the Weyl algebra
Outputs
    L:List
      of standard monomials
Description
  Text
    This method computes the standard monomial which constitute a basis of the D-modules obtained from quotienting the rational Weyl algebra by the ideal.
  Example
    D = makeWeylAlgebra(QQ[x,y]);
    I = ideal(x*dx^2-y*dy^2+2*dx-2*dy, x*dx+y*dy+1);
    stdMon(I)
Caveat
  Currently the output lives in the fraction field of the variables adjoin differentials, but this is not the correctly implemented rational Weyl algebra
SeeAlso

///