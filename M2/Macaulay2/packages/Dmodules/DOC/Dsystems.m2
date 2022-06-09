doc ///
  Key 
    toricIdealPartials
    (toricIdealPartials,Matrix,PolynomialRing)
  Headline
   The toric ideal of the matrix A in the polynomial ring of the partials inside of the Weyl algerba D
  Usage
    toricIdealPartials(A,D)
  Inputs
    A:Matrix
    D:Polynomial ring  
  Outputs
     :Ideal
      the toric ideal of the matrix A in the polynomial ring of the partials inside of the Weyl algerba D.
  Description
    Text 
      A $d \times n$ integer matrix $A$ determines a GKZ hypergeometric system of PDEs 
      in the Weyl algebra $D_n$ over $\mathbb{C}$.  The matrix $A$ is associated to the toric ideal 
      $I_A$ in the polynomial subring $\mathbb{C}[\partial_1,...,\partial_n]$ of $D$.  A field of characteristic zero may be used instead of  $\mathbb{C}$.
      For more details, see [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Chapters 3 and 4].
    Example 
      A = matrix{{1,2,0},{-1,1,3}}
      D = makeWA(QQ[x_1..x_3])
///
    
doc ///
  Key
    gkz
    (gkz, Matrix, List)
    (gkz, Matrix, List, PolynomialRing)
  Headline
    The A-hypergeometric systems of Gelfand, Kapranov and Zelevinsky (GKZ)
  Usage
    gkz(A,b)
    gkz(A,b,D)
  Inputs
    A:Matrix
    b:List
    D:PolynomialRing
  Outputs
    :Ideal 
      the GKZ hypergeometric
      system associated to the matrix A and the parameter vector b in the 
      Weyl algebra D.
  Description
    Text
      The GKZ hypergeometric system of PDE's associated to a $d$ $\times$ $n$
      integer matrix A is an ideal in the Weyl algebra $D_n$ over $\mathbb{C}$
      with generators $x_1,\dots,x_n$ and $\partial_1,\dots,\partial_n$.
      It consists of the toric ideal $I_A$ in the polynomial
      subring  $\mathbb{C}[\partial_1,...,\partial_n]$ and Euler relations given by the entries
      of the vector (A $\theta$ - b), where $\theta$ is the vector
      $(\theta_1,...,\theta_n)^t$, and $\theta_i = x_i \partial_i$. 
      A field of characteristic zero may be used instead of  $\mathbb{C}$.
      For more details, see [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Chapters 3 and 4].
    Example
      A = matrix{{1,1,1},{0,1,2}}
      b = {3,4}
      I = gkz (A,b)
      describe ring I
    Text
      The ambient Weyl algebra can be determined as an input.
    Example
      D = makeWA(QQ[x_1..x_3])
      gkz(A,b,D)
    Text 
      One may separately produce the toric ideal and the Euler operators.
   Example
      toricIdealPartials(A,D)
      eulerOperators(A,b,D)
  Caveat
    gkz(A,b) always returns a different ring and will use 
    variables x_1,...,x_n, D_1,...D_n.
  SeeAlso
    AppellF1
///

doc ///
  Key
    AppellF1
    (AppellF1, List)
  Headline
    Appell F1 system of PDE's
  Usage
    AppellF1 {a0,a1,a2,a3}
  Inputs
    :List
  Outputs
    :Ideal
      which represents @HREF("https://en.wikipedia.org/wiki/Appell_series#Derivatives_and_differential_equations", "Appell F1")@ system of PDE's associated to the parameters a0, a1, a2, and a3.
  Description
    Example
      w = {1,4/5,-2,3/2}
      I = AppellF1 w
  Caveat
    AppellF1 always returns a different ring and will use variables x and y.
    Input should be a List of 4 numbers.
  SeeAlso
    gkz
///

doc ///
  Key
    Vars
    [AppellF1,Vars]
///

doc ///
  Key
    PolyAnn
    (PolyAnn, RingElement)
  Headline
    annihilator of a polynomial in the Weyl algebra
  Usage
    PolyAnn f
  Inputs
    f:RingElement
      polynomial
  Outputs
    :Ideal
      the annihilating (left) ideal of @{EM "f"}@ in the Weyl algebra
  Description
    Example
      makeWA(QQ[x,y])
      f = x^2-y^3
      I = PolyAnn f
  Caveat
    The input f should be an element of a Weyl algebra, and not an element of a commutative polynomial ring.
    However, f should only involve commutative variables.
  SeeAlso
    RatAnn
///

doc ///
  Key
    RatAnn
    (RatAnn, RingElement, RingElement)
    (RatAnn, RingElement)
  Headline
    annihilator of a rational function in Weyl algebra
  Usage
    RatAnn f
    RatAnn(g,f)
  Inputs
    f:RingElement
      polynomial
    g:RingElement
      polynomial
  Outputs
    :Ideal
      left ideal of the Weyl algebra
  Description
    Text
      @{TT "RatAnn f"}@ computes the annihilator ideal in the Weyl algebra 
      of the rational function $1/f$.
      @BR{}@
      @{TT "RatAnn(g,f)"}@ computes the annihilator ideal in the 
      Weyl algebra of the rational function $g/f$.
   Example
      makeWA(QQ[x,y])
      f = x^2-y^3
      g = 2*x*y
      I = RatAnn (g,f)
  Caveat
    The inputs f and g should be elements of a Weyl algebra, and not elements of a commutative polynomial ring.
    However, f and g should only use the commutative variables.
  SeeAlso
    PolyAnn
///

end
restart
installPackage("Dmodules",FileName =>"../../Dmodules.m2")

viewHelp toricIdealPartials
