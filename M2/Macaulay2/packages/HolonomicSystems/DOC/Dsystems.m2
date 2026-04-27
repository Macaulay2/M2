doc ///
  Key 
    toricIdealPartials
    (toricIdealPartials,Matrix,PolynomialRing)
  Headline
   create the toric ideal of an integer matrix
  Usage
    toricIdealPartials(A,D)
  Inputs
    A:Matrix
    D:PolynomialRing  
  Outputs
     :Ideal
      the toric ideal of the matrix $A$ in the polynomial ring of the partials inside of the Weyl algebra $D$.
  Description
    Text 
      A $d \times n$ integer matrix $A$ determines a GKZ hypergeometric system of PDEs 
      in the Weyl algebra $D_n$ over $\mathbb{C}$.  The matrix $A$ is associated to the toric ideal 
      $I_A$ in the polynomial subring $\mathbb{C}[\partial_1,...,\partial_n]$ of $D$.  A field of characteristic zero may be used instead of  $\mathbb{C}$.
      For more details, see [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Chapters 3 and 4].
    Example 
      A = matrix{{1,2,0},{-1,1,3}}
      D = makeWA(QQ[x_1..x_3])
      I = toricIdealPartials(A,D)
      describe ring I
///
    
doc ///
  Key
    gkz
    (gkz, Matrix, List)
    (gkz, Matrix, List, PolynomialRing)
  Headline
    create the A-hypergeometric system of Gelfand, Kapranov and Zelevinsky (GKZ)
  Usage
    gkz(A,b)
    gkz(A,b,D)
  Inputs
    A:Matrix
    b:List -- parameter vector
    D:PolynomialRing -- a Weyl algebra
  Outputs
    :Ideal 
      the GKZ hypergeometric system associated to the matrix $A$ and the parameter vector $b$ in the Weyl algebra $D$
  Description
    Text
      The GKZ hypergeometric system of PDE's associated to a $d \times n$
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
    [AppellF1,Vars]
    Vars
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
    eulerOperators
    (eulerOperators, Matrix, PolynomialRing)
    (eulerOperators, Matrix, List, PolynomialRing)
  Headline
    Euler Operators
  Usage
    eulerOperators(A, D)
    eulerOperators(A, b, D)
  Inputs
    A:Matrix
    b:List
    D:PolynomialRing
  Outputs
    :List
      of Euler operators
  Description
      Text
       Given a $d \times n$ integer matrix $A = (a_{ij})$ and a Weyl algebra in $n$ variables, produce the $d$ corresponding Euler operators $E_i = \sum_{j=1}^n a_{ij}x_jdj$.
       An optional list $b$ imposes a multigrading so that one can look for solutions to the Euler operatros of multidegree $b$.
    Example
      D = makeWeylAlgebra(QQ[x,y,z])
      A = matrix{{2,-7,5},{14,8,-1}}
      L = eulerOperators(A,D)
      Example
      D = makeWeylAlgebra(QQ[x,y,z])
      A = matrix{{2,-7,5},{14,8,-1}}
      b = {2,-3}
      L = eulerOperators(A,b,D)
  Caveat
    Ring input should be a Weyl algebra. Matrix input should have as many columns as variables of the Weyl algebra.
    List should have as many entries as there are rows of matrix.
  SeeAlso
    gkz
///
