-- date: October 2018
-- author(s): Lily Silverstein
-- notes:

doc ///
  Key
    polarize
    (polarize, MonomialIdeal)
    [polarize, VariableBaseName]
  Headline
    given a monomial ideal, computes the squarefree monomial ideal obtained via polarization
  Usage
    polarize M
    polarize(M, VariableBaseName => "x")
  Inputs
    M: MonomialIdeal
    VariableBaseName => String
     specified letter or string for the new variables
  Outputs
    I: MonomialIdeal
       a squarefree monomial ideal in a new polynomial ring
  Description
    Text
      Polarization takes each minimal generator of a monomial ideal to a squarefree monomial
      in a new ring. The procedure is to define a new variable $z_{i,j}$ for the $j$th power of
      the $i$th variable in the original ring. For instance, polarizing the ideal $I=(x^3, y^2, xy)$, of the ring
      $\mathbb{Q}[x,y]$, results in the ideal $(z_{0,0}z_{0,1}z_{0,2}, z_{1,0}z_{1,1}, z_{0,0}z_{1,0})$ of
      $\mathbb{Q}[z_{0,0},z_{0,1},z_{0,2},z_{1,0},z_{1,1}]$.
      
      This is code adapted from the Monomial Ideals chapter, written by Greg Smith and Serkan Hosten, of
      {\bf Computations in algebraic geometry with Macaulay 2}. See @HREF
      "https://faculty.math.illinois.edu/Macaulay2/Book/ComputationsBook/chapters/monomialIdeals/chapter-wrapper.pdf"@ for
      the chapter PDF, and @HREF"https://faculty.math.illinois.edu/Macaulay2/Book/"@ for more information
      on this book.
    Example
      R = QQ[x,y,z];
      I = monomialIdeal(x^2,y^3,x*y^2*z,y*z^4);
      J = polarize(I)
    Text
      By default, the variables in the new rings are named $z_{i,j}$. To use a different
      letter (or longer string) instead of {\tt z}, use the {\tt VariableBaseName} option.
    Example
      R = QQ[a,b,c];
      I = monomialIdeal(a^2*b^2,b^2*c^2,a*b*c^4);
      J = polarize(I, VariableBaseName => "x")
      J = polarize(I, VariableBaseName => "foo")
    Text      
      Variables are always indexed from 0.
      To use an unindexed variable naming scheme, the polarized ideal 
      can always be mapped to a new ring after it is created. The 
      following code is one way to do this.
    Example
      S = ring J;
      T = QQ[a..h];
      F = map(T, S, first entries vars T);
      F(J)
  SeeAlso
   isSquareFree
   monomialIdeal
   "MinimalPrimes::radical"
   "substitution and maps between rings"
   "working with multiple rings"
///

TEST///
    R = QQ[x,y,z];
    I = monomialIdeal(x^2,y^3,x*y^2*z,y*z^4);
    J = polarize(I);
    assert(betti res I==betti res J)
///

TEST///
    R = QQ[x,y,z];
    I = monomialIdeal(x^2*y^2,y^2*z^2,x*y*z^4);
    J = polarize(I, VariableBaseName => "whyNotAWord");
    assert(betti res I==betti res J)
///
