doc ///
   Key
     toMonomial
   Headline
     Converts an exponent vector $L$ to a monomial in the variables of the polynomial ring $R$.
   Inputs
     R:PolynomialRing
       A polynomial ring.
     L:List
       An exponent vector with one entry per variable in @TT "vars R"@.
   Outputs
     mono:RingElement
       A monomial whose exponent vector is @TT "L"@.
   Usage 
     mono = toMonomial(R, L)
   Description
     Example
       gndR = QQ[x,y,z];
       toMonomial(gndR, {5,2,1})
///


doc ///
   Key
     leadCoef
     (leadCoef, RingElement)
   Headline
     Returns the coefficient of the lead monomial of f.
   Usage
     result = leadCoef(f)
   Inputs
     f:RingElement
   Outputs
     result:Thing
    	The coefficient of the lead monomial of f.
   Description       
     Example
       R = QQ[t_1, t_2];
       f = 5*t_1^2 + 7*t_2^2;
       leadCoef(f)
///


doc ///
   Key
     monoCoef
     (monoCoef, RingElement, RingElement)
   Headline
     For polynomial $p$ and monomial $m$, extract the (possibly non-constant) "coefficient" of $m$ in $p$.
   Inputs
     m:RingElement
       A monomial. 
     p:RingElement
       A polynomial from the same ring as m.
   Outputs
     coef:RingElement
       This function finds a polynomial @TT "coef"@ such that @TT "p == coef*m + [something]"@ where coef is "maximal."
   Usage 
     coef = monoCoef(m, p) 
   Description
     Text 
       The "coefficient" of the monomial @TT "m"@ may involve @TT "m"@ itself. 
       
       If m has an (actual) coefficient equal to something other than one, it is ignored
      
     Example
       gndR = QQ[x,y,z];
       p = x*y*z + z*y^2 + 2*x^2*y^2*z^2;
       m = x*y;
       monoCoef(m, p)
///
