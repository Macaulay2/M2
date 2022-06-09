doc ///
  Key
    AnnFs
    (AnnFs, RingElement)
    (AnnFs, List)
  Headline
    differential annihilator of a polynomial in a Weyl algebra
  Usage
    AnnFs(f)
    AnnFs(L)
  Inputs
    f:RingElement
      polynomial in a Weyl algebra $D$
    L:List
      of polynomials in the Weyl algebra $D$
  Outputs
     :Ideal
      the differential annihilator of $f$ in $D[s]$, for a new variable $s$, or the differential 
      annihilator of $L$ in $D[t_0,..,t_0,dt_0,..,dt_k]$, where $L$ is a list of $k+1$ elements 
      and the $t_i$ are new variables.
  Description
    Text
     This routine computes the ideal of the differential annihilator of a polynomial or list of 
     polynomials in a Weyl algebra $D$. This ideal is a left ideal of the ring $D[s]$ 
     or $D[t_0,..,t_k,dt_0,..,dt_k]$.  More
     details can be found in 
     [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Chapter 5].  
     The computation in the case of the element $f$ is via  Algorithm 5.3.6, and the computation
     in the case of the list $L$ is via the Algorithm 5.3.15.
    Example
      makeWA(QQ[x,y])
      f = x^2+y
      AnnFs f
    Example
      makeWA(QQ[x,y,z])
      L = {x^3,y+5*z}
  Caveat
     Must be over a ring of characteristic $0$.
///

doc ///
  Key
    diffRatFun
    diffRatFun (List, RingElement)
    diffRatFun (List, RingElement, RingElement, ZZ)
  Headline
    derivative of a rational function in a Weyl algebra
  Usage
    diffRatFun(m,f)
    diffRatFun(m,g,f,a)
  Inputs
    f:RingElement
      polynomial in a Weyl algebra $D$ in $n$ variables or ratinoal function in the fraction
      field of a polynomial ring in $n$ variables
    g:RingElement
      polynomial in a Weyl algebra $D$ in $n$ variables
    m:List 
      of nonnegative integers $m = \{m_1,...,m_n\}$
    a:ZZ
      an integer
   Outputs
     :RingElement
      the result of applying the product of the $(dx_i)^{m_i}$ to $f$ 
     :(RingElement,RingElement,ZZ)
       the result of applying the product of the $(dx_i)^{m_i}$ to $g/f^a$, written as 
       (numerator,denominator,power of denominator)
   Description
    Text
     Let $D$ be a Weyl algebra in the variables $x_1,..x_n$ and partials $dx_1,..,dx_n$.  Let $f$ 
     be either a polynomial or rational function in the $x_i$ and $m = (m_1,..,m_n)$ a list 
     of nonnegative integers.  The function $f$ may be given as an element of a polynomial ring in the $x_i$
     or of the fraction field of that polynomial ring or of $D$.  This method applies the product of the 
     $dx_i^{m_i}$ to $f$. In the case of the input $(m,g,f,a)$, where $f \neq 0$ and $g$ are both 
     polynomials and $a$ is a nonnegative integer, it applies the product of the $dx_i^{m_i}$ to $g/f^a$ 
     and returns the resulting derivative as (numerator,denominator,power of denominator), not necessarily 
     in lowest terms.
    Example 
     QQ[x,y,z]
     m = {1,1,0}
     f = x^2*y+z^5
     diffRatFun(m,f)
    Example 
     makeWA(QQ[x,y,z])
     m = {1,1,0}
     f = x^2*y+z^5
     diffRatFun(m,f)
    Example
     frac(QQ[x,y])
     m = {1,2}
     f = x/y
     diffRatFun(m,f)
    Example
     makeWA(QQ[x,y,z])
     m = {1,2,1}
     g = z
     f = x*y
     a = 3
     diffRatFun(m,g,f,a)
    Caveat
     Must be over a ring of characteristic $0$.  
///

