doc ///
  Key
    (diff,RingElement,RingElement)
  Headline
    differentiation
  Usage
    diff(x,f)
  Inputs
    x:
      a polynomial
    f:
      a polynomial in the same ring as @TT "x"@
  Outputs
    -- Mike wanted this: TO2("differential operator corresponding to a polynomial", "differential operator"),
    :
      the result of differentiating @TT "f"@ by the differential operator
      corresponding to @TT "x"@.
  Description
    Text
      If @TT "x"@ is an indeterminate this is simply the usual differentiation.
    Example
      R = QQ[x,y,z];
      diff(x,x^7 + 4*x^3*y - 3*y)
      diff(x^2+y^2+z^2, y^2*z^2 - x^3 - 1)
    Text
      Here is a shortcut that can save some typing.
    Example
      diff_x x^6
  SeeAlso
    contract
    jacobian
    -- Mike wanted this: "differential operator corresponding to a polynomial",
    "diff and contract"
///
doc ///
  Key
    (diff,RingElement,Matrix)
  Headline
    differentiate each entry of a matrix
  Usage
    diff(x,f)
  Inputs
    x:
      a polynomial
    f:
      a matrix between free modules over the same ring as @TT "x"@
  Outputs
    -- Mike wanted this: TO2("differential operator corresponding to a polynomial", "differential operator"),
    :Matrix
      having the same shape as f, whose (i,j) entry is the result of
      differentiating @TT "f_(i,j)"@ by the differential operator corresponding
      to @TT "x"@.
  Description
    Text
      The shape of the resulting matrix is the same as the shape of f, but the
      degrees of the source module are different in an attempt to ensure that
      the result is homogeneous.
    Example
      R = QQ[x,y,z];
      f = matrix{{x^2-y*z, x*y*z + z^4}, {x-1, 2*y^2+z^2-1}}
      diff(x,f)
      diff(x^2-y*z,f)
  SeeAlso
    contract
    jacobian
    -- Mike wanted this: "differential operator corresponding to a polynomial",
    "diff and contract"
///
doc ///
  Key
    (diff,Matrix,RingElement)
  Headline
    differentiation
  Usage
    diff(f,g)
  Inputs
    f:
      a matrix
    g:
      a polynomial with the same ring as @TT "f"@
  Outputs
    -- Mike wanted this: TO2("differential operator corresponding to a polynomial", "differential operator"),
    :
      the result of differentiating @TT "g"@ by the differential operator
      corresponding to each entry of @TT "f"@.
  Description
    Text
      The shape of the resulting matrix is the same as the shape of f, but the
      degrees of the source module are different in an attempt to ensure that
      the result is homogeneous.
    Example
      R = QQ[x,y,z,q];
      f = vars R
      diff(f, (x+y-z)^2)
      f2 = genericMatrix(R,2,2)
      diff(f2, (x+y-z)^2)
  SeeAlso
    contract
    jacobian
    -- Mike wanted this: "differential operator corresponding to a polynomial",
    "diff and contract"
///
doc ///
  Key
    (diff,Matrix,Matrix)
    (diff, Matrix, Vector)
    (diff, RingElement, Vector)
    (diff, Vector, Matrix)
    (diff, Vector, RingElement)
    (diff, Vector, Vector)
  Headline
    differentiate a matrix by a matrix
  Usage
    diff(m,n)
  Inputs
    m:
      a map @TT "m : F <--- P"@ between free modules of ranks f and p.
    n:
      a map @TT "n : G <--- Q"@ between free modules of ranks g and q.
  Outputs
    -- Mike wanted this: TO2("differential operator corresponding to a polynomial", "differential operator"),
    :Matrix
      with the shape @TT "h : dual F ** G <--- dual P ** Q"@, whose entry in
      the slot @TT {"h", SUB "g*i+j,q*k+l"}@ is the result of differentiating
      @TT {"n", SUB "j,l"}@ by the differential operator corresponding to
      @TT {"m", SUB "i,k", "."}@
  Description
    Text
      The arguments @TT "m"@ and @TT "n"@ may also be vectors or ring elements.
    Example
      R = QQ[a,b,c][x,y,z]
      m = transpose vars R
      n = matrix{{x^2-a*y^3, x^3-z^2*y, x*y-b, x*z-c}}
      diff(m,n)
  SeeAlso
    diff'
    contract
    -- Mike wanted this: "differential operator corresponding to a polynomial",
    "diff and contract"
///
doc ///
  Key
    (diff,ProjectiveHilbertPolynomial,ZZ)
  Description
    Text
      @TT "diff(P,i)"@ -- compute the i-th difference polynomial
///
doc ///
  Key
    (diff, ProjectiveHilbertPolynomial)
  Description
    Text
      @TT "diff P"@ -- compute the difference polynomial.
///
doc ///
  Key
    diff
  Headline
    differentiate or take difference
  Usage
    diff(f,g) or diff(P) or diff(P,i)
  Description
    Text
      This function has two different uses. The most common use is for
      differentiation: differentiate the second input by the first.

      The second use, less common but sometimes useful, is to compute the
      difference polynomial of a Hilbert polynomial.

      The arguments can also be ring elements or vectors.
    Example
      R = ZZ[x,y,z]
      f = vars R ** vars R
      diff(transpose vars R, f)
      diff(x, f)
      diff(y, f)
      diff(z, f)
  SeeAlso
    -- Mike wanted this: "differential operator corresponding to a polynomial",
    "diff and contract"
    diff'
    contract
    jacobian
    hilbertPolynomial
  Subnodes
    :differentiation
    (diff,RingElement,RingElement)
    (diff,RingElement,Matrix)
    (diff,Matrix,RingElement)
    (diff,Matrix,Matrix)
    :difference operator for Hilbert polynomials
    (diff,ProjectiveHilbertPolynomial)
    (diff,ProjectiveHilbertPolynomial,ZZ)
///

doc ///
  Key
    (diff', Matrix, Matrix)
    diff'
  Headline
    differentiate a matrix by a matrix, the dual notion
  Usage
    h = diff'(m,n)
  Inputs
    m:
      a map @TT "m : F <--- P"@ between free modules of ranks f and p.
    n:
      a map @TT "n : G <--- Q"@ between free modules of ranks g and q.
  Outputs
    h:
      a matrix with the shape @TT "h : F ** dual G <--- P ** dual Q"@, whose
      entry in the slot @TT {"h", SUB "g*i+j,q*k+l"}@ is the result of
      differentiating @TT {"n", SUB "j,l"}@, by @TT {"m", SUB "i,k"}@
  SeeAlso
    diff
    "diff and contract"
///
