doc ///
  Key
    (symbol |, Matrix, Matrix)
    (symbol |, RingElement, Matrix)
    (symbol |, Matrix, RingElement)
    (symbol |, RingElement, RingElement)
    (symbol |, Number, Matrix)
    (symbol |, Matrix, Number)
  Headline
    join matrices horizontally
  Usage
    f = g | h
  Inputs
    g:
      a @TT "m"@ by @TT "n"@ matrix
    h:
      a @TT "m"@ by @TT "r"@ matrix
  Outputs
    f:
      the @TT "m"@ by @TT "n+r"@ matrix, obtained from matrices @TT "g"@ and
      @TT "h"@ by concatenating the rows
  Description
    Example
      R = ZZ[i..p];
      g = matrix {{i,j},{k,l}}
      h = matrix {{m,n},{o,p}}
      f= g | h
    Text
      If one of the arguments is a ring element or a number, then it will be
      multiplied by a suitable identity matrix.
    Example
      f | (m-n)
  Caveat
    It is assumed that the matrices @TT "g"@ and @TT "h"@ have the same
    @TO Ring@.
  SeeAlso
    (symbol ||, Matrix, Matrix)
    (ring, Matrix)
///

doc ///
  Key
    (symbol ||, Net, Net)
  Headline
    join nets or strings vertically
  Description
    Text
      @TT "m||n"@ -- joins nets or strings by concatenating them vertically.
      The baseline of the result is the baseline of the first one.

      In this example, we build a large net with arrows to indicate the
      location of the baseline.
    Example
      x = "x" | "3"^1
      "<--- " | ( x || "" || x ) | " --->"
  SeeAlso
    "stack"
///

doc ///
  Key
    (symbol ||, Matrix, Matrix)
    (symbol ||, RingElement, Matrix)
    (symbol ||, Matrix, RingElement)
    (symbol ||, RingElement, RingElement)
    (symbol ||, Matrix, Number)
    (symbol ||, Number, Matrix)
  Headline
    join matrices vertically
  Usage
    f = g || h
  Inputs
    g:
      a @TT "m"@ by @TT "n"@ matrix.
    h:
      a @TT "r"@ by @TT "n"@ matrix.
  Outputs
    f:
      the @TT "m+r"@ by @TT "n"@ matrix, obtained from matrices @TT "g"@ and
      @TT "h"@ by concatenating the columns.
  Description
    Example
      R = ZZ[i..p];
      g = matrix {{i,j},{k,l}}
      h = matrix {{m,n},{o,p}}
      f= g || h
    Text
      If one of the arguments is a ring element or a number, then it will be
      multiplied by a suitable identity matrix.
    Example
      f || 33
  Caveat
    It is assumed that the matrices @TT "g"@ and @TT "h"@ have the same
    @TO Ring@.
  SeeAlso
    (symbol |, Matrix, Matrix)
    (ring, Matrix)
///


doc ///
  Key
    (symbol ||, Vector, Vector)
  Headline
    join vectors
  Usage
    v || w
  Inputs
    v:
    w:
  Outputs
    :Vector
      obtained from vectors v and w by concatenating the columns.
  Description
    Example
      R = (ZZ[x,y,z])^3;
      v = vector {1,x,x*y,x*z,x*y*z}
      w = vector {z*x,z^2,3}
      v || w
  SeeAlso
    (symbol ||, Matrix, Matrix)
///
