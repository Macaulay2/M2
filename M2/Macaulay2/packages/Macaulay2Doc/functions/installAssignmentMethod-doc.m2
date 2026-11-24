doc ///
  Key
    "installing assignment methods"
    installAssignmentMethod
    (installAssignmentMethod,Symbol,HashTable,Option)
    (installAssignmentMethod,Symbol,HashTable,HashTable,Option)
    (installAssignmentMethod,Symbol,HashTable,Function)
    (installAssignmentMethod,Symbol,HashTable,HashTable,Function)
  Headline
    install methods assignment to the value of an operator
  Usage
    installAssignmentMethod(op,X,Y,f)
  Inputs
    op:Keyword
      the symbol for a binary operator
    X:Type
      the type of the left-hand operand
    Y:Type
      the type of the right-hand operand
    f:Function
      the method function that will perform the assignment
  Consequences
    Item
      If the operator to be used is @TT "**"@, say, then @TT "op"@ should be
      @TT "symbol **"@. After installing the assignment method, assignments of
      the form @TT "x**y=v"@ will be handled by evaluating @TT "f(x,y,v)"@.
  Description
    Text
      If the operator is a unary operator, just omit @TT "X"@ and its comma.

      We illustrate this function by defining a type of mutable list whose
      elements can be both extracted and replaced using the operator @TT "_"@.
      We also install a method for @TO "net"@ so objects of the class can be
      printed.
    Example
      M = new Type of MutableList
      net M := m -> peek m
      M _ ZZ := (x,i) -> x#i
      installAssignmentMethod(symbol _, M, ZZ, (x,i,v) -> x#i = v);
      y = new M from (a..z)
      y_12
      y_12 = foo
      y
      y_12
    Text
      For sample applications of this facility, see
      @TO ((symbol _, symbol =), Symbol, Thing)@ and
      @TO ((symbol _, symbol =), MutableMatrix, Sequence)@.
  Caveat
    The syntax involved here is not pleasant, so we may change it to something
    briefer, something involving yet another assignment operator, distinct from
    @TT "="@ and @TT ":="@.
  SeeAlso
    MutableList
    (symbol #,BasicList,ZZ)
///

doc ///
  Key
    ((symbol _,symbol =),Symbol,Thing)
  Headline
    assignment to an indexed variable
  Usage
    -- this node documents the assignment method for x_a=v where x is a symbol
    x_a = v
  Inputs
    x:
    a:
    v:
  Outputs
    v:
  Consequences
    Item
      the indexed variable @TT "x_a"@ is assigned the value @TT "v"@.
  Description
    Example
      x_a
      x_a = b
      x_a
      x_c
      x_c = d
      x_c
    Text
      The source code explains how it works, behind the scenes.
    Code
      PRE ("    " | net code {(symbol _,Symbol,Thing),((symbol _,symbol =),Symbol,Thing)})
    Example
      peek x
  SeeAlso
    IndexedVariable
///

doc ///
  Key
    ((symbol _,symbol =),MutableMatrix,Sequence)
  Headline
    assignment to an element of a mutable matrix
  Usage
    f_(i,j) = r
  Inputs
    f:
    :Nothing
      a pair @TT "(i,j)"@ of integers
    r:
  Consequences
    Item
      the @TT "(i,j)"@ entry of @TT "f"@ is replaced by the ring element
      @TT "r"@
  Description
    Example
      R = QQ[a..d]
      f = mutableMatrix vars R
      f_(0,2)
      f_(0,2) = 7*c^2
      f
///
