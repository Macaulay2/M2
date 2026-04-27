--------------------------
-- arithmetic operators --
--------------------------

doc ///
  Key
    (symbol +, PythonObject, PythonObject)
    (symbol +, PythonObject, Thing)
    (symbol +, Thing, PythonObject)
    (symbol +, PythonObject)
  Headline
    add Python objects
  Usage
    x + y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- the sum of x and y
  Description
    Text
      Add two Python objects.
    Example
      toPython 2 + toPython 3
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object before adding.
    Example
      toPython 4 + 5
      6 + toPython 7
    Text
      It may also be used as a unary method.
    Example
      +toPython 6
  SeeAlso
    (symbol +=, PythonObject)
  Subnodes
    (symbol +=, PythonObject)
///

doc ///
  Key
    (symbol +=, PythonObject)
  Headline
    augmented addition of Python objects
  Usage
    x += y
  Inputs
    x:PythonObject
    y:Thing
  Outputs
    :PythonObject -- the sum of x and y, assigned to x
  Description
    Text
      Add two Python objects and assign the result to the first argument.
    Example
      x = toPython 2
      x += toPython 3
      x
    Text
      If the right-hand side is a Macaulay2 object, then it is first converted
      to a Python object before adding.
    Example
      x = toPython 4
      x += 5
      x
    Text
      If the Python class of @VAR "x"@ defines an @CODE "__iadd__"@ method
      for in-place addition, then it will be called.  Otherwise, @VAR "x"@ and
      @VAR "y"@ will be added in the usual way, creating a new Python object
      that is assigned back to @VAR "x"@.

      For example, Python lists support in-place addition.  In the example
      below, @VAR "x"@ is modified directly, and no new object is created.
    Example
      x = toPython {1, 2, 3}
      x += {4}
      x
  SeeAlso
    (symbol +, PythonObject, PythonObject)
    "Macaulay2Doc::augmented assignment"
///

doc ///
  Key
    (symbol -, PythonObject, PythonObject)
    (symbol -, PythonObject, Thing)
    (symbol -, Thing, PythonObject)
    (symbol -, PythonObject)
  Headline
    subtract Python objects
  Usage
    x - y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- the difference of x and y
  Description
    Text
      Subtract two Python objects.
    Example
      toPython 2 - toPython 3
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object before subtracting.
    Example
      toPython 4 - 5
      6 - toPython 7
    Text
      It may also be used as a unary method.
    Example
      -toPython 6
  SeeAlso
    (symbol -=, PythonObject)
  Subnodes
    (symbol -=, PythonObject)
///

doc ///
  Key
    (symbol -=, PythonObject)
  Headline
    augmented subtraction of Python objects
  Usage
    x -= y
  Inputs
    x:PythonObject
    y:Thing
  Outputs
    :PythonObject -- the difference of x and y, assigned to x
  Description
    Text
      Subtract two Python objects and assign the result to the first argument.
    Example
      x = toPython 5
      x -= toPython 3
      x
    Text
      If the right-hand side is a Macaulay2 object, then it is first converted
      to a Python object before subtracting.
    Example
      x = toPython 9
      x -= 5
      x
    Text
      If the Python class of @VAR "x"@ defines an @CODE "__isub__"@ method
      for in-place subtraction, then it will be called.  Otherwise, @VAR "x"@
      and @VAR "y"@ will be subtracted in the usual way, creating a new Python
      object that is assigned back to @VAR "x"@.

      For example, Python sets support in-place subtraction.  In the example
      below, @VAR "x"@ is modified directly, and no new object is created.
    Example
      x = toPython set {1, 2, 3}
      x -= set {3}
      x
  SeeAlso
    (symbol -, PythonObject, PythonObject)
    "Macaulay2Doc::augmented assignment"
///

doc ///
  Key
    (symbol *, PythonObject, PythonObject)
    (symbol *, PythonObject, Thing)
    (symbol *, Thing, PythonObject)
  Headline
    multiply Python objects
  Usage
    x * y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- the product of x and y
  Description
    Text
      Multiply two Python objects.
    Example
      toPython 2 * toPython 3
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object before multiplying.
    Example
      toPython 4 * 5
      6 * toPython 7
  SeeAlso
    (symbol @, PythonObject, PythonObject)
    (symbol *=, PythonObject)
  Subnodes
    (symbol *=, PythonObject)
///

doc ///
  Key
    (symbol *=, PythonObject)
  Headline
    augmented multiplication of Python objects
  Usage
    x *= y
  Inputs
    x:PythonObject
    y:Thing
  Outputs
    :PythonObject -- the product of x and y, assigned to x
  Description
    Text
      Multiply two Python objects and assign the result to the first argument.
    Example
      x = toPython 2
      x *= toPython 3
      x
    Text
      If the right-hand side is a Macaulay2 object, then it is first converted
      to a Python object before multiplying.
    Example
      x = toPython 4
      x *= 5
      x
    Text
      If the Python class of @VAR "x"@ defines an @CODE "__imul__"@ method
      for in-place multiplication, then it will be called.  Otherwise,
      @VAR "x"@ and @VAR "y"@ will be multiplied in the usual way,
      creating a new Python object that is assigned back to @VAR "x"@.

      For example, Python lists support in-place multiplication by an integer
      (i.e., repetition).  In the example below, @VAR "x"@ is modified
      directly, and no new object is created.
    Example
      x = toPython {1, 2, 3}
      x *= 3
      x
  SeeAlso
    (symbol *, PythonObject, PythonObject)
    "Macaulay2Doc::augmented assignment"
///


doc ///
  Key
    (symbol @, PythonObject, PythonObject)
    (symbol @, PythonObject, Thing)
    (symbol @, Thing, PythonObject)
  Headline
    matrix multiplication of Python objects
  Usage
    x @ y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- the product of x and y as matrices
  Description
    Text
      Multiply two Python objects (e.g., NumPy arrays) as matrices.
    CannedExample
      i1 : installNumPyMethods();

      i2 : a = toPython matrix {{1, 0}, {0, 1}}

      o2 = [[1 0]
            [0 1]]

      o2 : PythonObject of class numpy.ndarray

      i3 : b = toPython matrix {{4, 1}, {2, 2}}

      o3 = [[4 1]
            [2 2]]

      o3 : PythonObject of class numpy.ndarray

      i4 : a @ b

      o4 = [[4 1]
            [2 2]]

      o4 : PythonObject of class numpy.ndarray
    Text
      In Macaulay2, the @CODE "*"@ operator is used for matrix multiplication,
      but in NumPy, this results in componentwise multiplication.
    CannedExample
      i5 : a * b

      o5 = [[4 0]
            [0 2]]

      o5 : PythonObject of class numpy.ndarray
  SeeAlso
    (symbol *, PythonObject, PythonObject)
    (symbol @=, PythonObject)
    installNumPyMethods
  Subnodes
    (symbol @=, PythonObject)
///

doc ///
  Key
    (symbol @=, PythonObject)
  Headline
    augmented matrix multiplication of Python objects
  Usage
    x @= y
  Inputs
    x:PythonObject
    y:Thing
  Outputs
    :PythonObject -- the product of x and y as matrices, assigned to x
  Description
    Text
      Multiply two Python objects (e.g., NumPy arrays) as matrices and assign
      the result to the first argument.
    CannedExample
      i1 : installNumPyMethods();

      i2 : x = toPython matrix {{1, 2}, {3, 4}}

      o2 = [[1 2]
            [3 4]]

      o2 : PythonObject of class numpy.ndarray

      i3 : x @= toPython matrix {{5, 6}, {7, 8}}

      o3 = [[19 22]
            [43 50]]

      o3 : PythonObject of class numpy.ndarray

     i4 : x

     o4 = [[19 22]
           [43 50]]

     o4 : PythonObject of class numpy.ndarray
    Text
      If the right-hand side is a Macaulay2 object, then it is first converted
      to a Python object before multiplying.
    CannedExample
      i5 : x = toPython matrix {{1, 2}, {3, 4}}

      o5 = [[1 2]
            [3 4]]

      o5 : PythonObject of class numpy.ndarray

      i6 : x @= matrix {{8, 7}, {6, 5}}

      o6 = [[20 17]
            [48 41]]

      o6 : PythonObject of class numpy.ndarray

      i7 : x

      o7 = [[20 17]
            [48 41]]

      o7 : PythonObject of class numpy.ndarray
    Text
      If the Python class of @VAR "x"@ defines an @CODE "__imatmul__"@ method
      for in-place matrix multiplication, then it will be called.  Otherwise,
      @VAR "x"@ and @VAR "y"@ will be multiplied in the usual way,
      creating a new Python object that is assigned back to @VAR "x"@.

      For example, NumPy arrays support in-place matrix multiplication.  In the
      examples above, @VAR "x"@ was modified directly, and no new objects were
      created.
  SeeAlso
    (symbol @, PythonObject, PythonObject)
    "Macaulay2Doc::augmented assignment"
    installNumPyMethods
///

doc ///
  Key
    (symbol /, PythonObject, PythonObject)
    (symbol /, PythonObject, Thing)
    (symbol /, Thing, PythonObject)
  Headline
    true division of Python objects
  Usage
    x / y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- the true quotient of x and y
  Description
    Text
      Divide two Python objects, giving the "true" quotient, e.g., when dividing
      two integers, the result will be a float.
    Example
      toPython 5 / toPython 3
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object before dividing.
    Example
      toPython 9 / 5
      11 / toPython 7
  SeeAlso
    (symbol //, PythonObject, PythonObject)
    (symbol /=, PythonObject)
  Subnodes
    (symbol /=, PythonObject)
///

doc ///
  Key
    (symbol /=, PythonObject)
  Headline
    augmented true division of Python objects
  Usage
    x /= y
  Inputs
    x:PythonObject
    y:Thing
  Outputs
    :PythonObject -- the true quotient of x and y, assigned to x
  Description
    Text
      Divide two Python objects, giving the "true" quotient, e.g., when
      dividing two integers, the result will be a float.  Then assign the
      result to the first argument.
    Example
      x = toPython 5
      x /= toPython 3
      x
    Text
      If the right-hand side is a Macaulay2 object, then it is first converted
      to a Python object before dividing.
    Example
      x = toPython 12
      x /= 5
      x
    Text
      If the Python class of @VAR "x"@ defines an @CODE "__itruediv__"@ method
      for in-place true division, then it will be called.  Otherwise,
      @VAR "x"@ and @VAR "y"@ will be divided in the usual way,
      creating a new Python object that is assigned back to @VAR "x"@.

      For example, NumPy arrays support in-place true division.  In the example
      below, @VAR "x"@ is modified  directly, and no new object is created.
    CannedExample
      i7 : installNumPyMethods();

      i8 : x = toPython matrix(RR, {{1, 2}, {3, 4}})

      o8 = [[1. 2.]
            [3. 4.]]

      o8 : PythonObject of class numpy.ndarray

      i9 : x /= matrix(RR, {{5, 6}, {7, 8}})

      o9 = [[0.2        0.33333333]
            [0.42857143 0.5       ]]

      o9 : PythonObject of class numpy.ndarray

      i10 : x

      o10 = [[0.2        0.33333333]
             [0.42857143 0.5       ]]

      o10 : PythonObject of class numpy.ndarray
  SeeAlso
    (symbol /, PythonObject, PythonObject)
    "Macaulay2Doc::augmented assignment"
    installNumPyMethods
///

doc ///
  Key
    (symbol //, PythonObject, PythonObject)
    (symbol //, PythonObject, Thing)
    (symbol //, Thing, PythonObject)
  Headline
    floor division of Python objects
  Usage
    x // y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- the floor quotient of x and y
  Description
    Text
      Divide two Python objects, giving the "floor" quotient, e.g., when
      dividing two integers, the result is the quotient using Euclidean
      division.
    Example
      toPython 5 // toPython 3
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object before dividing.
    Example
      toPython 9 // 5
      11 // toPython 7
    Text
      Note that the behavior of floor division for integers differs between
      Macaulay2 and Python.  In particular, in Macaulay2, the quotient is
      rounded down when @VAR "y"@ is positive and up when @VAR "y"@ is
      negative.  In Python, the quotient is always rounded down.
    Example
      5 // -3
      toPython 5 // -3
  SeeAlso
    (symbol /, PythonObject, PythonObject)
    (symbol %, PythonObject, PythonObject)
    (quotientRemainder, PythonObject, PythonObject)
    (symbol //=, PythonObject)
  Subnodes
    (symbol //=, PythonObject)
///

doc ///
  Key
    (symbol //=, PythonObject)
  Headline
    augmented floor division of Python objects
  Usage
    x //= y
  Inputs
    x:PythonObject
    y:Thing
  Outputs
    :PythonObject -- the floor quotient of x and y, assigned to x
  Description
    Text
      Divide two Python objects, giving the "floor" quotient, e.g., when
      dividing two integers, the result is the quotient using Euclidean
      division.  Then assign the result to the first argument.
    Example
      x = toPython 5
      x //= toPython 3
      x
    Text
      If the right-hand side is a Macaulay2 object, then it is first converted
      to a Python object before dividing.
    Example
      x = toPython 12
      x //= 5
      x
    Text
      If the Python class of @VAR "x"@ defines an @CODE "__ifloordiv__"@ method
      for in-place floor division, then it will be called.  Otherwise,
      @VAR "x"@ and @VAR "y"@ will be divided in the usual way,
      creating a new Python object that is assigned back to @VAR "x"@.

      For example, NumPy arrays support in-place floor division.  In the example
      below, @VAR "x"@ is modified  directly, and no new object is created.
    CannedExample
      i7 : installNumPyMethods();

      i8 : x = toPython matrix {{5, 6}, {7, 8}}

      o8 = [[1 2]
            [3 4]]

      o8 : PythonObject of class numpy.ndarray

      i9 : x //= matrix {{1, 2}, {3, 4}}

      o9 = [[5 3]
            [2 2]]

      o9 : PythonObject of class numpy.ndarray

      i10 : x

      o10 = [[5 3]
             [2 2]]

      o10 : PythonObject of class numpy.ndarray
  SeeAlso
    (symbol //, PythonObject, PythonObject)
    "Macaulay2Doc::augmented assignment"
    installNumPyMethods
///

doc ///
  Key
    (symbol %, PythonObject, PythonObject)
    (symbol %, PythonObject, Thing)
    (symbol %, Thing, PythonObject)
  Headline
    the modulo operator for Python objects
  Usage
    x % y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- the remainder when x is divided by y
  Description
    Text
      Perform the modulo operator on Python objects.
    Example
      toPython 5 % toPython 3
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object before dividing.
    Example
      toPython 9 % 5
      11 % toPython 7
    Text
      Note that the behavior of the modulo operator for integers differs between
      Macaulay2 and Python.  In particular, in Macaulay2, the remainder is
      always nonnegative. In Python, the remainder will have the same sign
      as @VAR "y"@.
    Example
      5 % -3
      toPython 5 % -3
  SeeAlso
    (symbol //, PythonObject, PythonObject)
    (quotientRemainder, PythonObject, PythonObject)
    (symbol %=, PythonObject)
  Subnodes
    (symbol %=, PythonObject)
///

doc ///
  Key
    (symbol %=, PythonObject)
  Headline
    augmented modulo operator for Python objects
  Usage
    x %= y
  Inputs
    x:PythonObject
    y:Thing
  Outputs
    :PythonObject -- the remainder when x is divided by y, assigned to x
  Description
    Text
      Divide two Python objects, giving the remainder, and assign the result to
      the first argument.
    Example
      x = toPython 5
      x %= toPython 3
      x
    Text
      If the right-hand side is a Macaulay2 object, then it is first converted
      to a Python object before dividing.
    Example
      x = toPython 12
      x %= 5
      x
    Text
      If the Python class of @VAR "x"@ defines an @CODE "__imod__"@ method
      for in-place modulo, then it will be called.  Otherwise,
      @VAR "x"@ and @VAR "y"@ will be divided in the usual way,
      creating a new Python object that is assigned back to @VAR "x"@.

      For example, NumPy arrays support in-place modulo.  In the example
      below, @VAR "x"@ is modified  directly, and no new object is created.
    CannedExample
      i7 : installNumPyMethods();

      i8 : x = toPython matrix {{7, 8}, {9, 10}}

      o8 = [[ 7  8]
            [ 9 10]]

      o8 : PythonObject of class numpy.ndarray

      i9 : x %= matrix {{3, 4}, {5, 6}}

      o9 = [[1 0]
            [4 4]]

      o9 : PythonObject of class numpy.ndarray

      i10 : x

      o10 = [[1 0]
             [4 4]]

      o10 : PythonObject of class numpy.ndarray
  SeeAlso
    (symbol %, PythonObject, PythonObject)
    "Macaulay2Doc::augmented assignment"
    installNumPyMethods
///

doc ///
  Key
    (quotientRemainder, PythonObject, PythonObject)
    (quotientRemainder, PythonObject, Thing)
    (quotientRemainder, Thing, PythonObject)
  Headline
    get the quotient and remainder when dividing Python objects
  Usage
    quotientRemainder(x, y)
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :Sequence -- containing two Python objects
  Description
    Text
      This returns a sequence containing both the quotient and remainder when
      dividing the first argument by the second.  Note that the return value
      is a Macaulay2 sequence and not a Python tuple.  This makes it useful
      with parallel assignment.
    Example
      (q, r) = quotientRemainder(toPython 5, toPython 3)
      q
      r
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object before dividing.
    Example
      quotientRemainder(toPython 9, 5)
      quotientRemainder(11, toPython 7)
    Text
      Note that the behavior of this function for integers differs between
      Macaulay2 and Python.  In particular, in Macaulay2, the quotient is
      rounded down when @VAR "y"@ is positive and up when it is negative, and
      the remainder is always nonnegative. In Python, the quotient is always
      rounded down and the remainder will have the same sign as @VAR "y"@.
    Example
      quotientRemainder(5, -3)
      quotientRemainder(toPython 5, -3)
  SeeAlso
    (symbol //, PythonObject, PythonObject)
    (symbol %, PythonObject, PythonObject)
///

doc ///
  Key
    (symbol ^, PythonObject, PythonObject)
    (symbol ^, PythonObject, Thing)
    (symbol ^, Thing, PythonObject)
    (symbol **, PythonObject, PythonObject)
    (symbol **, PythonObject, Thing)
    (symbol **, Thing, PythonObject)
  Headline
    exponentiation of Python objects
  Usage
    x ^ y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- x raised to the y power
  Description
    Text
      Raise one Python object to the power of another.
    Example
      (toPython 2)^(toPython 3)
    Text
      In addition to the Macaulay2-style @CODE "^"@, this operation is also
      available using the Python-style @CODE "**"@.
    Example
      toPython 2 ** toPython 3
    Text
      Note that parentheses were not necessary in the latter case since
      @CODE "**"@ has much lower precedence than @CODE "^"@.

      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object before exponentiating.
    Example
      (toPython 2)^3
      4^(toPython 5)
  Caveat
    In Python, the @CODE "^"@ operator corresponds to the bitwise xor operation.
    We use @CODE "^^"@ in this case for consistency with the rest of Macaulay2.
  SeeAlso
    (symbol ^^, PythonObject, PythonObject)
    (symbol ^=, PythonObject)
  Subnodes
    (symbol ^=, PythonObject)
///

doc ///
  Key
    (symbol ^=, PythonObject)
    (symbol **=, PythonObject)
  Headline
    augmented exponentiation for Python objects
  Usage
    x ^= y
  Inputs
    x:PythonObject
    y:Thing
  Outputs
    :PythonObject -- x raised to the y power, assigned to x
  Description
    Text
      Raise one Python object to the power of another, and assign the result to
      the first argument.
    Example
      x = toPython 2
      x ^= toPython 3
      x
    Text
      In addition to the Macaulay2-style @CODE "^="@, this operation is also
      available using the Python-style @CODE "**="@.
    Example
      x = toPython 6
      x **= toPython 2
      x
    Text
      If the right-hand side is a Macaulay2 object, then it is first converted
      to a Python object before exponentiating.
    Example
      x = toPython 16
      x ^= 2
      x
    Text
      If the Python class of @VAR "x"@ defines an @CODE "__ipow__"@ method
      for in-place exponentiation, then it will be called.  Otherwise,
      @VAR "x"@ will be raised to the @VAR "y"@ power in the usual way,
      creating a new Python object that is assigned back to @VAR "x"@.

      For example, NumPy arrays support in-place exponentiation.  In the example
      below, @VAR "x"@ is modified  directly, and no new object is created.
    CannedExample
      i7 : installNumPyMethods();

      i8 : x = toPython matrix {{1, 2}, {3, 4}}

      o8 =  [[1 2]
             [3 4]]

      o8 : PythonObject of class numpy.ndarray

      i9 : x ^= 2

      o9 = [[ 1  4]
            [ 9 16]]

      o9 : PythonObject of class numpy.ndarray

      i10 : x

      o10 = [[ 1  4]
             [ 9 16]]

      o10 : PythonObject of class numpy.ndarray
  SeeAlso
    (symbol ^, PythonObject, PythonObject)
    "Macaulay2Doc::augmented assignment"
    installNumPyMethods
///
