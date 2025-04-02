-----------------------
-- bitwise operators --
-----------------------

doc ///
  Key
    (symbol <<, PythonObject, PythonObject)
    (symbol <<, PythonObject, Thing)
    (symbol <<, Thing, PythonObject)
  Headline
    the left shift operator for Python objects
  Usage
    x << y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- the result of shifting x left by y
  Description
    Text
      Perform the left shift operator on Python objects.
    Example
      toPython 1 << toPython 3
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object before shifting.
    Example
      toPython 1 << 4
      1 << toPython 5
  SeeAlso
    (symbol >>, PythonObject, PythonObject)
    (symbol <<=, PythonObject)
  Subnodes
    (symbol <<=, PythonObject)
///

doc ///
  Key
    (symbol <<=, PythonObject)
  Headline
    augmented left shift for Python objects
  Usage
    x <<= y
  Inputs
    x:PythonObject
    y:Thing
  Outputs
    :PythonObject -- x shifted left by y, assigned to x
  Description
    Text
      Perform the left shift operator on Python objects, and assign the result to
      the first argument.
    Example
      x = toPython 1
      x <<= toPython 3
      x
    Text
      If the right-hand side is a Macaulay2 object, then it is first converted
      to a Python object before shifting.
    Example
      x = toPython 1
      x <<= 5
      x
    Text
      If the Python class of @VAR "x"@ defines an @CODE "__ilshift__"@ method
      for in-place left shift, then it will be called.  Otherwise,
      @VAR "x"@ will be shifted by @VAR "y"@ in the usual way,
      creating a new Python object that is assigned back to @VAR "x"@.

      For example, NumPy arrays support in-place left shift.  In the example
      below, @VAR "x"@ is modified  directly, and no new object is created.
    CannedExample
      i7 : installNumPyMethods();

      i8 : x = toPython matrix {{1, 2}, {4, 8}}

      o8 =  [[1 2]
             [4 8]]

      o8 : PythonObject of class numpy.ndarray

      i9 : x <<= 2

      o9 = [[ 4  8]
            [16 32]]

      o9 : PythonObject of class numpy.ndarray

      i10 : x

      o10 = [[ 4  8]
             [16 32]]

      o10 : PythonObject of class numpy.ndarray
  SeeAlso
    (symbol <<, PythonObject, PythonObject)
    "Macaulay2Doc::augmented assignment"
    installNumPyMethods
///

doc ///
  Key
    (symbol >>, PythonObject, PythonObject)
    (symbol >>, PythonObject, Thing)
    (symbol >>, Thing, PythonObject)
  Headline
    the right shift operator for Python objects
  Usage
    x >> y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- the result of shifting x right by y
  Description
    Text
      Perform the right shift operator on Python objects.
    Example
      toPython 128 >> toPython 3
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object before shifting.
    Example
      toPython 256 >> 5
      16384 >> toPython 4
  SeeAlso
    (symbol <<, PythonObject, PythonObject)
    (symbol >>=, PythonObject)
  Subnodes
    (symbol >>=, PythonObject)
///

doc ///
  Key
    (symbol >>=, PythonObject)
  Headline
    augmented right shift for Python objects
  Usage
    x >>= y
  Inputs
    x:PythonObject
    y:Thing
  Outputs
    :PythonObject -- x shifted right by y, assigned to x
  Description
    Text
      Perform the right shift operator on Python objects, and assign the result to
      the first argument.
    Example
      x = toPython 8
      x >>= toPython 3
      x
    Text
      If the right-hand side is a Macaulay2 object, then it is first converted
      to a Python object before shifting.
    Example
      x = toPython 32
      x >>= 5
      x
    Text
      If the Python class of @VAR "x"@ defines an @CODE "__irshift__"@ method
      for in-place right shift, then it will be called.  Otherwise,
      @VAR "x"@ will be shifted by @VAR "y"@ in the usual way,
      creating a new Python object that is assigned back to @VAR "x"@.

      For example, NumPy arrays support in-place right shift.  In the example
      below, @VAR "x"@ is modified  directly, and no new object is created.
    CannedExample
      i7 : installNumPyMethods();

      i8 : x = toPython matrix {{4, 8}, {16, 32}}

      o8 =  [[ 4  8]
             [16 32]]

      o8 : PythonObject of class numpy.ndarray

      i9 : x >>= 2

      o9 = [[1 2]
            [4 8]]

      o9 : PythonObject of class numpy.ndarray

      i10 : x

      o10 = [[1 2]
             [4 8]]

      o10 : PythonObject of class numpy.ndarray
  SeeAlso
    (symbol >>, PythonObject, PythonObject)
    "Macaulay2Doc::augmented assignment"
    installNumPyMethods
///

doc ///
  Key
    (symbol &, PythonObject, PythonObject)
    (symbol &, PythonObject, Thing)
    (symbol &, Thing, PythonObject)
  Headline
    bitwise conjunction of Python objects
  Usage
    x & y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- bitwise conjunction of x and y
  Description
    Text
      Perform the bitwise conjunction ("and") operation on Python objects.
    Example
      toPython 6 & toPython 3
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object.
    Example
      toPython 14 & 7
      14 & toPython 7
  SeeAlso
    (symbol |, PythonObject, PythonObject)
    (symbol ^^, PythonObject, PythonObject)
    (symbol ~, PythonObject)
    (symbol and, PythonObject, PythonObject)
    (symbol &=, PythonObject)
  Subnodes
    (symbol &=, PythonObject)
///

doc ///
  Key
    (symbol &=, PythonObject)
  Headline
    augmented bitwise conjunction of Python objects
  Usage
    x &= y
  Inputs
    x:PythonObject
    y:Thing
  Outputs
    :PythonObject -- bitwise conjunction of x and y, assigned to x
  Description
    Text
      Perform the bitwise conjunction ("and") operation on Python objects, and
      assign the result to the first argument.
    Example
      x = toPython 6
      x &= toPython 3
      x
    Text
      If the right-hand side is a Macaulay2 object, then it is first converted
      to a Python object.
    Example
      x = toPython 14
      x &= 7
      x
    Text
      If the Python class of @VAR "x"@ defines an @CODE "__iand__"@ method
      for in-place and, then it will be called.  Otherwise,
      @VAR "x"@ will and'ed with @VAR "y"@ in the usual way,
      creating a new Python object that is assigned back to @VAR "x"@.

      For example, NumPy arrays support in-place and.  In the example
      below, @VAR "x"@ is modified  directly, and no new object is created.
    CannedExample
      i7 : installNumPyMethods();

      i8 : x = toPython matrix {{1, 2}, {3, 4}}

      o8 = [[1 2]
            [3 4]]

      o8 : PythonObject of class numpy.ndarray

      i9 : x &= 2

      o9 = [[0 2]
            [2 0]]

      o9 : PythonObject of class numpy.ndarray

      i10 : x

      o10 = [[0 2]
             [2 0]]

      o10 : PythonObject of class numpy.ndarray
  SeeAlso
    (symbol &, PythonObject, PythonObject)
    "Macaulay2Doc::augmented assignment"
    installNumPyMethods
///

doc ///
  Key
    (symbol |, PythonObject, PythonObject)
    (symbol |, PythonObject, Thing)
    (symbol |, Thing, PythonObject)
  Headline
    bitwise disjunction of Python objects
  Usage
    x | y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- bitwise disjunction of x and y
  Description
    Text
      Perform the bitwise disjunction ("or") operation on Python objects.
    Example
      toPython 6 or toPython 3
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object.
    Example
      toPython 14 | 7
      14 | toPython 7
  SeeAlso
    (symbol &, PythonObject, PythonObject)
    (symbol ^^, PythonObject, PythonObject)
    (symbol ~, PythonObject)
    (symbol or, PythonObject, PythonObject)
    (symbol |=, PythonObject)
  Subnodes
    (symbol |=, PythonObject)
///

doc ///
  Key
    (symbol |=, PythonObject)
  Headline
    augmented bitwise disjunction of Python objects
  Usage
    x |= y
  Inputs
    x:PythonObject
    y:Thing
  Outputs
    :PythonObject -- bitwise disjunction of x and y, assigned to x
  Description
    Text
      Perform the bitwise disjunction ("or") operation on Python objects, and
      assign the result to the first argument.
    Example
      x = toPython 6
      x |= toPython 3
      x
    Text
      If the right-hand side is a Macaulay2 object, then it is first converted
      to a Python object.
    Example
      x = toPython 14
      x |= 7
      x
    Text
      If the Python class of @VAR "x"@ defines an @CODE "__ior__"@ method
      for in-place or, then it will be called.  Otherwise,
      @VAR "x"@ will or'ed with @VAR "y"@ in the usual way,
      creating a new Python object that is assigned back to @VAR "x"@.

      For example, NumPy arrays support in-place or.  In the example
      below, @VAR "x"@ is modified  directly, and no new object is created.
    CannedExample
      i7 : installNumPyMethods();

      i8 : x = toPython matrix {{1, 2}, {3, 4}}

      o8 = [[1 2]
            [3 4]]

      o8 : PythonObject of class numpy.ndarray

      i9 : x |= 2

      o9 = [[3 2]
            [3 6]]

      o9 : PythonObject of class numpy.ndarray

      i10 : x

      o10 = [[3 2]
             [3 6]]

      o10 : PythonObject of class numpy.ndarray
  SeeAlso
    (symbol |, PythonObject, PythonObject)
    "Macaulay2Doc::augmented assignment"
    installNumPyMethods
///

doc ///
  Key
    (symbol ^^, PythonObject, PythonObject)
    (symbol ^^, PythonObject, Thing)
    (symbol ^^, Thing, PythonObject)
  Headline
    bitwise exclusive disjunction of Python objects
  Usage
    x ^^ y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- bitwise exclusive disjunction of x and y
  Description
    Text
      Perform the bitwise exclusive disjunction ("xor") operation on Python
      objects.
    Example
      toPython 6 ^^ toPython 3
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object.
    Example
      toPython 14 ^^ 7
      14 ^^ toPython 7
  Caveat
    In Python, the @CODE "^"@ operator corresponds to the bitwise xor operation.
    We use @CODE "^^"@ in this case for consistency with the rest of Macaulay2.
    The @CODE "^"@ operator is used for exponentiation.
  SeeAlso
    (symbol &, PythonObject, PythonObject)
    (symbol |, PythonObject, PythonObject)
    (symbol ~, PythonObject)
    (symbol xor, PythonObject, PythonObject)
    (symbol ^, PythonObject, PythonObject)
    (symbol ^^=, PythonObject)
  Subnodes
    (symbol ^^=, PythonObject)
///

doc ///
  Key
    (symbol ^^=, PythonObject)
  Headline
    augmented bitwise exclusive disjunction of Python objects
  Usage
    x ^^= y
  Inputs
    x:PythonObject
    y:Thing
  Outputs
    :PythonObject -- bitwise exclusive disjunction of x and y, assigned to x
  Description
    Text
      Perform the bitwise exclusive disjunction ("xor") operation on Python
      objects, and assign the result to the first argument.
    Example
      x = toPython 6
      x ^^= toPython 3
      x
    Text
      If the right-hand side is a Macaulay2 object, then it is first converted
      to a Python object.
    Example
      x = toPython 14
      x ^^= 7
      x
    Text
      If the Python class of @VAR "x"@ defines an @CODE "__ixor__"@ method
      for in-place xor, then it will be called.  Otherwise,
      @VAR "x"@ will xor'ed with @VAR "y"@ in the usual way,
      creating a new Python object that is assigned back to @VAR "x"@.

      For example, NumPy arrays support in-place xor.  In the example
      below, @VAR "x"@ is modified  directly, and no new object is created.
    CannedExample
      i7 : installNumPyMethods();

      i8 : x = toPython matrix {{1, 2}, {3, 4}}

      o8 = [[1 2]
            [3 4]]

      o8 : PythonObject of class numpy.ndarray

      i9 : x ^^= 2

      o9 = [[3 0]
            [1 6]]

      o9 : PythonObject of class numpy.ndarray

      i10 : x

      o10 = [[3 0]
             [1 6]]

      o10 : PythonObject of class numpy.ndarray
  SeeAlso
    (symbol ^^, PythonObject, PythonObject)
    "Macaulay2Doc::augmented assignment"
    installNumPyMethods
///

doc ///
  Key
    (symbol ~, PythonObject)
  Headline
    bitwise negation of a python object
  Usage
    x~
  Inputs
    x:PythonObject
  Outputs
    :PythonObject -- the bitwise negation of x
  Description
    Text
      This operation negates each bit.  For integers, this is equivalent to
      CODE "-x - 1".  Unlike Python, @CODE "~"@ is a postfix unary operator
      in Macaulay2.
    Example
      (toPython 5)~
  SeeAlso
    (symbol &, PythonObject, PythonObject)
    (symbol |, PythonObject, PythonObject)
    (symbol ^^, PythonObject, PythonObject)
    (symbol not, PythonObject)
///
