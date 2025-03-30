beginDocumentation()

--------------------
-- top-level-node --
--------------------

doc ///
  Key
    Python
  Headline
    interface to Python
  Description
    Text
      This package provides a basic interface to run Python code within
      Macaulay2 and to convert back and forth between Python and Macaulay2
      objects.
    Example
      toPython {1, 2/3, "foo", (1, 2, 3), hashTable {"foo" => "bar"}}
      value pythonValue "[1, 2/3, 'foo', (1, 2, 3), {'foo' : 'bar'}]"
      math = import "math"
      math@@sqrt 2
  Subnodes
    :tutorials
    "Python tutorial: creating a virtual environment and installing NumPy"
    "Python tutorial: matplotlib"
    :classes
    PythonObject
///

---------------
-- tutorials --
---------------

doc ///
  Key
    "Python tutorial: creating a virtual environment and installing NumPy"
  Description
    Text
      In this tutorial, we demonstrate how to create a Python virtual
      environment and install the @HREF("https://numpy.org", "numpy")@
      module within it. A virtual environment allows us to isolate
      Python packages from the system-wide installation, ensuring that
      dependencies do not interfere with other projects.

      @HEADER2 "Step 1: Create a Virtual Environment"@

      First, we choose a directory where we want to create our virtual
      environment. We then call @TO setupVirtualEnvironment@ with that
      directory. This command initializes a new virtual environment,
      which includes a dedicated Python interpreter and an isolated
      package installation directory.
    CannedExample
      i2 : dir = temporaryFileName()

      o2 = /tmp/M2-2158442-1/0

      i3 : setupVirtualEnvironment dir
    Text
      @HEADER2 "Step 2: Reload the Python Package with the Virtual Environment"@

      To ensure that the Python interface uses the newly created
      virtual environment, we reload the Python package while
      specifying the virtual environment’s Python interpreter. This is
      done by setting the "executable" configuration option to point
      to the @CODE "python3"@ binary inside our virtual environment.
    CannedExample
      i4 : loadPackage("Python", Reload => true,
               Configuration => {"executable" => dir | "/bin/python3"})

      o4 = Python

      o4 : Package
    Text
      At this point, all Python commands will use the virtual
      environment's interpreter rather than the system-wide Python
      installation.

      @HEADER2 "Step 3: Install NumPy"@

      Next, we install the NumPy module using @TO pipInstall@. This
      downloads and installs NumPy into the virtual environment.
    CannedExample
      i5 : pipInstall "numpy"
      Collecting numpy
        Downloading numpy-2.2.4-cp312-cp312-manylinux_2_17_x86_64.manylinux2014_x86_64.whl.metadata (62 kB)
           ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 62.0/62.0 kB 2.5 MB/s eta 0:00:00

      Downloading numpy-2.2.4-cp312-cp312-manylinux_2_17_x86_64.manylinux2014_x86_64.whl (16.1 MB)
         ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 16.1/16.1 MB 47.9 MB/s eta 0:00:00

      Installing collected packages: numpy
      Successfully installed numpy-2.2.4
    Text
      Once this step completes, NumPy is fully installed and ready for use.

      @HEADER2 "Step 4: Import and Use NumPy"@

      Now that NumPy is installed, we can import it and perform
      numerical computations.
    CannedExample
      i6 : np = import "numpy"

      o6 = <module 'numpy' from '/tmp/M2-2158442-1/0/lib/python3.12/site-packages/
           numpy/__init__.py'>

      o6 : PythonObject of class module
    Text
      To verify that NumPy is working correctly, we perform matrix multiplication using NumPy arrays:
    CannedExample
      i7 : np@@array {{1, 2}, {3, 4}} @ np@@array {{5, 6}, {7, 8}}

      o7 = [[19 22]
            [43 50]]

      o7 : PythonObject of class numpy.ndarray
///

doc ///
  Key
    "Python tutorial: matplotlib"
  Description
    Text
      In this tutorial, we use @HREF{"https://matplotlib.org/", "matplotlib"}@
      to plot the twisted cubic.

      First, we import the necessary modules using @TO import@.  Note that
      we can essentially replace the Python @CODE "import foo as bar"@ with
      @CODE "bar = import \"foo\""@.
    CannedExample
      i2 : plt = import "matplotlib.pyplot"

      o2 = <module 'matplotlib.pyplot' from
           '/usr/lib/python3/dist-packages/matplotlib/pyplot.py'>

      o2 : PythonObject of class module

      i3 : np = import "numpy"

      o3 = <module 'numpy' from '/usr/lib/python3/dist-packages/numpy/__init__.py'>

      o3 : PythonObject of class module
    Text
      Next, we begin to create the various Python objects needed for our
      plot.  This example is heavily inspired by the
      @HREF{"https://matplotlib.org/stable/gallery/mplot3d/lines3d.html",
	  "Parametric curve"}@ example in the matplotlib documentation.

      Note that we basically replace the Python @CODE "foo.bar"@ with
      @CODE "foo\x40\x40bar"@
      (see @TO (symbol \@\@, PythonObject, Thing)@).
      -- TODO: rework this -- we don't need so many quotes
      We need to be careful for attributes that include underscores.
      They must given as strings, i.e., delimited using quotes.  This
      is also the case for keyword arguments.
    CannedExample
      i4 : fig = plt@@figure()

      o4 = Figure(640x480)

      o4 : PythonObject of class matplotlib.figure.Figure

      i5 : ax = fig@@"add_subplot"("projection" => "3d")

      o5 = Axes3DSubplot(0.125,0.11;0.775x0.77)

      o5 : PythonObject of class matplotlib.axes._subplots.Axes3DSubplot

      i6 : t = np@@linspace(-10, 10, 100)

      o6 = [-10.          -9.7979798   -9.5959596   -9.39393939  -9.19191919
             -8.98989899  -8.78787879  -8.58585859  -8.38383838  -8.18181818
             -7.97979798  -7.77777778  -7.57575758  -7.37373737  -7.17171717
             -6.96969697  -6.76767677  -6.56565657  -6.36363636  -6.16161616
             -5.95959596  -5.75757576  -5.55555556  -5.35353535  -5.15151515
             -4.94949495  -4.74747475  -4.54545455  -4.34343434  -4.14141414
             -3.93939394  -3.73737374  -3.53535354  -3.33333333  -3.13131313
             -2.92929293  -2.72727273  -2.52525253  -2.32323232  -2.12121212
             -1.91919192  -1.71717172  -1.51515152  -1.31313131  -1.11111111
             -0.90909091  -0.70707071  -0.50505051  -0.3030303   -0.1010101
              0.1010101    0.3030303    0.50505051   0.70707071   0.90909091
              1.11111111   1.31313131   1.51515152   1.71717172   1.91919192
              2.12121212   2.32323232   2.52525253   2.72727273   2.92929293
              3.13131313   3.33333333   3.53535354   3.73737374   3.93939394
              4.14141414   4.34343434   4.54545455   4.74747475   4.94949495
              5.15151515   5.35353535   5.55555556   5.75757576   5.95959596
              6.16161616   6.36363636   6.56565657   6.76767677   6.96969697
              7.17171717   7.37373737   7.57575758   7.77777778   7.97979798
              8.18181818   8.38383838   8.58585859   8.78787879   8.98989899
              9.19191919   9.39393939   9.5959596    9.7979798   10.        ]

      o6 : PythonObject of class numpy.ndarray
    Text
      Now we construct the twisted cubic.  Note that even though Python itself
      uses @CODE "**"@ for exponentiation, we use @TO symbol ^@ for consistency
      with the rest of Macaulay2.
    CannedExample
      i7 : ax@@plot(t, t^2, t^3)

      o7 = [<mpl_toolkits.mplot3d.art3d.Line3D object at 0x78bdf578a000>]

      o7 : PythonObject of class list
    Text
      Finally, we show our plot.  It will appear in a separate window.
    CannedExample
      i8 : plt@@show()

      o8 = None

      o8 : PythonObject of class NoneType
    Text
      @IMG("src" => Python#"auxiliary files" | "twisted-cubic.png",
	  "alt" => "parametric plot of the twisted cubic")@
///

-- TODO: more tutorials!

------------------
-- PythonObject --
------------------

doc ///
  Key
    PythonObject
  Headline
    a Python object
  Description
    Text
      In Macaulay2, all Python objects have the class @CODE "PythonObject"@.
      Their Python classes are displayed after they are printed.
    Example
      toPython 3
      toPython "foo"
  Subnodes
    :arithmetic operators
    (symbol +,  PythonObject, PythonObject)
    (symbol -,  PythonObject, PythonObject)
    (symbol *,  PythonObject, PythonObject)
    (symbol @,  PythonObject, PythonObject)
    (symbol /,  PythonObject, PythonObject)
    (symbol //, PythonObject, PythonObject)
    (symbol %,  PythonObject, PythonObject)
    (symbol ^,  PythonObject, PythonObject)
    :bitwise operators
    (symbol <<, PythonObject, PythonObject)
    (symbol >>, PythonObject, PythonObject)
    (symbol &,  PythonObject, PythonObject)
    (symbol |,  PythonObject, PythonObject)
    (symbol ^^, PythonObject, PythonObject)
    (symbol ~,  PythonObject)
    :logical operators
    (symbol and, PythonObject, PythonObject)
    (symbol or,  PythonObject, PythonObject)
    (symbol xor, PythonObject, PythonObject)
    (symbol not, PythonObject)
    :comparison operators
    (symbol ?, PythonObject, PythonObject)
    (symbol ==, PythonObject, PythonObject)
///

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
      Multiply two Python objects as matrices.
    CannedExample
      i2 : np = import "numpy";

      i3 : a = np@@array {{1, 0}, {0, 1}}

      o3 = [[1 0]
            [0 1]]

      o3 : PythonObject of class numpy.ndarray

      i4 : b = np@@array {{4, 1}, {2, 2}}

      o4 = [[4 1]
            [2 2]]

      o4 : PythonObject of class numpy.ndarray

      i5 : a @ b

      o5 = [[4 1]
            [2 2]]

      o5 : PythonObject of class numpy.ndarray
    Text
      In Macaulay2, the @CODE "*"@ operator is used for matrix multiplication,
      but in Python, this results in componentwise multiplication.
    CannedExample
      i6 : a * b

      o6 = [[4 0]
            [0 2]]

      o6 : PythonObject of class numpy.ndarray
  SeeAlso
    (symbol *, PythonObject, PythonObject)
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
///

-- TODO: document the different behavior between //, %, and quotientRemainder
-- for Python v. Macaulay2
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
  SeeAlso
    (symbol /, PythonObject, PythonObject)
    (symbol %, PythonObject, PythonObject)
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
  SeeAlso
    (symbol //, PythonObject, PythonObject)
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
///

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
      toPython 6 | toPython 3
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

-----------------------
-- logical operators --
-----------------------

doc ///
  Key
    (symbol and, PythonObject, PythonObject)
    (symbol and, PythonObject, Thing)
    (symbol and, Thing, PythonObject)
  Headline
    logical conjunction of Python objects
  Usage
    x and y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- logical conjunction of x and y
  Description
    Text
      Perform the logical conjunction ("and") operation on Python objects.
    Example
      toPython true and toPython true
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object.
    Example
      toPython true and false
      false and toPython true
    Text
      Unlike Macaulay2, Python supports using @CODE "and"@ with non-boolean
      objects.  In this case, the first argument is returned if it is "falsy".
      Otherwise, the second argument is returned.
    Example
      toPython 0 and toPython "foo"
      toPython 5 and toPython "bar"
    Text
      In Python, when the first argument is falsy, then is is returned
      immediately without evaluating the second argument.  This is known as
      "short-circuiting".  However, in Macaulay2, both arguments are evaluated
      before the Python conjunction method is called.
    Example
      stopIfError = false
      toPython 0 and 1/0
    Text
      However, if the first argument is the Macaulay2 @TO false@ object, then
      short-cicuiting will occur.
    Example
     false and pythonValue "1/0"
  SeeAlso
    (symbol or, PythonObject, PythonObject)
    (symbol xor, PythonObject, PythonObject)
    (symbol not, PythonObject)
    (symbol &, PythonObject, PythonObject)
///

doc ///
  Key
    (symbol or, PythonObject, PythonObject)
    (symbol or, PythonObject, Thing)
    (symbol or, Thing, PythonObject)
  Headline
    logical disjunction of Python objects
  Usage
    x or y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- logical disjunction of x and y
  Description
    Text
      Perform the logical disjunction ("or") operation on Python objects.
    Example
      toPython true or toPython true
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object.
    Example
      toPython true or false
      false or toPython true
    Text
      Unlike Macaulay2, Python supports using @CODE "or"@ with non-boolean
      objects.  In this case, the first argument is returned if it is "truthy".
      Otherwise, the second argument is returned.
    Example
      toPython 0 or toPython "foo"
      toPython 5 or toPython "bar"
    Text
      In Python, when the first argument is truthy, then is is returned
      immediately without evaluating the second argument.  This is known as
      "short-circuiting".  However, in Macaulay2, both arguments are evaluated
      before the Python disjunction method is called.
    Example
      stopIfError = false
      toPython 1 or 1/0
    Text
      However, if the first argument is the Macaulay2 @TO true@ object, then
      short-cicuiting will occur.
    Example
     true or pythonValue "1/0"
  SeeAlso
    (symbol or, PythonObject, PythonObject)
    (symbol xor, PythonObject, PythonObject)
    (symbol not, PythonObject)
    (symbol &, PythonObject, PythonObject)
///

doc ///
  Key
    (symbol xor, PythonObject, PythonObject)
    (symbol xor, PythonObject, Thing)
    (symbol xor, Thing, PythonObject)
  Headline
    logical exclusive disjunction of Python objects
  Usage
    x xor y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :PythonObject -- logical exclusive disjunction of x and y
  Description
    Text
      Perform the logical exclusive disjunction ("xor") operation on Python
      objects.
    Example
      toPython true xor toPython true
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object.
    Example
      toPython true xor false
      false xor toPython true
    Text
      Unlike Macaulay2, Python supports using @CODE "xor"@ with non-boolean
      objects. If exactly one of the arguments is truthy, then it will return
      that argument.  Otherwise, it will return @CODE "False"@.
    Example
      toPython 0 xor toPython "foo"
      toPython 5 xor toPython ""
      toPython 6 xor toPython "bar"
  SeeAlso
    (symbol or, PythonObject, PythonObject)
    (symbol and, PythonObject, PythonObject)
    (symbol not, PythonObject)
    (symbol ^^, PythonObject, PythonObject)
///

doc ///
  Key
    (symbol not, PythonObject)
  Headline
    logical negation of a python object
  Usage
    not x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject -- the logical negation of x
  Description
    Text
      Perform the logical negation ("not") operation on a Python object.
    Example
      not toPython true
      not toPython false
    Text
      Unlike Macaulay2, Python supports using @CODE "not"@ with non-boolean
      objects. It will return @CODE "False"@ if the argument is truthy and
      @CODE "True"@ if it is falsy.
    Example
      not toPython 0
      not toPython "foo"
  SeeAlso
    (symbol or, PythonObject, PythonObject)
    (symbol and, PythonObject, PythonObject)
    (symbol xor, PythonObject, PythonObject)
    (symbol ~, PythonObject)
///

--------------------------
-- comparison operators --
--------------------------

doc ///
  Key
    (symbol ?, PythonObject, PythonObject)
    (symbol ?, PythonObject, Thing)
    (symbol ?, Thing, PythonObject)
  Headline
    compare Python objects
  Usage
    x ? y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :Symbol -- <, >, ==, or @TO incomparable@
  Description
    Text
      Compare two Python objects.  The result is one of the following symbols:
      @TO symbol <@, @TO symbol >@, @TO symbol ==@, or @TO incomparable@.
    Example
      toPython 2 ? toPython 3
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object before comparing.
    Example
      toPython 4 ? 4
      7 ? toPython 6
    Text
      This method is used by the comparison operators @TO symbol <@,
      @TO symbol >@, @TO symbol <=@, and @TO symbol >=@.
    Example
      toPython 2 < toPython 3
      4 >= toPython 5
  SeeAlso
    (symbol ==, PythonObject, PythonObject)
///

doc ///
  Key
    (symbol ==, PythonObject, PythonObject)
    (symbol ==, PythonObject, Thing)
    (symbol ==, Thing, PythonObject)
  Headline
    equality of Python objects
  Usage
    x == y
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :Boolean -- whether x and y are equal
  Description
    Text
      Determine whether two Python objects are equal.
    Example
      toPython 2 == toPython 3
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object before comparing.
    Example
      toPython 4 == 4
      7 == toPython 6
  SeeAlso
    (symbol ?, PythonObject, PythonObject)
///

doc ///
  Key
    (symbol _, PythonObject, Thing)
  Headline
    get elements of python sequences
  Usage
    x_y
  Inputs
    x:PythonObject
    y:Thing
  Outputs
   :PythonObject
  Description
    Text
      You may access elements of python sequences using @TT "_"@.
      This is equivalent to square brackets (@TT "[]"@) in Python. For
      example, this works for lists.
    Example
      x = pythonValue "[1,2,3,4]"
      x_1
    Text
      It also works for dictionaries.
    Example
      x = pythonValue "{'spam':1,'eggs':2}"
      x_"eggs"
///

doc ///
  Key
    ((symbol _, symbol =), PythonObject, Thing)
  Headline
    set elements of mutable python sequences
  Usage
    x_y = e
  Inputs
    x:PythonObject
    y:Thing
    e:Thing
  Description
    Text
      You may set elements of mutable python sequences using @TT "_"@.
      This is equivalent to square brackets (@TT "[]"@) in Python. For
      example, this works for lists.
    Example
      x = toPython {1, 2, 3, 4}
      x_0 = 5
      x
    Text
      It also works for dictionaries.
    Example
      x = toPython hashTable {"spam" => 1, "eggs" => 2}
      x_"ham" = 3
      x
///

doc ///
  Key
    pythonValue
    (pythonValue, String)
    (pythonValue, Sequence)
  Headline
    execute Python source code from a string
  Usage
    pythonValue s
  Inputs
    s:{String, Sequence} -- containing Python source code
  Outputs
    :PythonObject -- the return value of the given code
  Description
    Text
      This function a is wrapper around the function @TT
      HREF{"https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_String",
      "PyRun_String"}@ from the Python C API.
    Example
      pythonValue "2 + 2"
    Text
      If a sequence is given, then its elements are converted to strings using
      @TO "toString"@ and then joined using @TO "concatenate"@.  You can see the
       expression sent to the Python interpreter by setting @TO "debugLevel"@
       to a positive value.
    Example
      debugLevel = 1
      x = 5
      pythonValue("3 + ", x)
  SeeAlso
    runSimpleString
///

doc ///
  Key
    runSimpleString
  Headline
    execute Python source code from a string in __main__
  Usage
    runSimpleString s
  Inputs
    s:String -- containing Python source code
  Description
    Text
      This function a is wrapper around the function @TT
      HREF{"https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_SimpleString",
      "PyRun_SimpleString"}@ from the Python C API.  Note that, unlike
      @TO "pythonValue"@, it has no return value.
    Example
      runSimpleString "print('Hello, world!')" -* no-capture-flag *-
  SeeAlso
    pythonValue
///

doc ///
  Key
    (iterator, PythonObject)
  Headline
    get iterator of iterable python object
  Usage
    i = iterator x
  Inputs
    x:PythonObject -- an iterable
  Outputs
    i:PythonObject -- an iterator
  Description
    Text
      This function works just like its
      @HREF{"https://docs.python.org/3/library/functions.html#iter",
      "Python counterpart"}@.  In particular, @TT "i"@ is an iterator
      for the iterable object @TT "x"@.
    Example
      builtins = import "builtins"
      x = builtins@@range 3
      i = iterator x
  SeeAlso
    (next, PythonObject)
///

doc ///
  Key
    (next, PythonObject)
  Headline
    retrieve the next item from a python iterator
  Usage
    next i
  Inputs
    i:PythonObject -- an iterator
  Description
    Text
      This function works just like its
      @HREF{"https://docs.python.org/3/library/functions.html#next",
      "Python counterpart"}@.  In particular, it retrieves the next item
      from an iterator.
    Example
      builtins = import "builtins"
      x = builtins@@range 3
      i = iterator x
      next i
      next i
      next i
    Text
      When the iterator is exhausted, @TO "StopIteration"@ is returned.
    Example
      next i
  SeeAlso
    (iterator, PythonObject)
///

doc ///
  Key
    toFunction
    (toFunction,PythonObject)
    (symbol SPACE, PythonObject, Thing)
  Headline
    convert callable python objects to Macaulay2 functions
  Usage
    toFunction x
  Inputs
    x:PythonObject
  Outputs
    :FunctionClosure
  Description
    Text
      This function will convert a Python object into a Macaulay2 function.
    Example
      math = import "math"
      pysqrt = toFunction math@@sqrt
      pysqrt 2
    Text
      Optional arguments can be provided using options.
    Example
      builtins = import "builtins"
      int = toFunction builtins@@int
      int("deadbeef", "base" => 16)
    Text
      If a python object and a Macaulay2 thing are separated by a space, then
      @TT "toFunction"@ will be called on the python object and then resulting
      function will be called with the Macaulay2 object as its argument.
    Example
      math@@cos pi
///

doc ///
  Key
    (abs, PythonObject)
  Headline
    absolute value of a python object
  Usage
    abs x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject -- the absolute value of @TT "x"@
  Description
    Text
      This is equivalent to the Python @HREF {
      "https://docs.python.org/3/library/functions.html#abs", "abs"}@ function.
    Example
      abs toPython(-12)
///

doc ///
  Key
    (length,PythonObject)
  Headline
    returns the length of a python object
  Usage
    length x
  Inputs
    x:PythonObject
  Outputs
    :ZZ
  Description
    Text
      This is equivalent the Python @HREF {
      "https://docs.python.org/3/library/functions.html#len", "len"}@ function.
    Example
      length toPython "Hello, world!"
      length pythonValue "[1,2,3,4,5]"
///

doc ///
  Key
    (value,PythonObject)
  Headline
    convert python objects to Macaulay2 things
  Usage
    value x
  Inputs
    x:PythonObject
  Outputs
    :Thing -- the Macaulay2 equivalent of @TT "x"@
  Description
    Text
      This function attempts to convert @TT "x"@ to its corresponding
      Macaulay2 equivalent.
    Example
      value pythonValue "[1, 3.14159, 'foo', (1,2,3), {'foo':'bar'}]"
      class \ oo
    Text
      Since the type of @TT "x"@ is not initially known, a sequence of
      @TO2 {"using hooks", "hooks"}@ are used to determine its type
      and then convert it.
    Example
      hooks value
    Text
      If no conversion can be done, then @TT "x"@ is returned.
--    Example
--      TODO
    Text
      Users may add additional hooks using @TO "addHook"@ or the
      convenience function @TO "addPyToM2Function"@.
///

doc ///
  Key
    addPyToM2Function
    (addPyToM2Function, String, Function, String)
    (addPyToM2Function, List, Function, String)
  Headline
    convenience function for adding value hooks
  Usage
    addPyToM2Function(type, f, desc)
  Inputs
    type:{String,List} -- the type(s) to convert
    f:Function -- the function that will do the converting
    desc:String -- passed to the @TT "Strategy"@ option of @TO "addHook"@
  Description
    Text
      Most of the hooks used by @TO "value"@ have the same general format:
      if the python object has a particular type, then use a particular
      function to convert it to a corresponding Macaulay2 thing.  This function
      simplifies the process of adding such a hook.
    Text
      For example, suppose we would like to convert @TT "Fraction"@
      objects from the Python @HREF
      {"https://docs.python.org/3/library/fractions.html",
      "fractions"}@ module to @TO "QQ"@ objects.  Without adding a hook,
      @TO "value"@ will do nothing with these objects.
    Example
      fractions = import "fractions"
      x = fractions@@"Fraction"(2, 3)
      value x
    Text
      So we write a function to do the conversion and then install the hook
      using @TT "addPyToM2Function"@.
    Example
      toQQ = x -> value x@@"numerator" / value x@@"denominator";
      addPyToM2Function("Fraction", toQQ, "Fraction -> QQ");
      value x
      hooks value
///

doc ///
  Key
    toPython
    (toPython, Boolean)
    (toPython, CC)
    (toPython, Function)
    (toPython, HashTable)
    (toPython, Nothing)
    (toPython, Number)
    (toPython, PythonObject)
    (toPython, RR)
    (toPython, Sequence)
    (toPython, Set)
    (toPython, String)
    (toPython, VisibleList)
    (toPython, ZZ)
  Headline
    convert Macaulay2 things to Python objects
  Usage
    toPython x
  Inputs
    x:Thing
  Outputs
    :PythonObject
  Description
    Text
      Attempt to convert a Macaulay2 thing to a Python object.
    Example
      toPython 2
      toPython (1/2)
      toPython pi
      toPython ii
      toPython "foo"
      toPython {1, 2, 3, 4}
      toPython (1, 2, 3, 4)
      toPython hashTable {"foo" => "bar"}
      toPython set {1, 2, 3, 4}
      toPython true
      toPython null
      m2sqrt = x -> (
	  print "calling Macaulay2 code from Python!";
	  sqrt x)
      pysqrt = toPython m2sqrt
      pysqrt 2
///

doc ///
  Key
    import
    (import, String)
  Headline
    import a Python module
  Usage
    import s
  Inputs
    s:String -- the name of a python module
  Outputs
    :PythonObject -- the imported module
  Description
    Text
      This is a wrapper around the Python C API function @HREF{
      "https://docs.python.org/3/c-api/import.html#c.PyImport_ImportModule",
      "PyImport_ImportModule"}@ and returns an imported Python module.
    Text
      Once imported, the statements and definitions from the module are
      available using @TO (symbol \@\@, PythonObject, Thing)@.
    Example
      math = import "math"
      math@@pi
      math@@sqrt 2
///

doc ///
  Key
    (symbol @@, PythonObject, Thing)
  Headline
    get an attribute of a python object
  Usage
    x@@y
  Inputs
    x:PythonObject
    y:Thing
  Outputs
    :PythonObject
  Description
    Text
      Get an attribute of a Python object.  In Python, the @CODE "."@
      operator is used.  However, this operator has a special meaning
      in Macaulay2 and cannot be overloaded.  Since @CODE "\@\@"@ has
      similar precedence, it is used instead.
    Example
      foo = toPython "Hello, world!"
      foo@@upper()
    Text
      Note that @TO toString@ is called on @CODE "y"@ before it is used.
      Therefore, symbols and functions may be used as well as strings.
      Sometimes, however, it is necessary to use quotes, e.g., when using
      special "dunder" methods with double underscores.  These would
      result in a Macaulay2 syntax error if left unquoted.
    Example
      foo@@"__len__"()
///

doc ///
  Key
    (symbol @@?, PythonObject, Thing)
  Headline
    whether a python object has an attribute
  Usage
    x @@? y
  Outputs
    :Boolean -- whether @TT "y"@ is an attribute of @TT "x"@
  Inputs
    x:PythonObject
    y:Thing
  Description
    Text
      This is equivalent to the Python @HREF{
      "https://docs.python.org/3/library/functions.html#hasattr", "hasattr"}@
      function.
    Example
      foo = toPython "Hello, world!"
      foo@@?upper
      foo@@?bar
///

doc ///
  Key
    ((symbol @@, symbol =), PythonObject, Thing)
  Headline
    set an attribute of a python object
  Usage
    x@@y = e
  Inputs
    x:PythonObject
    y:Thing
    e:Thing
  Description
    Text
      This is equivalent to the Python @HREF{
      "https://docs.python.org/3/library/functions.html#setattr", "setattr"}@
      function.  Note that @TT "e"@ is converted to a Python object using
      @TO "toPython"@.

      As with @TO "getattr"@, when using the shortcut @TT "\@\@"@, @TT "y"@
      need not be a string.
    Example
      math = import "math"
      math@@pi = 22/7 -* no-capture-flag *-
      math@@pi
///

doc ///
  Key
    objectType
  Headline
    type of a python object
  Usage
    objectType x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject -- the type of @TT "x"@
  Description
    Text
      This is equivalent to the @HREF{
      "https://docs.python.org/3/library/functions.html#type", "type"}@ function
      in Python.
    Example
      objectType toPython 2
      objectType toPython "Hello, world!"
///

doc ///
  Key
    (isMember, PythonObject, PythonObject)
    (isMember, PythonObject, Thing)
    (isMember, Thing, PythonObject)
  Headline
    test membership in a python object
  Usage
    isMember(x, y)
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :Boolean -- whether x is in y
  Description
    Text
      This tests whether @CODE "x"@ is a member of @CODE "y"@.  This is
      equivalent to @CODE "x in y"@ in Python.
    Example
      isMember(toPython 3, toPython {1, 2, 3})
      isMember(toPython 4, toPython {1, 2, 3})
    Text
      If one of the arguments is a Macaulay2 object, then it is first converted
      to a Python object before testing for membership.
    Example
      isMember(toPython 3, {1, 2, 3})
      isMember(4, toPython {1, 2, 3})
    Text
      When used with dictionaries, which are Python's equivalent of Macaulay2
      hash tables, the keys are tested for membership.  In this sense, it is
      like the Macaulay2 @TO symbol #?@ and @TO symbol .?@ operators.
    Example
      isMember("foo", toPython hashTable {"foo" => "bar"})
      isMember("bar", toPython hashTable {"foo" => "bar"})
///

doc ///
  Key
    (quotientRemainder, PythonObject, PythonObject)
    (quotientRemainder, PythonObject, Thing)
    (quotientRemainder, Thing, PythonObject)
  Headline
    quotient and remainder of python objects
  Usage
    quotientRemainder(x, y)
  Inputs
    x:PythonObject
    y:PythonObject
  Outputs
    :Sequence -- a pair of two python objects
  Description
    Text
      The quotient and remainder when @TT "x"@ is divided by @TT "y"@.  This
      calls Python's built-in @TT "divmod"@ function.
    Example
      quotientRemainder(toPython 37, toPython 5)
      class \ oo
    Text
      If just one of the arguments is a python object, then the other is
      converted to a python object using @TO "toPython"@.
    Example
      quotientRemainder(toPython 37, 5)
      class \ oo
///

doc ///
  Key
    (round, ZZ, PythonObject)
    (round, PythonObject, PythonObject)
    (round, PythonObject)
  Headline
    round a python object
  Usage
    round(n, x)
    round x
  Inputs
    n:ZZ
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @TT "round"@ function, which round @TT "x"@
      to @TT "n"@ decimal places, or to the nearest integer if @TT "n"@ is not
      given.
    Example
      x = (import "math")@@pi
      round x
      round(3, x)
    Text
      Ties are broken by @EM "round half to even"@.
    Example
      round toPython 2.5
      round toPython 3.5
///

doc ///
  Key
    (truncate, PythonObject)
  Headline
    truncate a python object
  Usage
    truncate x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @TT "math.trunc"@ function, which rounds
      toward zero.
    Example
      truncate toPython 5.8
      truncate toPython(-5.8)
///
doc ///
  Key
    (floor, PythonObject)
  Headline
    floor of a python object
  Usage
    floor x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @TT "math.floor"@ function, which rounds
      toward negative infinity.
    Example
      floor toPython 5.8
      floor toPython(-5.8)
///

doc ///
  Key
    (ceiling, PythonObject)
  Headline
    ceiling of a python object
  Usage
    ceiling x
  Inputs
    x:PythonObject
  Outputs
    :PythonObject
  Description
    Text
      This calls Python's built-in @TT "math.ceil"@ function, which rounds
      toward positive infinity.
    Example
      ceiling toPython 5.8
      ceiling toPython(-5.8)
///

doc ///
  Key
    (help#0, PythonObject)
  Headline
    documentation for python object
  Usage
    help x
  Inputs
    x:PythonObject
  Outputs
    :String
  Description
    Text
      This calls Python's built-in @TT "help"@ function, which provides
      documentation for Python objects.
    Example
      math = import "math"
      help math
      help math@@sin
///
