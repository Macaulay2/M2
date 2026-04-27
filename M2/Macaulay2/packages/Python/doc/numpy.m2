-* code for canned example

np = installNumPyMethods()
A = toPython matrix {{1, 2}, {3, 4}}
v = toPython vector {5, 6, 7}
value A
value v

np@@arange(2, 9, 2)
*-

doc ///
  Key
    installNumPyMethods
    (toPython, Matrix)
    (toPython, MutableMatrix)
    (toPython, Vector)
  Headline
    install NumPy methods
  Usage
    np = installNumPyMethods()
  Outputs
    np:PythonObject -- the NumPy module
  Description
    Text
      This function loads the @HREF("https://numpy.org/", "NumPy")@ module and
      installs several @TO toPython@ and @TO (value, PythonObject)@ methods for
      converting back and forth between Macaulay2 matrices/vectors and NumPy
      arrays.
    CannedExample
      i1 : np = installNumPyMethods()

      o1 = <module 'numpy' from '/usr/lib/python3/dist-packages/numpy/__init__.py'>

      o1 : PythonObject of class module

      i2 : A = toPython matrix {{1, 2}, {3, 4}}

      o2 = [[1 2]
            [3 4]]

      o2 : PythonObject of class numpy.ndarray

      i3 : v = toPython vector {5, 6, 7}

      o3 = [5 6 7]

      o3 : PythonObject of class numpy.ndarray

      i4 : value A

      o4 = | 1 2 |
           | 3 4 |

                    2       2
      o4 : Matrix ZZ  <-- ZZ

      i5 : value v

      o5 = | 5 |
           | 6 |
           | 7 |

             3
      o5 : ZZ
    Text
      The output of the call to @CODE "installNumPyMethods"@ is a
      @TO PythonObject@ referring to the module itself and can be used to
      call NumPy functions directly.
    CannedExample
      i6 : np@@arange(2, 9, 2)

      o6 = [2 4 6 8]

      o6 : PythonObject of class numpy.ndarray
///
