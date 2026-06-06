--------------------------
-- complex number functions --
--------------------------

doc ///
  Key
    (conjugate, RObject)
  Headline
    complex conjugate of an R object
  Usage
    conjugate z
  Inputs
    z:RObject
  Outputs
    :RObject -- the element-wise complex conjugate of @VAR "z"@
  Description
    Text
      Compute the complex conjugate of each element of an R complex vector,
      calling R's @SAMP "Conj"@.
    Example
      z = RObject(3 + 2*ii)
      conjugate z
  SeeAlso
    (realPart, RObject)
    (imaginaryPart, RObject)
///

doc ///
  Key
    (realPart, RObject)
  Headline
    real part of an R object
  Usage
    realPart z
  Inputs
    z:RObject
  Outputs
    :RObject -- the element-wise real part of @VAR "z"@
  Description
    Text
      Extract the real part of each element of an R complex vector,
      calling R's @SAMP "Re"@.
    Example
      z = RObject(3 + 2*ii)
      realPart z
  SeeAlso
    (imaginaryPart, RObject)
    (conjugate, RObject)
///

doc ///
  Key
    (imaginaryPart, RObject)
  Headline
    imaginary part of an R object
  Usage
    imaginaryPart z
  Inputs
    z:RObject
  Outputs
    :RObject -- the element-wise imaginary part of @VAR "z"@
  Description
    Text
      Extract the imaginary part of each element of an R complex vector,
      calling R's @SAMP "Im"@.
    Example
      z = RObject(3 + 2*ii)
      imaginaryPart z
  SeeAlso
    (realPart, RObject)
    (conjugate, RObject)
///
