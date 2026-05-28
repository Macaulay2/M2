doc ///
  Key
    (symbol ~, RObject, RObject)
    (symbol ~, RObject, Thing)
    (symbol ~, Thing, RObject)
  Headline
    create a two-sided R formula
  Usage
    y ~ x
  Inputs
    y:RObject
    x:RObject
  Outputs
    :RObject -- a two-sided formula @SAMP "y ~ x"@
  Description
    Text
      Create a two-sided R formula using R's @SAMP "~"@ operator.
      Formulas are used throughout R's statistical modeling functions
      to specify relationships between variables.
    Example
      RQuote mpg ~ RQuote wt
    Text
      To create a one-sided formula (right-hand side only), use the
      unary form; see @TO (symbol ~, RObject)@.
  SeeAlso
    (symbol ~, RObject)
///
