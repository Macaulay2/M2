doc ///
  Key
    RInterface
  Headline
    interface to R for statistical computing
  Description
    Text
      This package allows for the ability to interface with
      @HREF("https://www.r-project.org/", "R")@ for statistical computing
      inside Macaulay2.
    Example
      dataFrame = RFunction "data.frame"
      chisqTest = RFunction "chisq.test"
      M = dataFrame(F => {762, 327, 468}, M => {484, 239, 477},
	  "row.names" => {"Democrat", "Independent", "Republican"})
      chisqTest M
      value oo_"p.value"
  Subnodes
    :classes
    RObject
    RFunction
    RContext
    :functions
    RSymbol
    RQuote
    RValue
///

load "./doc/arithmetic.m2"
load "./doc/bitwise.m2"
load "./doc/complex.m2"
load "./doc/context.m2"
load "./doc/exp-log.m2"
load "./doc/extract.m2"
load "./doc/formulas.m2"
load "./doc/functions.m2"
load "./doc/hyperbolic.m2"
load "./doc/iteration.m2"
load "./doc/logical.m2"
load "./doc/objects.m2"
load "./doc/r-value.m2"
load "./doc/relational.m2"
load "./doc/rounding.m2"
load "./doc/special.m2"
load "./doc/stats.m2"
load "./doc/symbols.m2"
load "./doc/trig.m2"
load "./doc/value.m2"
