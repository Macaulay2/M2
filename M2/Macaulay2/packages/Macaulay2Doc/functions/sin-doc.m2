--- status: DRAFT
--- author(s): L. Gold
--- notes: Is the comment about being used as an example still relevant?

doc ///
  Key
    sin
    (sin,InexactNumber)
  Headline
    -- this node is used as an example in the documentation node Inputs and Outputs.
    compute the sine
  Usage
    sin x
    sin I
  Inputs
    x:RR
    I:RRi
  Outputs
    :RR
      the sine of @TT "x"@
    :RRi
      an interval containing the sines of the points of @TT "I"@
  Description
    Example
      sin (pi/2)
///
