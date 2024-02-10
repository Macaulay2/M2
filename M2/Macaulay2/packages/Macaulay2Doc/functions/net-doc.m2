--- status: TODO
--- author(s): 
--- notes: 

undocumented toList(set methods net - set{(net, Thing), (net, String)})

doc ///
Node
  Key
     net
    (net, Thing)
  Headline
    format for printing, as a net
  Usage
    net o
  Inputs
    x:Thing
  Outputs
    :Net
      obtained by formatting @TT "o"@ for printing
  Description
    Text
      This function is the primary function called upon by @TO "<<"@ to format expressions for printing.
      The default method provided by the system is to convert @TT "o"@ to an @TO "Expression"@ with
      @TO "expression"@ and then to convert that to a @TO Net@.

      A new method for formatting expressions of type @TT "T"@ may be installed as follows:
    Code
      EXAMPLE { PRE ////net T := x -> ...//// }
    Text
      The function provided by the user should return a @TO Net@ or a @TO String@.

      There are various such methods for formatting, but we don't document them separately.
  SeeAlso
    String
    Expression
    AfterPrint
    toString
    toExternalString
    expression

Node
  Key
    (net, String)
  Headline
    convert a string to a net
  Usage
    net s
  Inputs
    s:String
  Outputs
    :Net
      formed by stacking the lines of the string @TT "s"@ vertically
  Description
    Text
      Tab characters are converted to spaces, with a tab stop at every 8-th column.

      In the following examples we can use @TT "peek"@ to see the exact shape of the nets produced.
    Example
      net "line 1\nline 2\nline 3\n"
      peek oo
      net ""
      peek oo
      net "\n"
      peek oo
      net "\n\n\ta"
      peek oo
  SeeAlso
    peek
    stack
    unstack
///
