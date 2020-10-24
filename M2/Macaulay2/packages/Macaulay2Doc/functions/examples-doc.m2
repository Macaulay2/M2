--- status: DRAFT
--- author(s): M. Stillman
--- notes: 

doc ///
Node
  Key
     capture
    (capture, Net)
    (capture, List)
    (capture, String)
  Headline
    evaluate Macaulay2 code and capture the output (under development)
  SeeAlso
    examples

Node
  Key
     examples
    (examples, Thing)
  Headline
    list the examples in documentation
  Usage
    examples s
  Inputs
    s:Thing
      a descriptor for a documentation node (see below for examples)
  Outputs
    :Net
      containing examples of code provided in the documentation of @TT "s"@
  Description
    Text
      The output is returned as a @TO Net@ of height 0, so the examples will be
      displayed indented by just white space, allowing immediate entry.
      Alternatively, one could use @TO "print"@ to display them with no indentation.
  SeeAlso
    "reading the documentation"
    capture
    help
///

TEST ///
     assert( class examples MutableList === Net )
///
