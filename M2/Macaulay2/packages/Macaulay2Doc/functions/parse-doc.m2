doc ///
  Key
    parse
  Headline
    get a concrete syntax tree of Macaulay2 code
  Usage
    parse s
  Inputs
    s:String -- Macaulay2 code
  Outputs
    :List
  Description
    Text
      This function parses a string @VAR "s"@ containing Macaulay2 code and
      returns a list representing the corresponding concrete syntax tree.
      Each element of the list corresponds a statement.  Except for very simple
      code, they will have a nested structure.  The first element is a tag
      representing the type of node and later elements are its children.

      Consider the following:
    Example
      parse "2 + 3"
    Text
      We see that this is a @CODE "Binary"@ node with three children: the
      left hand side, the operator, and the right hand side.  All three
      are @CODE "Token"@ nodes.

      The next example shows how Macaulay2 deals with the
      @wikipedia "danging else"@ problem:
    Example
      parse "if a then if b then s1 else s2"
  SeeAlso
    pseudocode
    disassemble
///
