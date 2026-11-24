--- status: DRAFT
--- author(s): from doc2, MES
--- notes: 

doc ///
  Key
    characters
  Headline
    get characters from a string
  Usage
    characters s
  Inputs
    s:String
  Outputs
    :List
      of the characters in the string @TT "s"@
  Description
    Text
      The characters are represented by strings.
    Example
      characters "asdf"
    Text
      utf8 characters are correctly treated:
    Example
      characters "π ≈ 3.14159"
    Text
      To get the one-byte characters of the string, use @TO "toList"@ instead.
  SeeAlso
    String
///

