--- status: DRAFT
--- author(s): PZJ
--- notes: 

doc ///
 Key
  makeKeyword
  (makeKeyword, String)
  [makeKeyword, Precedence]
  [makeKeyword, Syntax]
 Headline
  create a new keyword
 Usage
  makeKeyword s
 Inputs
  s: String
  Precedence => {ZZ,Symbol}
  Syntax => {List,Symbol}
 Description
  Text
   Creates a new Keyword out of the input string.
   The options specify the parsing behavior of the keyword.
   Possible choices for @TT "Syntax"@ are: @TT "Binary"@, @TT "Prefix"@, @TT "Postfix"@ or @TT "{Binary,Prefix}"@.
   If @TT "Precedence"@ is a @TT "Symbol"@, the precedence is set to the one of that symbol.
  Example
   makeKeyword("≺",Precedence => symbol <)
   ZZ ≺ ZZ := (i,j) -> j==i+1
   3≺4
///
