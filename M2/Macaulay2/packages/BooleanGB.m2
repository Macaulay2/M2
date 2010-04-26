newPackage(
    "BooleanGB",
    Version => "0.1", 
    Date => "",
    Authors => {{Name => "", 
    Email => "", 
    HomePage => ""}},
    Headline => "",
    DebuggingMode => true
    )

export {}

-- Code here

beginDocumentation()

doc ///
Key
  BooleanGB
Headline
  Compute Groebner Basis in a Boolean Ring
Description
  Text
  Example
Caveat
SeeAlso
///

doc ///
Key
  (gbBoolean, Ideal)
Headline
  Compute Groebner Basis
Usage
  gbBoolean I
Inputs
  I:Ideal
Outputs
  J:Ideal
    the reduced groebner basis of I
Consequences
Description
  Text
  Example
Caveat
SeeAlso
///

TEST ///
-- test code and assertions here
-- may have as many TEST sections as needed
///

