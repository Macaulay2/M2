--- status: DRAFT
--- author(s): from before
--- notes: 

undocumented methods texMath

doc ///
  Key
    texMath
  Headline
    convert to TeX math format
  Usage
    texMath x
  Inputs
    x:
      any Macaulay2 object
  Outputs
    :String
      @TT "x"@ converted to TeX math format
  Description
    Text
      The main difference between this and @TO tex@ is that the surrounding
      dollar signs aren't there.
    Example
      R = ZZ[x];
      texMath (x-1)^6
  Caveat
    No attempt is made to wrap large matrices or equations.
  SeeAlso
    tex
    mathML
    showTex
///
