--- status: 
--- author(s): 
--- notes: 

undocumented methods html

doc ///
Node
  Key
    html
  Headline
    convert to HTML format
  Usage
    html x
  Inputs
    x:Thing
      any Macaulay2 object
  Outputs
    :String
      the HTML rendering of @TT "x"@
  Description
    Text
      @TT "html x"@ converts @TT "x"@ to HTML format.

      The return value is a string that is suitable for use in a .html file readable by a browser.
      @TO "hypertext"@ elements are translated into the corresponding HTML elements. When no HTML
      conversion is available, @TO "tex"@ is called. $\LaTeX$ can be rendered in the browser using
      MathJax or $\KaTeX$.
  SeeAlso
    show
    mathML
    "Text::html(TEX)"
///
