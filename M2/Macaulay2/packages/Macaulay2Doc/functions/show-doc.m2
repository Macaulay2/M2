--- status: Done
--- author(s): MES, Mahrud
--- notes:

doc ///
Node
  Key
    URL
    (NewFromMethod, URL, String)
  Headline
    a type representing a URL
  Usage
    URL str
  Inputs
    str:String
      a URL
  Outputs
    :URL
  Description
    Text
      The function @TO "show"@ knows how to display entities of various types, including URLs.
  SeeAlso
    show

Node
  Key
    show
   (show, URL)
    showHtml
    showTex
  Headline
    display various objects in an external viewer
  Usage
    show x
  Inputs
    x:{Hypertext,TEX,URL}
  Consequences
    Item
      An external viewer, such as a web browser or PDF viewer, is started to view the object @TT "x"@
  Description
    Text
      The functions {\tt showTex} and {\tt showHtml} are specializations of the {\tt show} method
      which first convert {\tt x} to a @TO TEX@ or @TO Hypertext@ object, respectively, and display
      that result.

      For example, the following lines would display a matrix in a browser or PDF viewer:
    CannedExample
      i1 : showTex matrix{{1,2,3},{4,5,6}}
      + cd /tmp/M2-95678-0/0/
      + pdflatex -interaction=batchmode show
      This is pdfTeX, Version 3.14159265-2.6-1.40.21 (TeX Live 2020) (preloaded format=pdflatex)
      restricted \write18 enabled.
      entering extended mode

      i2 : showHtml matrix{{1,2,3},{4,5,6}}
      Opening in existing browser session.
    Text
      By default, the viewer is determined by either @TT "open"@ on macOS or @TT "xdg-open"@ on
      Linux distributions. As backup for when neither @TT "open"@ nor @TT "xdg-open"@ is available,
      the environmental variable @TT "WWWBROWSER"@ or @TT "firefox"@ is used.
  Caveat
    No attempt is made to wrap large matrices or equations.
    The code for this function is Unix dependent at the moment,
    requiring that certain commands like {\tt pdflatex} are present.
  SeeAlso
    tex
    texMath
    html
///
