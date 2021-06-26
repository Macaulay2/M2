--- status: moved February 2021
--- author(s):
--- notes:

doc ///
Node
  Key
    locate
   (locate, Symbol)
   (locate, Sequence)
   (locate, Pseudocode)
   (locate, Function)
   (locate, Nothing)
   (locate, List)
   (locate, ZZ)
  Headline
    locate source code
  Usage
    locate f
  Inputs
    x:{Function,Sequence,Symbol,List,ZZ}
  Outputs
    :{Sequence,List,Nothing}
      {\tt (filename, start,startcol, stop,stopcol, pos,poscol)}, respectively
  Description
    Text
      For a symbol interpreted function {\tt f}, returns a sequence {\tt (n,i,c,j,d,k,e)}
      describing the location of the definition in the source code:
    Tree
      :The name of the source file is {\tt n};
      :the code occupies line {\tt i} column {\tt c} through line {\tt j} column {\tt d};
      :the central point of interest located at line {\tt k} column {\tt e}.
    Example
      locate needs
      code needs
    Text
      If {\tt f} is a sequence, then @TO "lookup"@ is applied first,
      and the location of the resulting function is provided.
    Example
      locate(resolution, Module)
    Text
      In particular, the output of @TO "methods"@ can be used as an input to @TT "locate"@.
    Example
      locate methods resolution
      methods doc
      locate 0
    Text
      If the function {\tt f} is compiled, or if {\tt f} is @TO "null"@,
      then a location is not available and @TO "null"@ is returned.
  SeeAlso
    code
    methods
    (locate, DocumentTag)
///
