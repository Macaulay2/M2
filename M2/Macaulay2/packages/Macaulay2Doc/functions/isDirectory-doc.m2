--- status: TODO
--- author(s): 
--- notes: 

doc ///
  Key
    isDirectory
  Headline
    whether a file is a directory
  Usage
    isDirectory fn
  Inputs
    fn:String
      a filename or path
  Outputs
    :Boolean
      whether @TT "fn"@ is the path to a directory
  Description
    Example
      isDirectory "."
      fn = temporaryFileName()
      fn << "hi there" << close
      isDirectory fn
      removeFile fn
  SeeAlso
    isRegularFile
    readlink
///
