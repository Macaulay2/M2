--- status: TODO
--- author(s): 
--- notes: 

doc ///
  Key
    isRegularFile
  Headline
    whether a file is a regular file
  Usage
    isRegularFile fn
  Inputs
    fn:String
      a filename or path
  Outputs
    :Boolean
      whether @TT "fn"@ is the path to a regular file
  Description
    Text
      In UNIX, a regular file is one that is not special in some way. Special
      files include symbolic links and directories. A regular file is a
      sequence of bytes stored permanently in a file system.
    Example
      fn = temporaryFileName()
      fn << "hi there" << close
      isRegularFile fn
      removeFile fn
  SeeAlso
    isDirectory
    readlink
///
