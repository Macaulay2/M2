undocumented (pretty, Thing)
doc ///
  Key
    pretty
  Headline
    a pretty printer
  Description
    Text
      This function is experimental and under development.
///

doc ///
  Key
    NetFile
    (symbol <<, NetFile, String)
    (symbol <<, NetFile, Net)
    (symbol SPACE, Manipulator, NetFile)
    (symbol <<, NetFile, Manipulator)
  Headline
    the class of all net files
  Description
    Text
      This class is experimental. Net files are intended to supplant output
      files eventually. Whereas a file is a stream of bytes, or in some
      non-unix operating systems, a sequence of lines each of which is a
      sequence of bytes, a net file is a sequence of lines, each of which is a
      net. Each output line is assembled by joining nets one by one.
    Example
      f = newNetFile()
      f << "aabbcc" << endl
      f << "aa" << "bb"^1 << "cc"^-1 << endl
      f << "aa" << "bb"^1 << "cc"^-1 << endl
      getNetFile f
      peek oo
      class \ ooo
  Subnodes
    newNetFile
    getNetFile
///

doc ///
  Key
    getNetFile
  Headline
    get the sequence of completed lines (nets) from a net file
  Usage
    getNetFile n
  Inputs
    n:NetFile
  Description
    Text
      This function is experimental.
///

doc ///
  Key
    newNetFile
  Headline
    create a new net file
  Usage
    newNetFile()
  Outputs
    :NetFile
  Description
    Text
      This function is experimental.
///
