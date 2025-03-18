undocumented (pretty, Thing)
document { Key => pretty,
     Headline => "a pretty printer", "This function is experimental and under development." }

document {
    Key => {
	NetFile,
	(symbol <<, NetFile, String),
	(symbol <<, NetFile, Net),
	(symbol SPACE, Manipulator, NetFile),
	(symbol <<, NetFile, Manipulator)
     },
     Headline => "the class of all net files",
     "This class is experimental.  Net files are intended to supplant output files eventually.  Whereas a file is a stream of bytes,
     or in some non-unix operating systems, a sequence of lines each of which is a sequence of bytes, a net file is a sequence of lines, each of which is
     a net.  Each output line is assembled by joining nets one by one.",
     EXAMPLE lines ///
	  f = newNetFile()
	  f << "aabbcc" << endl
	  f << "aa" << "bb"^1 << "cc"^-1 << endl
	  f << "aa" << "bb"^1 << "cc"^-1 << endl
	  getNetFile f
	  peek oo
	  class \ ooo
     ///,
     Subnodes => {
	 TO newNetFile,
	 TO getNetFile,
     }
}

document {
    Key => getNetFile,
    Headline => "get the sequence of completed lines (nets) from a net file",
    Usage => "getNetFile n",
    Inputs => { "n" => NetFile },
    "This function is experimental."
}

document {
    Key => newNetFile,
    Headline => "create a new net file",
    Usage => "newNetFile()",
    Outputs => { NetFile },
    "This function is experimental."
}
