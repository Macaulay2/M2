--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => realpath,
     Headline => "convert a filename to one passing through no symbolic links",
     Usage => "realpath fn",
     Inputs => { "fn" => String => "a filename, or path to a file" },
     Outputs => { String => { "the canonicalized absoluate pathname to ", TT "fn" }},
     EXAMPLE lines ///
     	  realpath "."
     	  p = temporaryFileName()
	  q = temporaryFileName()
	  symlinkFile(p,q)
	  readlink q
	  realpath q
	  p << "hi there" << close
	  realpath q
	  removeFile q
     ///,
     SeeAlso => {readlink},
     Caveat => "Links going nowhere are not traversed, as the example shows."
     }
