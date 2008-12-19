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
	  p << close
	  realpath q
	  removeFile q
     ///,
     SeeAlso => {readlink},
     Caveat => "Every component of the path must exist in the file system and be accessible to the user."
     }
