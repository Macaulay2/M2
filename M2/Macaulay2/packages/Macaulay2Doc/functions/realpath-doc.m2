--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => realpath,
     Headline => "convert a filename to one passing through no symbolic links",
     Usage => "realpath fn",
     Inputs => { "fn" => String => "a filename, or path to a file" },
     Outputs => { String => { "a pathname to ", TT "fn", " passing through no symbolic links, and
	       ending with a slash if ", TT "fn", " refers to a directory" }},
     EXAMPLE lines ///
     	  realpath "."
     	  p = temporaryFileName()
	  q = temporaryFileName()
	  symlinkFile(p,q)
	  p << close
	  readlink q
	  realpath q
	  removeFile p
	  removeFile q
     ///,
     PARA {
	  "The empty string is interpreted as a reference to the current directory."
	  },
     EXAMPLE ///realpath ""///,
     SeeAlso => {readlink},
     Caveat => {
	  "Every component of the path must exist in the file system and be accessible to the user.
	  Terminal slashes will be dropped.  Warning: under most operating systems, the value returned
	  is an absolute path (one starting at the root of the file system), but under Solaris,
	  this system call may, in certain circumstances, return a relative path when given a relative path."
	  }
     }
