document { 
     Key => fileExists,
     Headline => "whether a file exists",
     Usage => "fileExists fn",
     Inputs => { "fn" => String },
     Outputs => { Boolean => { "whether a file with the filename or path ", TT "fn", " exists" }},
     EXAMPLE lines ///
	  fn = temporaryFileName()
	  fileExists fn
	  fn << "hi there" << close
	  fileExists fn
	  removeFile fn
     ///,
     "If ", TT "fn", " refers to a symbolic link, then whether the file exists is determined by the content of the link and whether there is
     a file with the new path determined by it.  The presence of a symbolic link can be detected with ", TO "readlink", "."
     }
