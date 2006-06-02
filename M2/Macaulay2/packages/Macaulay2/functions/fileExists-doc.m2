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
     ///
     }
