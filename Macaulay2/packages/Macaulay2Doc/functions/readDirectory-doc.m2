--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => readDirectory,
     Headline => "read the contents of a directory",
     Usage => "readDirectory dir",
     Inputs => { "dir" => String => "a filename or path to a directory" },
     Outputs => { List => "the list of filenames stored in the directory" },
     EXAMPLE lines ///
     	  dir = temporaryFileName()
	  makeDirectory dir
	  (fn = dir | "/" | "foo") << "hi there" << close
	  readDirectory dir
	  removeFile fn
	  removeDirectory dir
     ///,
     SeeAlso => { removeDirectory, removeFile, makeDirectory }
     }
