--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => readlink,
     Headline => "get the resolved value a symbolic link",
     Usage => "readlink fn",
     Inputs => { "fn" => String => "a filename or path to a file" },
     Outputs => { String => { "the resolved path to a symbolic link,
	     or null if the file was not a symbolic link." }},
     EXAMPLE lines ///
     	  p = temporaryFileName ()
	  symlinkFile ("foo", p)
	  readlink p
	  removeFile p
     ///,
     SeeAlso => { realpath },
     }
