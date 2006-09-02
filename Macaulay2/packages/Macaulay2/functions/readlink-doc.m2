--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => readlink,
     Headline => "readlink fn",
     Usage => "read the contents of a symbolic link",
     Inputs => { "fn" => String => "a filename or path to a file" },
     Outputs => { Boolean => { "whether ", TT "fn", " is the path to a symbolic link" }},
     EXAMPLE lines ///
     	  p = temporaryFileName ()
	  symlinkFile ("foo", p)
	  readlink p
	  removeFile p
     ///
     }
