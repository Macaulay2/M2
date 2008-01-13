--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => removeDirectory,
     Headline => "remove a directory",
     Usage => "removeDirectory dir",
     Inputs => { "dir" => String => "a filename or path to a directory" },
     Consequences => { "the directory is removed" },     
     EXAMPLE lines ///
     	  dir = temporaryFileName()
	  makeDirectory dir
	  readDirectory dir
	  removeDirectory dir
     ///,
     SeeAlso => {readDirectory, makeDirectory}
     }
