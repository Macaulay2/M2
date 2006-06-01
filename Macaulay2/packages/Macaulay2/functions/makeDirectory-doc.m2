--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {(makeDirectory,String),makeDirectory},
     Headline => "make a directory",
     Usage => "makeDirectory dir",
     Inputs => { "dir" => String => "a path to the desired directory" },
     Consequences => { { "the directory is made, with as many new path components as needed" } },     
     EXAMPLE lines ///
     	  dir = temporaryFileName()
	  makeDirectory (dir|"/a/b/c")
	  removeDirectory (dir|"/a/b/c")
	  removeDirectory (dir|"/a/b")
	  removeDirectory (dir|"/a")
     ///
     SeeAlso => {}
     }
