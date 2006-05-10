--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => symlinkFile,
     Headline => "",
     Usage => "symlinkFile(source,target)",
     Inputs => {
	  "source" => String => "",
	  "target" => String => ""
	  },
     Consequences => {
	  {"Returns null if successful.  
	  Creates a symbolic link ", TT "target", " to the 
	  original file ", TT "source"}
	  },     
     "This is equivalent to the unix command ", 
     TT "ln -s source target",
     "Both the source and the target should be either absolute
     paths or ",
     PRE ///symlinkFile("~/foo.m2", ".")///,
     Caveat => {},
     SeeAlso => {"file manipulation"}
     }


processID     
currentTime

minimizeFilename
relativizeFilename 
fileMode -- can use to get or set mode (can take File, or String), and ZZ.
         -- fileMode(File,ZZ)
	 -- fileMode(String,ZZ)
	 -- fileMode(String)
fileLength (File or String)
openFiles
loadedFiles -- mutable hash table: i th entry, path to file loaded