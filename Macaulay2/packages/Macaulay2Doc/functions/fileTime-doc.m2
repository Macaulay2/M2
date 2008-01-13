--- status: DRAFT
--- author(s): MES
--- notes: 

document { 
     Key => fileTime,
     Headline => "get or set file modified time",
     Usage => "fileTime s  or  fileTime(t,s)",
     Inputs => {
	  "t" => ZZ => "if present, the file-modified-time will be set to this value",
	  "s" => String => "the name of a file"
	  },
     Outputs => {
	  ZZ => { "the last modified date of the file with name ", TT "s" }
	  },
     Consequences => {
	  { TT "fileTime(t,s)", " sets the last modified time for the file ", TT "s", " to ", TT "t", ", and returns ", TT "null", " if no error occurs" }
	  },     
     "The value is the number of seconds since 00:00:00 1970-01-01 UTC, the beginning of the epoch, so the
     number of seconds ago a file or directory was modified may be found by using the following code.",
     EXAMPLE ///currentTime() - fileTime "."///,          
     SeeAlso => {currentTime, "file manipulation"}
     }
