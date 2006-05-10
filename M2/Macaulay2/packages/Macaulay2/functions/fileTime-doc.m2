--- status: DRAFT
--- author(s): MES
--- notes: 

document { 
     Key => fileTime,
     Headline => "get or set file modified time",
     Usage => "d = fileTime s  or  fileTime(s,e)",
     Inputs => {
	  "s" => String => "the filename",
	  "e" => ZZ => "if present, set the file modified time to this value"
	  },
     Outputs => {
	  "d" => ZZ => "the last modified date of the file with name s"
	  },
     Consequences => {
	  "fileTime(s,e) sets the last modified time for the file s to e, and
	  returns null if no error occurs"
	  },     
     "The value is generally the number of seconds from the beginning of the 'epoch', so the
     number of seconds ago the file 'foo' was modified may be found by using
     the following code.",
     PRE ///  currentTime() - fileTime "foo"///,          
     "To set the time of the file 'foo' to one hour before the current time, we would
     use the following code.",
     PRE ///fileTime("foo", currentTime()-3600)///,
     SeeAlso => {currentTime, "file manipulation"}
     }

end

///
currentTime()  - fileTime "a"
fileTime("a",currentTime()-24*3600)
///
