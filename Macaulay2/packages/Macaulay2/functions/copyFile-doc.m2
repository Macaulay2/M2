--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => copyFile,
     Headline => "copy a file",
     Usage => "copyFile(oldnm,newnm)",
     Inputs => {
	  "oldnm" => String => "the filename or path to an existing regular file",
	  "newnm" => String => "the filename or path to the copy to be made",
	  UpdateOnly => Boolean => { "whether to copy the file only if ", TT "newnm", " refers to a non-existent file or to a file that is older than ", TT "oldnm" },
	  Verbose => Boolean => { "whether to provide feedback about the operation performed" }
	  },
     Consequences => {
	  "the file may be copied"
	  },     
     EXAMPLE lines ///
     	  f = temporaryFileName()
     	  g = temporaryFileName()
	  f << "hi there" << close
	  copyFile(f,g,UpdateOnly=>true,Verbose=>true)
	  get g
	  copyFile(f,g,UpdateOnly=>true,Verbose=>true)
	  removeFile f
	  removeFile g
     ///,
     SeeAlso => { copyDirectory, symlinkDirectory }
     }


document { Key => [copyFile, UpdateOnly],
     Usage => "copyFile(..., UpdateOnly => true)",
     Consequences => {{ "during the indicated copy operation, newer files will not be replaced by copies of older ones" }}}
document { Key => [copyFile, Verbose],
     Usage => "copyFile(..., Verbose => ...)",
     Consequences => {{ "during the file operation, details of the operations performed will be displayed" }}}


document { 
     Key => (copyFile,String,String),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [copyFile, Exclude],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [copyFile, Verbose],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [copyFile, Undo],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [copyFile, UpdateOnly],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
