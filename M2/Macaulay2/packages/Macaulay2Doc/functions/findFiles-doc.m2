--- author(s): dan
document { 
     Key => {(findFiles,String),findFiles,[findFiles, Exclude],[findFiles, FollowLinks]},
     Headline => "find files recursively",
     Usage => "findFiles p",
     Inputs => {
	  "p",
	  Exclude => "a list of strings, regarded as regular expressions; files found whose paths
	      have base filenames matching one of these regular expressions will be excluded from the results",
	  FollowLinks => Boolean => "whether to follow symbolic links to directories and to report on files found there"
	  },
     Outputs => {
	  {"a list of paths to files found in the directory tree rooted at the path specified by ", TT "p" }
	  }
     }
