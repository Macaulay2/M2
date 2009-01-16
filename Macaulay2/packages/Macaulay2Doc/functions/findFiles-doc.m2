--- author(s): dan
document { 
     Key => {findFiles,(findFiles,String),(findFiles,List),[findFiles, Exclude],[findFiles, FollowLinks]},
     Headline => "find files recursively",
     Usage => "findFiles p",
     Inputs => {
	  "p" => {ofClass{String,List}, ", a directory or list of directories"},
	  Exclude => "a list of strings, regarded as regular expressions; files found whose paths
	      have base filenames matching one of these regular expressions will be excluded from the results",
	  FollowLinks => Boolean => "whether to follow symbolic links to directories and to report on files found there"
	  },
     Outputs => {
	  {"a list of paths to files found in the directory tree(s) rooted at the locations(s) specified by ", TT "p" }
	  }
     }
