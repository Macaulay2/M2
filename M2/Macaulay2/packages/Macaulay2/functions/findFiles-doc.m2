--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {(findFiles,String),findFiles},
     Headline => "find files recursively",
     Usage => "findFiles p",
     Inputs => {
	  "p",
	  Exclude => "a list of strings, regarded as regular expressions; files found whose paths
	      have base filenames matching one of these regular expressions will be excluded from the results",
	  FollowLinks => Boolean => "whether to follow symbolic links to directories and to report on files found there"
	  },
     Outputs => {
	  {"a list of paths files found in the directory tree rooted at the path specified by ", TT "p" }
	  }
     }

-- document { 
--      Key => [findFiles, Exclude],
--      Usage => "findFiles(..., Exclude => e)",
--      Inputs => {
-- 	  "e" => {"a list of strings, regarded as regular expressions"}
-- 	  },
--      Consequences => {
-- 	  {"paths to files whose base filename part match one of the regular expressions in ", TT "e", " will be excluded from the list returned"}
-- 	  },     
--      SeeAlso => {"regular expressions", "baseFilename" }
--      }
-- document { 
--      Key => [findFiles, FollowLinks],
--      Usage => "findFiles(..., FollowLinks => true)",
--      Consequences => {
-- 	  {"symbolic links to directories will be followed, and files in those directories may appear in the list returned" }
-- 	  }
--      }
