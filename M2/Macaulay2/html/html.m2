makeHTML (
     "Macaulay 2",					    -- top node
     directoryPath { "." },				    -- path to gifs
     directoryPath {"..","m2","cache","doc"},		    -- path to put *.html in
     DocDatabase					    -- the entire doc database 
     )
