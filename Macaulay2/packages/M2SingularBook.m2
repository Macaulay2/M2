newPackage(
	"M2SingularBook",
    	Version => "1.0", 
    	Date => "August 23, 2005",
    	Authors => {
	     {Name => "Mike Stillman", Email => "mike@math.cornell.edu"}
	     },
    	HomePage => "http://www.math.cornell.edu/~mike/",
    	Headline => "Macaulay2 examples for the Singular book",
    	DebuggingMode => true 
    	)

poly = method()
poly String := (f) -> (ideal f)_0

showPDF = x -> (
     f := temporaryFileName();
     f | ".tex" 
     << ///\documentclass{article}
     \usepackage{amsmath}
     \usepackage{amssymb}
     \begin{document}
     ///  
     << tex x <<
     ///
     \end{document}
     ///  
     << close;
     if 0 === run("cd /tmp; pdflatex " | f)
     then run("(open "|f|".pdf; rm -f "|f|".tex "|f|".dvi "|f|".log "|f|".aux)&")
     else error ("latex failed on input file " | f | ".tex")
     )

beginDocumentation()

load "M2SingularBook/doc.m2"

