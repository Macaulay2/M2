--		Copyright 2000 by Daniel R. Grayson

BEGIN := ascii 2
END := ascii 5

statementNumber := 0

String.TeXmacsEvaluate = s -> (
     statementNumber = statementNumber + 1;
     v := value s;
     << BEGIN << "verbatim:" 
	<< BEGIN << "latex:" << endl
	   << "\\begin{eqnarray*}" << endl
	      << "\\text{o" << statementNumber << "} = &" << tex v << endl
	   << "\\end{eqnarray*}" << endl
	<< END << endl << endl
	<< BEGIN << "latex:" << endl
	   << "\\begin{eqnarray*}" << endl
	      << "\\text{o" << statementNumber << "} : &" << tex class v << endl
	   << "\\end{eqnarray*}" << endl
	<< END << endl << endl
     << END << flush;
     )

addStartFunction(
     () -> (
	  TeXmacsMode = member("--texmacs",commandLine);
	  if TeXmacsMode then (
	       << BEGIN << "verbatim:" << " Macaulay 2 starting up " << endl << END << flush;
	       );
	  )
     )
