--		Copyright 2000 by Daniel R. Grayson

BEGIN := ascii 2
END := ascii 5

statementNumber := 0

String.TeXmacsEvaluate = s -> (
     statementNumber = statementNumber + 1;
     v := try value s else "Evaluation failed";
     tv := try texMath v else "\\text{Display failed}";
     tc := try texMath class v else "\\text{Class display failed}";
     << BEGIN << "verbatim:" 
	<< BEGIN << "latex:" << endl
	   << "\\begin{eqnarray*}" << endl
	      << "\\text{o" << statementNumber << "} = &" << tv << "\\\\" << endl
	      << "\\text{o" << statementNumber << "} : &" << tc << endl
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
