--		Copyright 2000 by Daniel R. Grayson

BEGIN := ascii 2
END := ascii 5

statementNumber := 0

String.TeXmacsEvaluate = s -> (
     statementNumber = statementNumber + 1;
     try (
	  v := value s;
     	  tv := try texMath v else "\\text{Display failed}";
     	  tc := try texMath class v else "\\text{Class display failed}";
	  -- << BEGIN << "verbatim:" 
	     << BEGIN << "latex:"
		<< "\\begin{eqnarray*}"
		   << "\\text{o" << statementNumber << "} & = &" << tv << "\\\\"
		   << "\\text{o" << statementNumber << "} & : &" << tc
		<< "\\end{eqnarray*}"
	     << END
	  -- << END;
	  )
     else (
	  << BEGIN << "verbatim: evaluation failed" << END;
	  );
     << flush;
     )

addStartFunction(
     () -> (
	  TeXmacsMode = member("--texmacs",commandLine);
	  if TeXmacsMode then (
	       << BEGIN << "verbatim:" << " Macaulay 2 starting up " << endl << END << flush;
	       );
	  )
     )
