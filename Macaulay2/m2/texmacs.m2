--		Copyright 2000 by Daniel R. Grayson

BEGIN := ascii 2
END := ascii 5

statementNumber := 0

rot := x -> (
     symbol oooo <- ooo;			  -- avoid GlobalAssignHook with <-
     symbol ooo <- oo;
     symbol oo <- x;
     )

String.TeXmacsEvaluate = s -> (
     statementNumber = statementNumber + 1;
     try (
	  v := value s;
	  if v =!= null then value concatenate("symbol o",toString statementNumber) <- v;
	  rot v;
     	  tv := try texMath v else "\\text{Display failed}";
     	  tc := try texMath class v else "\\text{Class display failed}";
	  << BEGIN << "latex:";
	     if v =!= null then (
		<< "\\begin{eqnarray*}"
		   << "\\text{o" << statementNumber << "} & = & " << tv << "\\\\"
		   << "\\text{o" << statementNumber << "} & : & " << tc
		<< "\\end{eqnarray*}"
		);
	  << END;
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
