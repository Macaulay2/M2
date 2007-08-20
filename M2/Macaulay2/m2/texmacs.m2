--		Copyright 2000 by Daniel R. Grayson

TeXmacsBegin = ascii 2
TeXmacsEnd   = ascii 5

String.TeXmacsEvaluate = s -> (
     lineNumber = lineNumber + 1;
     << TeXmacsBegin << "verbatim:";
     try (
	  v := value s;
	  v = (lookup(AfterEval,class v)) v;
	  if v =!= null then value concatenate("symbol o",toString lineNumber) <- v;
     	  fmt := v -> concatenate lines try mathML v else mathML toString v;
	  if v =!= null then (
	     << TeXmacsBegin << "html:"
	     << "<p>" << "o" << lineNumber << " = " << "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">" << fmt v << "</math>" << "</p>"
	     << "<p></p>"
	     << "<p>" << "o" << lineNumber << " : " << "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">" << fmt class v << "</math>" << "</p>"
	     << TeXmacsEnd;
	     );
	  )
     else << "evaluation failed";
     << TeXmacsEnd << flush;
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
