--		Copyright 2000 by Daniel R. Grayson

TeXmacsBegin = ascii 2
TeXmacsEnd   = ascii 5

red := p -> concatenate("<mstyle color=\"red\">",concatenate p,"</mstyle>")
mathMode := s -> concatenate("<math xmlns=\"http://www.w3.org/1998/Math/MathML\">",concatenate s,"</math>")
prompt := (sep,rhs) -> concatenate(
     "<mrow>",
     red concatenate("<mtext>o",toString lineNumber," ",sep," </mtext>"),
     rhs,
     "</mrow>")
para := p -> concatenate("<p>",concatenate p,"</p>")
tmhtml := p -> concatenate(TeXmacsBegin,"html:",concatenate p,TeXmacsEnd)
tmverbatim := p -> concatenate(TeXmacsBegin,"verbatim:",concatenate p,TeXmacsEnd)

String.TeXmacsEvaluate = s -> (
     lineNumber = lineNumber + 1;
     << TeXmacsBegin << "verbatim:";
     try (
	  v := try value s else "evaluation failed";
	  v = (lookup(AfterEval,class v)) v;
     	  fmt := v -> mathMode concatenate lines try mathML v else mathML toString v;
	  fv := mathMode(prompt("=",fmt v));
	  fcv := mathMode(prompt(":", fmt class v));
	  << tmhtml( fv, para(), fcv ))
     else << "top level loop failed";
     << TeXmacsEnd << flush;
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
