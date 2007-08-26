--		Copyright 2000 by Daniel R. Grayson
TeXmacsBegin = ascii 2
TeXmacsEnd   = ascii 5
badcharregex := TeXmacsBegin | "|" | TeXmacsEnd
fix := s -> replace(TeXmacsBegin,"[BEGIN]",replace(TeXmacsEnd,"[END]",s))
fixn := s -> concatenate between("\0", apply(separate("\0", s), fix))
red := p -> concatenate("<mstyle color=\"red\">",concatenate p,"</mstyle>")
mathMode := s -> concatenate("<math xmlns=\"http://www.w3.org/1998/Math/MathML\">",concatenate s,"</math>")
prompto := () -> red concatenate("<mi>o",toString lineNumber,"</mi>")
prompt0 := sep -> red concatenate("<mtext>o",toString lineNumber," ",sep," </mtext>")
prompt := (sep,rhs) -> concatenate( "<mrow>", prompt0 sep, rhs, "</mrow>")
para := p -> concatenate("<p>",concatenate p,"</p>")
tmhtml := p -> concatenate(TeXmacsBegin,"html:",concatenate p,TeXmacsEnd)
tmverbatim := p -> concatenate(TeXmacsBegin,"verbatim:",concatenate p,TeXmacsEnd)
mtable := x -> concatenate(
     "<mtable columnalign=\"right center left\">", newline,
     apply(x, row -> ( "<mtr>", apply(row, e -> ("<mtd>",e,"</mtd>",newline)), "</mtr>", newline ) ),
     "</mtable>", newline )
String.TeXmacsEvaluate = s -> (
     lineNumber = lineNumber + 1;
     << TeXmacsBegin << "verbatim:";
     try (
	  v := try value s else "evaluation failed";
	  m := lookup(AfterEval,class v);
	  if m =!= null then v = m v;
     	  fmt := v -> concatenate lines try mathML v else mathML toString v;
	  po := prompto();
	  << tmhtml fixn mathMode mtable {{po,red "<mo>=</mo>",fmt v},{},{po,red "<mo>:</mo>",fmt class v}})
     else << "top level loop failed";
     << TeXmacsEnd << flush;
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
