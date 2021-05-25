--		Copyright 2000 by Daniel R. Grayson

needs "reals.m2"

TeXmacsBegin = "\2"
TeXmacsEnd   = "\5"
fix := s -> replace("\2|\5","\33\\0",s)
fixn := s -> concatenate between("\0", apply(separate("\0", s), fix))
red := p -> concatenate("<mstyle color=\"red\">",concatenate p,"</mstyle>")
mathMode := s -> concatenate("<math xmlns=\"http://www.w3.org/1998/Math/MathML\">",concatenate s,"</math>")
para := p -> concatenate("<p>",concatenate p,"</p>")
tmhtml := p -> concatenate(TeXmacsBegin,"html:",concatenate p,TeXmacsEnd)
mtable := x -> concatenate(
     "<mtable columnalign=\"right center left\">", newline,
     apply(x, row -> ( "<mtr>", apply(row, e -> ("<mtd>",e,"</mtd>",newline)), "</mtr>", newline ) ),
     "</mtable>", newline )
fmt := x -> concatenate lines try mathML x else mathML toString x;
Thing#{TeXmacs,Print} = send := v -> (
     po := red concatenate("<mi>o",toString lineNumber,"</mi>");
     << tmhtml fixn mathMode mtable {{po,red "<mo>=</mo>",fmt v},{},{po,red "<mo>:</mo>",fmt class v}})
Nothing#{TeXmacs,Print} = identity
InexactNumber#{TeXmacs,Print} = v -> withFullPrecision ( () -> send v )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
