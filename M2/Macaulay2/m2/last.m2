--		Copyright 1993-2001 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

recursionLimit = 300

addStartFunction(() -> path = unique apply( path, minimizeFilename))
addEndFunction(() -> (
	  scan(openFiles(), f -> if isOutputFile f then flush f);
	  path = {};
	  )
     )

addStartFunction(() -> (
	  printWidth = width stdio; -- later, we should adjust this when a window resizing interrupt occurs; "wrap" uses 80 if printWidth == 0
	  ))

lastLN := 0
lastWI := 0
promptWidth = () -> (
     if lineNumber === lastLN then lastWI
     else (
	  lastLN = lineNumber;
	  lastWI = max \\ width \ lines ZZ.InputPrompt lineNumber))

wr := (sep,x) -> wrap(printWidth - promptWidth(), sep, net x)
RawMatrix.Wrap = Matrix.Wrap = RingElement.Wrap = List.Wrap = Sequence.Wrap = x -> wr("-",x)
String.Wrap = x -> wr("",x)

erase symbol gg; erase symbol ggautoreduce; erase symbol ggbetti; erase symbol ggborel; erase symbol ggcalc; erase symbol ggcoeffs; erase symbol ggcompare; erase symbol ggcopy; erase symbol
ggdeclarefield; erase symbol ggdets; erase symbol ggdim; erase symbol ggdiv; erase symbol ggdup; erase symbol ggelem; erase symbol ggelim; erase symbol ggexterior; erase symbol
ggexteriorproduct; erase symbol ggfactor1; erase symbol ggfactor2; erase symbol gggb; erase symbol gggcdextended; erase symbol gggetchange; erase symbol gggetcols; erase symbol gggetgb;
erase symbol gggetmingens; erase symbol gggetsyz; erase symbol gggetvalue; erase symbol gggetzerodivisor; erase symbol ggGF; erase symbol gghilb; erase symbol ggindex; erase symbol
gginitial; erase symbol ggINT; erase symbol ggisequal; erase symbol ggissubset; erase symbol ggkbasis; erase symbol ggleadmonom; erase symbol ggmatrix; erase symbol ggmodtensor; erase symbol
ggmonideal; erase symbol ggmult; erase symbol ggnegate; erase symbol ggnmonoms; erase symbol ggpairs; erase symbol ggpfaffs; erase symbol ggpick; erase symbol ggpop; erase symbol ggprimes;
erase symbol ggqring; erase symbol ggrandom; erase symbol ggrandomseed; erase symbol ggreduce; erase symbol ggremaining; erase symbol ggres; erase symbol ggresmap; erase symbol ggresmodule;
erase symbol ggringmap; erase symbol ggsat; erase symbol ggsimplify; erase symbol ggsortcolumns; erase symbol ggstats; erase symbol ggsubmatrix; erase symbol ggterm; erase symbol ggtonet;
erase symbol ggtruncate; erase symbol callgg; erase symbol convert; erase symbol ConvertToExpression; erase symbol eePopBool; erase symbol eePopInt; erase symbol eePopIntarray; erase symbol
handle;

-- some symbols are mentioned in the documentation only:
GLex

-- make sure this is after all global symbols are defined or erased
closePackage "Macaulay2"

if not Macaulay2#?"processed documentation database" or not isOpen Macaulay2#"processed documentation database" then (
     currentPackage = Macaulay2;
     stderr << "--loading Macaulay2-doc.m2" << endl;
     notify = true;
     load "Macaulay2-doc.m2";
     currentPackage = null;
     )

addStartFunction( () -> if sourceHomeDirectory =!= null then Macaulay2#"source directory" = sourceHomeDirectory )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
