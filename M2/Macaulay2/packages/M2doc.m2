newPackage ("M2doc",
     Version => version#"VERSION")

beginDocumentation()

-- should rewrite this to load all the m2 files there
-- perhaps write a "pathFind" routine so we can find files such as M2doc on the path.

stderr << "--loading the Macaulay2 documentation" << endl

load "M2doc/doc.m2"
load "M2doc/doc1.m2"
load "M2doc/doc2.m2"
load "M2doc/doc3.m2"
load "M2doc/doc4.m2"
load "M2doc/doc5.m2"
load "M2doc/doc6.m2"
load "M2doc/doc7.m2"
load "M2doc/doc8.m2"
load "M2doc/doc9.m2"
load "M2doc/doc10.m2"
load "M2doc/doc11.m2"
load "M2doc/doc12.m2"
load "M2doc/minPres_doc.m2"
load "M2doc/normal_doc.m2"
load "M2doc/overviewA.m2"
load "M2doc/overviewB.m2"
load "M2doc/overviewC.m2"
load "M2doc/overview2.m2"
load "M2doc/overview3.m2"
load "M2doc/overview4.m2"
load "M2doc/macaulay.m2"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages M2doc.installed"
-- End:
