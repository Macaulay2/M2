newPackage ("Macaulay2",
     Version => version#"VERSION")

beginDocumentation()

-- should rewrite this to load all the m2 files there
-- perhaps write a "pathFind" routine so we can find files such as Macaulay2 on the path.

stderr << "--loading the Macaulay2 documentation" << endl

load "Macaulay2Doc/doc.m2"
load "Macaulay2Doc/doc1.m2"
load "Macaulay2Doc/doc2.m2"
load "Macaulay2Doc/doc3.m2"
load "Macaulay2Doc/doc4.m2"
load "Macaulay2Doc/doc5.m2"
load "Macaulay2Doc/doc6.m2"
load "Macaulay2Doc/doc7.m2"
load "Macaulay2Doc/doc8.m2"
load "Macaulay2Doc/doc9.m2"
load "Macaulay2Doc/doc10.m2"
load "Macaulay2Doc/doc11.m2"
load "Macaulay2Doc/doc12.m2"
load "Macaulay2Doc/minPres_doc.m2"
load "Macaulay2Doc/normal_doc.m2"
load "Macaulay2Doc/overviewA.m2"
load "Macaulay2Doc/overviewB.m2"
load "Macaulay2Doc/overviewC.m2"
load "Macaulay2Doc/overview2.m2"
load "Macaulay2Doc/overview3.m2"
load "Macaulay2Doc/overview4.m2"
load "Macaulay2Doc/macaulay.m2"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2Doc/packages Macaulay2Doc.installed"
-- End:
