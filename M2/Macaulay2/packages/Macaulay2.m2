newPackage ("Macaulay2",
     Version => version#"VERSION")

beginDocumentation()

-- should rewrite this to load all the m2 files there
-- perhaps write a "pathFind" routine so we can find files such as Macaulay2 on the path.

stderr << "--loading the Macaulay2 documentation" << endl

load "Macaulay2/doc.m2"
load "Macaulay2/doc1.m2"
load "Macaulay2/doc2.m2"
load "Macaulay2/doc3.m2"
load "Macaulay2/doc4.m2"
load "Macaulay2/doc5.m2"
load "Macaulay2/doc6.m2"
load "Macaulay2/doc7.m2"
load "Macaulay2/doc8.m2"
load "Macaulay2/doc9.m2"
load "Macaulay2/doc10.m2"
load "Macaulay2/doc11.m2"
load "Macaulay2/doc12.m2"
load "Macaulay2/minPres_doc.m2"
load "Macaulay2/normal_doc.m2"
load "Macaulay2/overviewA.m2"
load "Macaulay2/overviewB.m2"
load "Macaulay2/overviewC.m2"
load "Macaulay2/overview2.m2"
load "Macaulay2/overview3.m2"
load "Macaulay2/overview4.m2"
load "Macaulay2/macaulay.m2"
load "Macaulay2/doc_packages.m2"
load "Macaulay2/overview_packages.m2"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages Macaulay2.installed"
-- End:
