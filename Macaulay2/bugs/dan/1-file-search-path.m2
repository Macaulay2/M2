-- This was a silly change I was forced to make.  Maybe packages need a default
-- location for separate test files, and "check" will look there.


--- Macaulay2/packages/Dmodules/DMODdoc.m2	(revision 5611)
+++ Macaulay2/packages/Dmodules/DMODdoc.m2	(working copy)
@@ -1,21 +1,21 @@
 -- Copyright 1999-2006 by Anton Leykin and Harrison Tsai
 
-TEST /// input "Dmodules/TST/AnnFs.tst.m2" ///
-TEST /// input "Dmodules/TST/DHom.tst.m2" ///
-TEST /// input "Dmodules/TST/Dbasic.tst.m2" ///
-TEST /// input "Dmodules/TST/Ddual.tst.m2" ///
-TEST /// input "Dmodules/TST/DeRham.tst.m2" ///
-TEST /// input "Dmodules/TST/Dlocalize.tst.m2" ///
-TEST /// input "Dmodules/TST/Dresolution.tst.m2" ///
-TEST /// input "Dmodules/TST/Drestriction.tst.m2" ///
-TEST /// input "Dmodules/TST/WeylClosure.tst.m2" ///
-TEST /// input "Dmodules/TST/b-function.ideal.tst.m2" ///
-TEST /// input "Dmodules/TST/b-function.module.tst.m2" ///
-TEST /// input "Dmodules/TST/localCohom.tst.m2" ///
-TEST /// input "Dmodules/TST/makeCyclic.tst.m2" ///
-TEST /// input "Dmodules/TST/paramBpoly.tst.m2" ///
-TEST /// input "Dmodules/TST/stafford.tst.m2" ///
-TEST /// input "Dmodules/TST/CC.tst.m2" ///
+TEST /// input (Dmodules#"source directory" | "Dmodules/TST/AnnFs.tst.m2") ///
+TEST /// input (Dmodules#"source directory" | "Dmodules/TST/DHom.tst.m2") ///
+TEST /// input (Dmodules#"source directory" | "Dmodules/TST/Dbasic.tst.m2") ///
+TEST /// input (Dmodules#"source directory" | "Dmodules/TST/Ddual.tst.m2") ///
+TEST /// input (Dmodules#"source directory" | "Dmodules/TST/DeRham.tst.m2") ///
+TEST /// input (Dmodules#"source directory" | "Dmodules/TST/Dlocalize.tst.m2") ///
+TEST /// input (Dmodules#"source directory" | "Dmodules/TST/Dresolution.tst.m2") ///
+TEST /// input (Dmodules#"source directory" | "Dmodules/TST/Drestriction.tst.m2") ///
+TEST /// input (Dmodules#"source directory" | "Dmodules/TST/WeylClosure.tst.m2") ///
+TEST /// input (Dmodules#"source directory" | "Dmodules/TST/b-function.ideal.tst.m2") ///
+TEST /// input (Dmodules#"source directory" | "Dmodules/TST/b-function.module.tst.m2") ///
+TEST /// input (Dmodules#"source directory" | "Dmodules/TST/localCohom.tst.m2") ///
+TEST /// input (Dmodules#"source directory" | "Dmodules/TST/makeCyclic.tst.m2") ///
+TEST /// input (Dmodules#"source directory" | "Dmodules/TST/paramBpoly.tst.m2") ///
+TEST /// input (Dmodules#"source directory" | "Dmodules/TST/stafford.tst.m2") ///
+TEST /// input (Dmodules#"source directory" | "Dmodules/TST/CC.tst.m2") ///

