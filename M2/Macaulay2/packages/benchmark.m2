<< "-- beginning computation --" << endl

rr = ZZ/101[a..Z];
cc = timing res coker genericMatrix(rr,a,3,9)
ti = cc#0
re = cc#1
vv = apply(8,i->rank re_i)
assert( vv == {3, 9, 126, 378, 504, 360, 135, 21} )

<< "-- " << first lines get "!uname -a" << endl
<< "-- " <<  toString ti << " seconds"
<< ", Macaulay2 " << version#"VERSION"
<< ", compiled with " << version#"compiler"
<< endl

-- Results:

-- OSF1 despina.ks.uiuc.edu V4.0 1229 alpha
-- 0.75152 seconds, Macaulay2 0.8.52

-- Linux noether.matematik.su.se 2.1.121 #3 Wed Sep 16 10:05:16 CEST 1998 alpha unknown (600 mHz)
-- 1.20106 seconds, 0.8.46

-- Linux localhost.localdomain 2.2.1 #101 Fri Feb 5 16:17:12 EST 1999 ppc unknown
-- (250 MHz Macintosh Powerbook G3)
-- 1.67 seconds, Macaulay 2 version 0.8.52

-- SunOS orion.math.uiuc.edu 5.5.1 Generic_103640-26 sun4u sparc SUNW,Ultra-4
-- 2.14 seconds, Macaulay2 0.8.53, compiled with gcc 2.8

-- Linux yttrium 2.2.6 #82 Sun Apr 18 15:06:16 CDT 1999 i686 unknown
-- e-machine: Cyrix M II - 333 MMX
-- 2.45 seconds, Macaulay2 0.8.53, compiled with gcc 2.91

-- Linux geometry 2.1.121 #33 SMP Tue Sep 15 21:44:25 CDT 1998 i586
-- 4.01 seconds, Macaulay 2 version 0.8.47, compiled with gcc

-- Linux geometry 2.2.0-pre4 #65 Mon Jan 4 20:14:06 CST 1999 i586 unknown
-- 4.17 seconds, Macaulay 2 version 0.8.50, compiled with gcc 2.8.1

----- with SHAREDLIBS, including interpeter but not engine:
-- Linux geometry 2.2.0-pre4 #65 Mon Jan 4 20:14:06 CST 1999 i586 unknown
-- 4.27 seconds, Macaulay 2 version 0.8.50

-- Linux geometry 2.2.2 #77 Wed Feb 24 10:40:05 EST 1999 i586 unknown
-- 4.31 seconds, Macaulay2 0.8.53, compiled with gcc 2.91, statically linked

-- CYGWIN_NT-4.0 GEOMETRY 20.1 (0.3/1/1) 1998-12-3 20:39:18 i586 unknown
-- 4.327 seconds, Macaulay 2 version 0.8.52
-- gcc version egcs-2.91.60 19981201 (egcs-1.1.1 release)

-- Linux geometry 2.2.2 #77 Wed Feb 24 10:40:05 EST 1999 i586 unknown
-- 4.36 seconds, Macaulay2 0.8.53, compiled with gcc 2.8, dynamically linked

-- Linux geometry 2.2.2 #77 Wed Feb 24 10:40:05 EST 1999 i586 unknown
-- 4.38 seconds, Macaulay2 0.8.53, compiled with gcc 2.91

----- with SHAREDLIBS, including engine and interpeter:
-- Linux geometry 2.2.0-pre4 #65 Mon Jan 4 20:14:06 CST 1999 i586 unknown
-- 4.81 seconds, Macaulay 2 version 0.8.50

-- IRIX illi 5.3 12201932 IP22 mips
-- 	5.49 seconds		0.8.10

-- with SHAREDLIBS and DEBUG and egcs compiler
-- Linux geometry 2.2.2 #77 Wed Feb 24 10:40:05 EST 1999 i586 unknown
-- 5.65 seconds, Macaulay2 0.8.52, compiled with gcc 2.91

-- HP-UX ux1 B.10.10 U 9000/819 65611361 unlimited-user license
-- 	6.26 seconds

-- NeXTstep mms-hal 3.2 2 i586
-- 	6.74973 seconds

-- SunOS saturn.math.uiuc.edu 5.5.1 Generic sun4m sparc SUNW,SPARCstation-5
-- 7.71 seconds, Macaulay 2 version 0.8.41

-- SunOS orion.math.uiuc.edu 5.5 Generic sun4d sparc SUNW,SPARCserver-1000
-- 	8.41 seconds		0.8.46

-- SunOS kilburn 4.1.4 2 sun4m
-- Macaulay 2 version 0.8.46
-- 10.17 seconds

-- HP-UX skolem A.09.05 A 9000/750 2013387634 two-user license
-- 	10.25 seconds

-- Linux homotopy 2.0.14, NEC Versa E, i486 laptop, DX4/100, 25 mhz
-- 	Macaulay 2, version 0.8.26
--      	22.04 seconds
