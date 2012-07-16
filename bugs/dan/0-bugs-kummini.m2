-- -*- coding: utf-8 -*-
-- Place bugs that you find in here, and
-- then check in the file, using (outside of M2):
--   cvs ci -m "another bug"

###### 1.
listUserSymbols returns the following error:
stdio:36:1:(1): after eval: expected a list, hash table, or sequence
stdio:36:1:(1): before print: --backtrace update-- 
stdio:36:1:(1): at print: --backtrace update-- 

o36 : 
stdio:36:1:(1): after print: --backtrace update-- 

This happened at Wed May  4 16:13:43 PDT 2005


###### 2. 
A reference to [minors, First] is generated in the documentation from the code. 
Wed May  4 17:06:00 PDT 2005

###### 3. clearAll results in errors; e.g.,
i16 : clearAll
stdio:16:1:(1): after eval: expected a list, hash table, or sequence
stdio:16:1:(1): before print: --backtrace update-- 
stdio:16:1:(1): at print: --backtrace update-- 

o16 : 
stdio:16:1:(1): after print: --backtrace update-- 

i17 : 

Probably this is related to the error in listUserSymbols.
Thu May  5 09:08:13 PDT 2005

###### 4. 
The key Undocumented in the document file generates some error. This should
be fixed for the documentation of char.

(This is an eternal bug; the time stamp doesn't count, hence no stamp.)

###### 5. 
gb(..., DegreeLimit =>) doesn't seem to work. The following example is from the documentation of Macaulay2 > Gröbner bases > computing Gröbner bases:

AFTERTHOUGHT: I now think that the bug report is false, but I am leaving it
here since Mike did say that there is some problem with DegreeLimit. I don't
know what it is. 

i5 : R = ZZ/101[x,y,z,w];

i6 : I = ideal(x*y-z^2,y^2-w^2);
o6 : Ideal of R

i7 : g1 = gb(I)

o7 = GroebnerBasis[status: done; S-pairs encountered up to degree 4]

o7 : GroebnerBasis

i8 : g2 = gb(I,DegreeLimit => 2)

o8 = GroebnerBasis[status: DegreeLimit; all S-pairs handled up to degree 4]

o8 : GroebnerBasis

i9 : g3 = gb(I,DegreeLimit => 3);

i10 : gens g1

o10 = | y2-w2 xy-z2 yz2-xw2 z4-x2w2 |

              1       4
o10 : Matrix R  <--- R

i11 : gens g2

o11 = | y2-w2 xy-z2 yz2-xw2 z4-x2w2 |

              1       4
o11 : Matrix R  <--- R

i12 : gens g3

o12 = | y2-w2 xy-z2 yz2-xw2 z4-x2w2 |

              1       4
o12 : Matrix R  <--- R

This bug, unlike the previous one, was found at
Sat May  7 12:10:27 PDT 2005

The AFTERTHOUGHT was added
Sat May  7 15:00:19 PDT 2005

###### 6. 
Symbols are not initialised after a clearAll. See Example below:

i66 : restart
Macaulay 2, version 0.9.5
--package "Core" loaded
--beginDocumentation: using documentation database, skipping the rest of /a/porky/home/dan/local/share/Macaulay2/PrimaryDecomposition.m2
--package "PrimaryDecomposition" loaded
--beginDocumentation: using documentation database, skipping the rest of /u/m2fest3/.Macaulay2/local/share/Macaulay2/Macaulay2.m2
--package "Macaulay2" loaded
--making index of installed packages in /u/m2fest3/.Macaulay2/index.html

i1 : R = ZZ/1277[x..z];

i2 : I = ideal(x*y+y*z, y^2, x^2);

o2 : Ideal of R

i3 : clearAll 

i4 : R = ZZ/1277[x..z];

i5 : I = ideal(x*y+y*z, y^2, x^2);
stdio:5:12:(1): no method for binary operator * applied to objects:
--            x (of class Symbol)
--      *     y (of class Symbol)

i6 : y

o6 = y

o6 : Symbol

i7 : x

o7 = x

o7 : Symbol

i8 : z

o8 = z

o8 : Symbol

i9 : 

After this if I restart M2, everything works okay.

Filed at Sat May  7 15:09:39 PDT 2005

###### 7. 
StopBeforeComputation doesn't seem to work; see:

i1 : R = ZZ/1277[x..z];

i2 : I = ideal(x*y+y*z, y^2, x^2);

o2 : Ideal of R

i3 : g = gb(I, StopBeforeComputation => true)

o3 = GroebnerBasis[status: done; S-pairs encountered up to degree 3]

o3 : GroebnerBasis

i4 : gens g

o4 = | y2 xy+yz x2 yz2 |

             1       4
o4 : Matrix R  <--- R

Mike commented here that he suspected that M2 doesn't pass the options
completely into the engine (or something like it).

Filed at Sat May  7 15:33:05 PDT 2005
