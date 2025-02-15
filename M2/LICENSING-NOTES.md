# Macaulay2 Licensing Notes

**Macaulay2**

by Daniel R. Grayson <dan@math.uiuc.edu>  
and Michael E. Stillman <mike@math.cornell.edu>

available from https://macaulay2.com/

We thank the United States National Science Foundation for generous funding
to develop Macaulay2.

Macaulay2 is copyright 1993-2025 by the Macaulay2 Authors (see below)

We permit you to use the source code under the terms of the GNU General Public
License (GPL), version 2 or later, as published by the Free Software Foundation;
see the files [Macaulay2/COPYING-GPL-2](Macaulay2/COPYING-GPL-2) and 
[Macaulay2/COPYING-GPL-3](Macaulay2/COPYING-GPL-3).  The packages contributed
by various authors, located in the directory Macaulay2/packages/, come with
separate licenses.  The binary distributions of Macaulay2 are licensed under GPL
version 3.

This is free software.  There is no warranty; not even for merchantability or
fitness for a particular purpose.

Here we document briefly the licenses on the various libraries.

Libraries that may be linked with M2:

|                    |                                                                              |                                       |
| ------------------ | ---------------------------------------------------------------------------- | ------------------------------------- |
| -lblas             | [BLAS](https://www.netlib.org/blas/)                                         | implementation specific               |
| -lboost_regex      | [Boost.Regex](https://www.boost.org/)                                        | BSL-1.0                               |
| -lboost_stacktrace | [Boost.Stacktrace](https://www.boost.org/)                                   | BSL-1.0                               |
| -lfactory          | [Singular-Factory](https://www.singular.uni-kl.de/)                          | GPL-2.0-or-later                      |
| -lffi              | [libffi](https://sourceware.org/libffi/)                                     | MIT                                   |
| -lflint            | [FLINT](https://flintlib.org/)                                               | LGPL-3.0-or-later                     |
| -lfrobby           | [frobby](https://github.com/Macaulay2/frobby)                                | GPL-2.0-or-later                      |
| -lgc               | [Boehm gc](https://www.hboehm.info/gc/)                                      | Boehm-GC                              |
| -lgdbm             | [GDBM](https://www.gnu.org/software/gdbm/)                                   | GPL-3.0-or-later                      |
| -lgivaro           | [Givaro](https://casys.gricad-pages.univ-grenoble-alpes.fr/givaro/)          | CECILL-B                              |
| -lgmp              | [GMP](https://gmplib.org/)                                                   | LGPL-3.0-or-later OR GPL-2.0-or-later |
| -llapack           | [LAPACK](https://www.netlib.org/lapack/)                                     | implementation specific               |
| -lmathic           | [mathic](https://github.com/Macaulay2/mathic)                                | LGPL-2.0-or-later                     |
| -lmathicgb         | [mathicgb](https://github.com/Macaulay2/mathicgb)                            | GPL-2.0-or-later                      |
| -lmemtailor        | [memtailor](https://github.com/Macaulay2/memtailor)                          | BSD-3-Clause                          |
| -lmpfi             | [MPFI](https://gitlab.inria.fr/mpfi/mpfi)                                    | LGPL-2.1-or-later                     |
| -lmpfr             | [MPFR](https://www.mpfr.org/)                                                | LGPL-3.0-or-later                     |
| -lmps              | [MPSolve](https://numpi.dm.unipi.it/scientific-computing-libraries/mpsolve/) | GPL-3.0-or-later                      |
| -lnormaliz         | [Normaliz](https://www.normaliz.uni-osnabrueck.de/)                          | GPL-3.0-or-later                      |
| -lntl              | [NTL](https://libntl.org/)                                                   | LGPL-2.1-or-later                     |
| -lpython3          | [Python](https://www.python.org/)                                            | PSF-2.0                               |
| -lreadline         | [Readline](https://www.gnu.org/software/readline)                            | GPL-3.0-or-later                      |
| -ltbb              | [oneTBB](https://uxlfoundation.github.io/oneTBB/)                            | Apache-2.0                            |

Programs we may include with our distribution:
|                                                                       |                  |
| --------------------------------------------------------------------- | ---------------- |
| [4ti2](https://4ti2.github.io/)                                       | GPL-2.0-or-later |
| [cohomCalg](https://github.com/BenjaminJurke/cohomCalg)               | GPL-3.0-or-later |
| [CSDP](https://github.com/coin-or/Csdp)                               | EPL-2.0          |
| [gfan](https://users-math.au.dk/jensen/software/gfan/gfan.html)       | GPL-2.0-or-later |
| [lrslib](https://cgm.cs.mcgill.ca/~avis/C/lrs.html)                   | GPL-2.0-or-later |
| [msolve](https://msolve.lip6.fr/)                                     | GPL-2.0-or-later |
| [nauty](https://pallini.di.uniroma1.it/)                              | Apache-2.0       |
| [TOPCOM](https://www.wm.uni-bayreuth.de/de/team/rambau_joerg/TOPCOM/) | GPL-2.0-or-later |


Libraries linked with some of our programs:
|        |                                            |                  |
| ------ | ------------------------------------------ | ---------------- |
| -lcdd  | [cddlib](https://github.com/cddlib/cddlib) | GPL-2.0-or-later |
| -lglpk | [GLPK](https://www.gnu.org/software/glpk/) | GPL-3.0-or-later |

Macaulay2 binaries are licensed under GPL-3.0 due to linking with LGPL-3.0 libraries (FLINT, MPFR).
See https://www.gnu.org/licenses/gpl-faq.html#AllCompatibility

## Macaulay2 Authors
Dan Grayson  
Mike Stillman  
David Eisenbud

Dave Barton  
Guillem Blanco  
Charles Boyd
Michael Burr  
Edward Carter  
Frédéric Chapoton  
Justin Chen  
Eliana Duarte  
Timothy Duff  
Bill Furnish  
Florian Geiss  
Marc Harkonen  
Daoji Huang  
Franziska Hinkelmann  
Jerry James  
Felix Janda  
Thomas Kahle  
Lars Kastner  
Jakob Kröker  
Anton Leykin  
Frank Moore  
Dylan Peifer  
Brian Pike  
Mahrud Sayrafi  
Karl Schwede  
Lily Silverstein  
Jieao Song  
Zach Teitler  
Doug Torrance  
Jay Yang  
Mikhail V. Zinin  
Paul Zinn-Justin  
Radoslav Zlatev

See also the many [authors of Macaulay2 packages](https://www.macaulay2.com/doc/Macaulay2/share/doc/Macaulay2/Macaulay2Doc/html/_authors_spof_sp__Macaulay2_sppackages.html).
