
			       Macaulay2

Macaulay2 is a system for computing in commutative algebra, algebraic geometry
and related fields.  The system was originally written by Dan Grayson and Mike
Stillman.  David Eisenbud joined the project a number of years ago, and many
users are writing packages for the system, and some are contributing source
code.  See our web page 

  http://macaulay2.com/

for more details and for downloading binary releases.

The source code is available at

   https://github.com/Macaulay2/M2.

One uses the program "git" to access it.  For brief instructions about how to
use git, see

  https://github.com/Macaulay2/M2/wiki/Git-for-Macaulay2-Contributors .

Report bugs via the github issue tracker at

  http://github.com/Macaulay2/M2/.  

The directory containing this file is the top level directory of the directory
tree that contains the following subdirectories:

    M2	     The subdirectory "M2" contains everything needed by a user to
	     build Macaulay2, and the file "M2/INSTALL" gives instructions for
	     doing so.

    bugs     The subdirectory "bugs" is where we keep track of older bug 
    	     reports.

-----------------------------------------------------------------------------

Contributions to the code of Macaulay2 are welcome.

  To make a contribution, submit a "pull request" on github.  If the
  contribution involves changing an existing package in a non-trivial way, we
  will normally contact the authors to get their approval of the change.  If a
  new package with mathematical content is submitted, it will normally be
  accepted if it can be installed with "installPackage" and the tests pass as
  determined by "check", in the latest version of Macaulay2.

  To start working on an existing github "issue", volunteer to work on it, so
  you can get "assigned" to the issue, thereby preventing duplication of
  effort.

  To report a bug, submit an "issue" on github.
