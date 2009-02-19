			   Macaulay 2
	    by Daniel R. Grayson <dan@math.uiuc.edu>
	 and Michael Stillman <mike@math.cornell.edu>

	available from http://www.math.uiuc.edu/Macaulay2/

	Fink Macaulay2 package: Dave Morrison <drm@finkproject.org>

Welcome to Macaulay2!

This file tells how to install Macaulay 2 using fink on Mac OS X (10.4 or 10.5)
on either power pc or intel processors. If you have any difficulty with these
instructions (which are not complete yet), please email us.

If you are running on 10.3 or 10.2, the older version Macaulay2 0.9.2
is still available under fink, but the instructions here only apply to
newer versions of Macaulay 2.

---------------------------------------
-- Strengthen your operating system ---
---------------------------------------

    You should first install the Fink system
    <http://www.finkproject.org>.  Follow the instructions at the Fink
    website to install Fink itself. You can choose the binary
    installation method, or the source
    installation method (which will download the source files and compile
    them on your machine).  You may also wish to install emacs using Fink,
    for the best interface to Macaulay2.

    Fink can also be used to install other software of interest to
    mathematicians.  In most cases, you simply install the relevant Fink
    package.

    At present, Macaulay2 is only up-to-date in Fink's "unstable" tree
    (which might be more accurately called the "testing" tree).  To
    enable this tree after installing Fink, edit the file /sw/etc/fink.conf
    and append "unstable/main unstable/crypto" to the "Trees:" line.
    Then run "fink index; fink selfupdate" to activate the unstable tree.

-------------------------------------------------
-- Installation under fink ----------------------
-------------------------------------------------

    Once fink is installed, install the package 'macaulay2',

        fink install macaulay2

    You may also use Fink Commander to install Macaulay2.


   Arrange for Macaulay 2 to set up your .emacs file so that the Macaulay2 emacs
   interface will be functional.  Using emacs is the recommended way to work
   with Macaulay2.
   
        cd /sw/bin
	./M2
	  setupEmacs()  -- and answer the questions.  
                        -- Answering "y" to all is usually fine
          exit

   If you are running emacs, you will need to exit and restart it before 
   these changes take effect.

-------------------------------------------------
-- For Users, after installation ----------------
-------------------------------------------------

On a command line, type 'M2' (without the quotes).  If
Macaulay2 has been installed properly, then you should see a prompt
such as this:

	% M2
	Macaulay 2, version 1.2
	with packages: Elimination, IntegralClosure, LLLBases, 
          PrimaryDecomposition, ReesAlgebra, SchurRings, TangentCone
	i1 : 

At this point you should try something simple in Macaulay2, such as

   	100!
	R = QQ[a..d]
	(a+b+c+d)^4
	C = res coker vars R
	C.dd

Now type

	viewHelp

This starts your default web browser (if it is not already running),
and then opens the main Macaulay2 documentation page index.html file
mentioned below.  We suggest that you bookmark this page.  By the way,
to get the html help on a topic, such as "ideals", use

	viewHelp "ideals"

To exit Macaulay2, type one of: exit, end, quit, or the end of file character,
which is usually set to CTRL-D.

-------------------------------------------------
-- The Macaulay2 application directory ----------
-------------------------------------------------

If this is the first time that you have run a recent version of
Macaulay2 from your user account, Macaulay2 creates an "application
directory" for Macaulay2 in your home directory.  Under unix, that
directory is named ".Macaulay2", and under Mac OS X, it is
called "Library/Application Support/Macaulay2".

The application directory contains several useful files and
directories:

	init.m2	This file is run every time you start Macaulay2
	code/		This directory is on your Macaulay2 path, 
			so any .m2 files you put here are easy 
			to load in Macaulay2.
	index.html	This is the top page for the Macaulay2 html 
			documentation. It includes the Macaulay2 html
			distribution, together with any html files from
			installed Macaulay2 packages.
	local/		A directory tree containing installed packages.

After creating this directory, Macaulay2 never modifies init.m2 or the
code directory.  The page index.html is rewritten each time M2 is
started.

Finally, Enjoy!

You should now be up and running.  From the bookmarked web page,
choose 'Macaulay2', and then 'getting started', for a first Macaulay2
session, as well as how to use the Macaulay2 emacs interface.

If you have any questions, or problems, please contact one of us, or post a
message at our google groups site:

	http://groups.google.com/group/macaulay2

You may need to join the group in order to post messages, but this is easy:
click on 'Join this group' found on the right-hand side of the page.

