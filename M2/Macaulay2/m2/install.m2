installFile = replace_("@VERSION@", version#"VERSION") ///
			   Macaulay 2
	    by Daniel R. Grayson <dan@math.uiuc.edu>
	 and Michael E. Stillman <mike@math.cornell.edu>

	available from http://www.math.uiuc.edu/Macaulay2/

Welcome to Macaulay2!

This file tells how to install Macaulay 2 from a binary distribution on unix
systems such as linux, or under Microsoft Windows.  If you have any difficulty
with these instructions (which are not complete yet), please email us.

---------------------------------------
-- Strengthen your operating system ---
---------------------------------------

If you are running under Microsoft Windows, you should first make your
operating system look as much as possible like a unix operating system.  The
way to do this is to install cygwin, available from http://cygwin.com/.  Be
sure to install X11 and emacs.  The rest of the instructions should be followed
in a cygwin bash command shell.

---------------------------------------
-- Download and unpack Macaulay2 ------
---------------------------------------

In your browser, visit the web page at

     http://www.math.uiuc.edu/Macaulay2/Downloads/

and download the appropriate file for your architecture and operating system.
Details about the file naming conventions are visible there.  Typical names
include these.
  
     Macaulay2-@VERSION@-i686-Linux.tgz
     Macaulay2-@VERSION@-i686-CYGWIN_NT-5.0.tgz

If there is a later version than @VERSION@, use that instead. Unpack this
gzipped tar file as usual, in whatever directory is convenient, say in
/foo/bar, as follows.

     cd /foo/bar
     tar zxfp Macaulay2-@VERSION@-Linux.tar.gz

You will see that a directory tree rooted at /foo/bar/Macaulay2-@VERSION@ has
been created.  The subdirectories and files are organized in a way that mimics
the usual unix tree of subdirectories rooted at /usr.  For example, there are
subdirectories called bin, info, lib, and share.  There are also some extra
files called: INSTALL (this file), encapinfo, postinstall, and preremove.

-------------------------------------------------
-- Install method for Users ---------------------
-------------------------------------------------

   Arrange for Macaulay 2 to set up your .emacs files and your command shell
   init files so that M2 will be on the path.

        cd /foo/bar/Macaulay2-@VERSION@
	cd bin
	./M2 -e 'setup();exit 0' 

   After you log in again, your PATH will have Macaulay2's bin directory on it.

   The difference between M2-load-libs and M2 is that the former is a shell
   script that arranges for shared libraries that may be needed to be found.

-------------------------------------------------
-- Install method for System administrators -----
-------------------------------------------------

   The easiest and best way to install Macaulay2 is with "epkg", available from

   	http://encap.cso.uiuc.edu/epkg/

   The files encapinfo, preremove, and postinstall in the top level directory
   or init files for that program, whose purpose is to put appropriate
   symbolic links from locations under /usr or /usr/local to the files in 
   /foo/bar/Macaulay2-@VERSION@.  If you do this, then Macaulay2's files can
   be found without modifying any paths.  Otherwise, each user will have to
   run our "setup()" command, as described above, to set the paths correctly.
   The paths we have in mind are the environment variables "LD_LIBRARY_PATH",
   "INFOPATH" (used by "emacs" and "info"), and "PATH", as well as the emacs 
   variable "load-path".  Moreover, an installation made with "epkg" can be
   reliably removed.  If you manually remove the files without asking
   "epkg" to remove the symbolic links, the only thing left behind will be
   symbolic links pointing nowhwere, which take up little space on the disk and
   could be easily found and removed manually.
   
   Another thing "epkg" does is to run the "postinstall" script we provide,
   which runs "dumpdata" (if implemented on your system) and puts directory
   entries for Macaulay2 and each of its packages into the master info dir
   file, making the Macaulay2 documentation visible to the user who uses
   "info", stand-alone, or in emacs.
   
   However, if you don't have "epkg", you can install Macaulay2 directly,
   as follows.  Warning: this direct approach is not as good, because it
   may wipe out the previous version of Macaulay 2, and because there is
   no reliable way to remove all the files you've installed.

   Copy the contents of the subdirectories of /foo/bar/Macaulay2-@VERSION@ into
   /usr or /usr/local/.  If you have gnu "cp", the following command will do
   it:

      cd /foo/bar/Macaulay2-@VERSION@
      cp -ai bin info lib share /usr/local

   The "i" option to "cp" will prompt you before clobbering a file in the
   target directory.  The reason not to use the command

      cp -ai . /usr/local

   is to avoid copying the extra files, including this one.

   On some systems Macaulay2 comes with shared libraries, in the "lib"
   subdirectory, so if you want to change /usr/local to something nonstandard,
   M2 may not be able to find its shared libraries unless you add
   /foo/bar/Macaulay2-@VERSION@/lib to the search path.  Under linux, that
   would involve editing /etc/ld.so.conf.

-------------------------------------------------
-- Dumping data ---------------------------------
-------------------------------------------------
   
   Here is an additional step that will speed later loading of the program on
   linux systems and solaris systems.  (Mac OS X is sufficiently different from
   Unix that we haven't been able to implement dumpdata under it yet.)

   Issue the following command:
   
   	M2 --dumpdata
	
   It will save a file with a name of the form
   
        lib/Macaulay2/Core/cache/Macaulay2-*-data
   
   in the directory tree where you have installed it.  It contains the result
   of interpreting the source code written in the Macaulay 2 language.  The next
   time you start M2 it will try to load the file.  On some systems such loading
   is rarely successful.  In that case, just delete the saved data file to
   eliminate the warning messages about failing to load it.

-------------------------------------------------
-- For Users, after installation ----------------
-------------------------------------------------

On a command line, type 'M2' (without the quotes).  If
Macaulay2 has been installed properly, then you should see a prompt
such as this:

	% M2
	Macaulay 2, version @VERSION@

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

	init.m2		This file is run every time you start Macaulay2
	code/		This directory is on your Macaulay2 path, 
			so any .m2 files you put here are easy 
			to load in Macaulay2.
	index.html	This is the top page for the Macaulay2 html 
			documentation. It includes the Macaulay2 html
			distribution, together with any html files from
			installed Macaulay2 packages.
        encap/	   	A directory containing one subdirectory for
	     	        each installed package.  The subdirectory
			houses the files for that package.
        local/	   	A directory tree containing symbolic links
	     	        to the files of each installed package.
	local/info/	A directory with links to the info files
	     	        for each installed package.  You may wish
			to add this directory to the list of
			directories in your environment variable
			INFOPATH.

After creating this directory, Macaulay2 never modifies init.m2 or the
code directory.  The page index.html is rewritten each time M2 is
started.

Finally, Enjoy!

You should now be up and running.  From the bookmarked web page,
choose 'Macaulay2', and then 'getting started', for a first Macaulay2
session, as well as how to use the Macaulay2 emacs interface.

If you have any questions, or problems, please contact one of us, or
post a message at our sourceforge web site:

	http://sourceforge.net/projects/macaulay2

You may need to create a sourceforge user id in order to post
messages, but this is easy: choose the 'create user account' found
near the top of the page.
///
