installFile = ///
			   Macaulay 2
	    by Daniel R. Grayson <dan@math.uiuc.edu>
	 and Michael E. Stillman <mike@math.cornell.edu>

	available from http://www.math.uiuc.edu/Macaulay2/

Welcome to Macaulay2!

This file tells how to install Macaulay 2 from a binary distribution
on linux or other unix systems.  This is a first draft so if you
have any difficulty with these instructions, please email us.

---------------------------------------
-- Download and unpack Macaulay2 ------
---------------------------------------

In your browser, download the file

  http://www.math.uiuc.edu/Macaulay2/ftp-site/Macaulay2-0.9.8-Linux.tar.gz

If there is a later version, use that instead. Unpack this gzipped tar
file as usual, in whatever directory is convenient:

	tar zxf Macaulay2-0.9.8-Linux.tar.gz

-------------------------------------------------
-- Install method #1 for System administrators --
-------------------------------------------------

1. After unpacking this tarfile, rename (or link) the directory name 
    to Macaulay2.
2. Make a symbolic link from somewhere on the path to Macaulay2/bin/M2
3. In the "For users" section below, change the "/usr/local" path
    to wherever you have placed the Macaulay2 directory.
4. Give your users the info in the "For users" section below.

-------------------------------------------------
-- Install method #2 for System administrators --
-------------------------------------------------

1. After unpacking this tarfile, unpack into /usr/local.
2. In the "For users" section below, change the "/usr/local" path
    to wherever you have placed the Macaulay2 directory.
3. Give your users the info in the "For users" section below.

------------------------------------------------
-- Install method for Users --------------------
------------------------------------------------

The easiest way is to follow the method #1 above.  You can unpack
Macaulay2 into some directory in your own account.  Suppose that 
directory is ~/foo.  You will need a bin directory on your path (say
~/bin).  Then make a symbolic link:
  ln -s ~/foo/Macaulay2/bin/M2 ~/bin/
After that, continue with the "For Users" section below.

---------------------------------------------------------------------
-- For Users --------------------------------------------------------
-- Give the following info to each user who plans to use Macaulay2 --
---------------------------------------------------------------------

First, make sure your Macaulay2 emacs interface is set up.
Place the following lines into your .emacs file in your home
directory.  If this file doesn't exist, create it.

	(setq load-path 
	      (append
	       '( "/usr/local/Macaulay2/share/emacs/site-lisp/" )
	       load-path
	       ))

	(load "M2-init.el" t)

	; comment out the following line with an initial 
	; semicolon if you want to use your f12 key for 
	; something else
	(global-set-key [ f12 ] 'M2)

Second, on a command line, type 'M2' (without the quotes).  If
Macaulay2 has been installed properly, then you should see a prompt
such as this:

	% M2
	Macaulay 2, version 0.9.8

	i1 : 

Now type

	viewHelp

This starts your default web browser (if it is not already running),
and then opens the main Macaulay2 documentation page index.html file
mentioned below.  We suggest that you bookmark this page.  By the way,
to get the html help on a topic, such as "ideals", use

	viewHelp "ideals"

At this point you should try something simple in Macaulay2, such as

	R = QQ[a..d]
	(a+b+c+d)^4

To exit Macaulay2, type one of: exit, end, or quit.

	exit

The Macaulay2 application directory

If this is the first time that you have run a recent version of
Macaulay2 from your user account, Macaulay2 creates an "application
directory" for Macaulay2 in your home directory.  Under unix, that
directory is named ".Macaulay2", and under Mac OS X, it is
called "Library/Application Support/Macaulay2".)

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
