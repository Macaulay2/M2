Macaulay2 on MacOS X

	    by Daniel R. Grayson <dan@math.uiuc.edu>
	 and Michael E. Stillman <mike@math.cornell.edu>

	available from http://www.math.uiuc.edu/Macaulay2/

Welcome to Macaulay2!

This file tells how to get Macaulay2 installed and running on your MacOS X
10.7 or 10.8 system.  If you have any difficulty with these instructions,
please email us.

Step 1.  Download Macaulay2.

In your browser, download the file

  http://www.math.uiuc.edu/Macaulay2/Downloads/MacOSX/Macaulay2-1.6-x86_64-MacOS-10.8.dmg

If there is a later version, use that instead. This should work fine under
MacOSX version 10.7.

If you are running an older version of MacOSX, then we recommend either
compiling Macaulay2 yourself, or asking on our Macaulay2 google group to see if
someone else has created a distribution for your operating system.

Step 2. Important step: Unquarantine the disk image file!!!

First unmount the disk image, if Safari or Firefox mounted it for you.  

Start the Terminal application (located in the Utilities folder in the
Application folder).  In the Terminal window, type: (Assuming that you have
placed the .dmg file on your desktop):

	cd ~/Desktop
	xattr -d com.apple.quarantine Macaulay2*.dmg

(If the system responds "No such xattr: com.apple.quarantine", then the file
had not been quarantined, and everything should be fine.)  The reason for this
step: if you do not do it, then the system will ask you, every time you open a
Macaulay2 help page, whether it is safe to do so.  This quickly gets old!

Step 3. Unpack the disk image

Mount this disk image by double-clicking on it, and drag the Macaulay2 folder
in this mounted disk to somewhere on your disk. One possible location is your
Applications folder (your system Applications folder, at top level on your main
disk).

Step 4. Installation

Arrange for Macaulay 2 to set up your .emacs files and your command shell init
files so that M2 will be on the path.

The easiest way to set up these files is to do the following in a Terminal
window (the Terminal application is found in the Utilities folder inside the
Applications folder.  You might want to drag this application to your dock
too).

First change to the Macaulay2 folder (here we suppose that it is in your
Applications folder), and then run the 'setup' routine in Macaulay2:

	cd /Applications/Macaulay2-1.6
	cd bin
	./M2
	  setup()  -- this line and the next are typed inside of 
	  exit     -- Macaulay2: answer the prompts which follow.

After you log out and in again, your PATH will have Macaulay2's bin directory
on it.  The 'setup' routine will modify (some of) your shell command init files
(such as .profile, .bashrc, .login, .cshrc), and your .emacs file.  The
existing files are backed up first.  It also writes a file .profile-Macaulay2
in your home directory which does the actual work including putting M2 on your
path.  Once again, a backup is created first, if the file already exists.

This allows you to run Macaulay2 by typing 'M2' (without the quotes) at a
terminal command line, to access the info and man pages, and to use the
Macaulay2 emacs interface.

Step 5. Try M2 in a terminal window

In a Terminal window, type 'M2' (without the quotes).  If Macaulay2 has been
installed properly, and your .profile startup file has been correctly modified,
then you should see a prompt such as this:

	bash% M2
	Macaulay 2, version 1.6
	with packages: ConwayPolynomials, Elimination, IntegralClosure, LLLBases, 
         PrimaryDecomposition, ReesAlgebra, TangentCone

	i1 : 

At this point you should try something simple in Macaulay2, such as

	printWidth = 60
	R = QQ[a..d]
	(a+b+c+d)^4

Now type

	viewHelp

This starts your default web browser (if it is not already running), and then
opens the main Macaulay2 documentation page index.html file mentioned below.
We suggest that you bookmark this page.  By the way, to get the html help on a
topic, such as "ideals", use

	viewHelp "ideals"

To exit Macaulay2, type one of: exit, end, or quit.

	exit

The Macaulay2 application directory (folder)

If this is the first time that you have run a recent version of Macaulay2 from
your user account, Macaulay2 creates an "application folder" in your home:

	Library/Application Support/Macaulay2

The application folder contains several useful files and
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

After creating this directory, Macaulay2 never modifies init.m2 or the code
directory.  The page index.html is rewritten each time M2 is started.

The directory "local" is used by Macaulay2 to manage installed packages: you
should mostly ignore this directory.  If you are upgrading from previous
versions of Macaulay2, you may wish to delete the folder: local.

Under MacOSX 10.7 and higher, Apple has decided to make the Library folder
invisible.  There are several ways to get into this folder.  One way is to type
(or, better, cut and paste) the following into a Terminal window:

	cd ~/Library/Application\ Support/Macaulay2
	open .

This opens the Macaulay2 support folder in the finder.

Step 6. Enjoy!

You should now be up and running.  From the bookmarked web page, choose
'Macaulay2', and then 'getting started', for a first Macaulay2 session, as well
as how to use the Macaulay2 emacs interface.

If you have any questions, or problems, please contact one of us, or post a
message at our google groups site:

	http://groups.google.com/group/macaulay2

You may need to join the group in order to post messages, but this is easy:
click on 'Join this group' found on the right-hand side of the page.

Step 7.  Some suggestions to streamline your Macintosh for use with Macaulay2.

There are at least three ways to run emacs on the mac: using Aquamacs (the
recommended way), in a terminal window, and under the X11 window system.

In all of these case, the function keys F11 and F12 are ones that Macaulay2
likes to use.  Unfortunately, the system has grabbed these.  Fortunately, it is
easy to get them back:

In System Preferences, change the keys for dashboard and expose, so that the
functions keys F11 and F12 are not used. (Then you can use F12 in emacs to
start Macaulay2, and F11 to send lines from a file which ends in ".m2" to
Macaulay2.)

Aquamacs (recommended)

Aquamacs is a MacOSX implementation of emacs, which is designed to play well
with other applications and MacOSX concepts: drag and drop, cut and paste all
work.  Download the latest version of Aquamacs at

  http://aquamacs.org/download.shtml.  

It is quite nice to use these days.  After downloading it and moving it to your
Applications folder, it is ready to run Macaulay2.

I like smaller fonts, personally.  Changing the fonts is fairly self
explanatory: Under the menu item "Options", choose "Appearance", and then set
the font to a fixed-width font.  Then choose "Save Options" under the "Options"
menu.

Emacs in a terminal window.

The version of emacs which comes with MacOSX only runs inside a terminal
window.  The function keys generally work correctly, but you must know many of
the emacs keystrokes, since the mouse button is not enabled in this version of
emacs.

It is suggested that you change your "option key" to a "meta key": In Terminal,
open the Preferences menu item, click "Settings", choose "Keyboard", and click
the box to use the option key as a meta key.

