			Installation of Macaulay 2

----------------------
1. Introduction
----------------------

Macaulay 2 is a software system for algebraic geometry research, written by
Daniel R. Grayson and Michael E. Stillman with support of the National
Science Foundation.

----------------------
2. Installation
----------------------

If you are installing Macaulay 2 on a Windows machine, see the special
instructions in a later section of this document.

You have downloaded an unpacked a file with a name something like

     Macaulay2-0.8.46-i586-Linux-2.1.121.tar.gz

Untarring this file created a directory called Macaulay2, and in that
directory you will find a shell script called 'setup' or, under Windows, an
executable file called 'setup.exe'.  The first step is to run this program.

Run this command:

	setup

or
	./setup

if it's unix and '.' is not first on your path.  Then copy the script bin/M2
(or bin/M2.bat if it's Windows) somewhere on your path (under Unix we suggest
/usr/local/bin).  (The script contains the absolute path of the current
directory.)  Now issue the command

	M2

to start the program.

    NOTE: On some types of computers Macaulay 2 will take extra time to
    start up: it will print "loading source code" and load the contents of
    about 100 files containing code written in the Macaulay 2 language.  On
    other types of computers, this process is replaced by one we call
    "loaddata", which is much quicker because it involves mapping into
    memory a large file containing an image of the data segment of the
    program saved with "dumpdata" by the "setup" script mentioned above.

To see if Macaulay 2 works, make a ring with

	R = ZZ/101[x,y,z]

and then make a resolution with

	res coker vars R

Here's what you should get:

	      1      3      3      1
	o2 = R  <-- R  <-- R  <-- R

	     0      1      2      3

If, at some later time, you wish to move the directory tree, just run setup
again and install the new bin/M2 it creates on your path.

One of the things that running 'setup' may do is to create a file called
bin/Macaulay2-XXXX.data, where XXXX is the name of your machine's
architecture.  To see what your machine's architecture is, run the command
'uname -m'.  This way of naming that file is intended for Unix environments
where machines of different architectures must coexist.  That file and the
binary executable file bin/Macaulay2.exe or bin/Macaulay2 are the only
architecture dependent files: the rest may be shared.  On Sun machines, the
differences between architectures sun4d and sun4c are slight enough that the
binary file bin/Macaulay2 will work on both, but the bin/Macaulay2-XXXX.data
files will differ.  If you have both of these types of machines, you can run
'setup' on each of these in the same directory tree.

Unless you are using the Windows version, make sure you also download and
install the manual.  It is kept in a separate file at our web site, the name
of which is something like this:

	Macaulay2-0.8.46-doc.tar.gz

Unpack it in the same directory.

The directions above are intended mainly for system administrators.  The next
thing that individual users should do is to read the file emacs/README.  On
all systems except the Macintosh, the best way to interact with Macaulay 2 is
through an emacs macro package we provide.

----------------------
3. The manual
----------------------

The manual is available in various forms, and includes tutorials as well as
descriptions of each function.  Its constituent files are provided in a
separate tar file at our web site, as mentioned above.

A. While running Macaulay 2.

    While running Macaulay2, on-line help is available on 
    predefined functions and types, and other topics.  For example,
    'help Matrix' gives information about matrices, and
    'help matrix' describes the 'matrix' command.

    Also useful is the 'apropos' command: 'apropos "Matrix"' will return
    a list containing all identifiers containing the string "Matrix".
    One caveat: it is case sensititive, so that for example the 'matrix'
    routine will not appear on the list.

B. With a web browser

    The directory 'html' contains the documentation in HTML form.  Start
    at the file 'html/index.html' using your favorite web browser, such as
    Netscape or Internet Explorer.

C. In book form

    The directory 'book' contains the code for creating the book form of
    the documentation, in TeX.  But if you don't have gnu make you might
    want to just down load the dvi files for the book from the web site -
    the file is M2book.dvi, or M2hbook.dvi if you want the HyperTeX
    version viewable with xhdvi.

----------------------
4. Other documentation
----------------------

The file emacs/emacs.hlp gives a brief tutorial introduction to the use of
emacs with Macaulay 2, and includes some lines of emacs code to put into your
.emacs file.  One nice feature of emacs' Macaulay 2 mode is command
completion.

See also emacs/emacs.m2, for information about editing Macaulay 2 source files.

Read the file CHANGES to see what changes have been made since the last
version you used.

-------------------------
5. Licenses and copyright
-------------------------

See the file licenses/README for information about copyright and licenses for
the program, its code, and the libraries it uses.

----------------------
6. Distribution
----------------------

Take a look at the home page for Macaulay 2

	http://www.math.uiuc.edu/Macaulay2

to download the source code, versions of Macaulay 2 for various machines, or
the documentation.

----------------------
7. Other help
----------------------

Send email to <Macaulay2@math.uiuc.edu> if you get stuck.  Email to that
address will get forwarded to both of us.

	Dan Grayson <dan@math.uiuc.edu>
	Mike Stillman <mike@math.cornell.edu>

------------------------------------------------------------------
8. Notes on Windows 95/NT installation
------------------------------------------------------------------

    You will have downloaded a file with a name like this:

              Macaulay2-0.8.46-i586-Windows-95-98-NT.zip

    This file contains the files you need to run the program, as well as the
    html documentation files.  If you want the dvi file containing the book
    form of the documentation, you'll have to download that separately.

    To unpack it you will need a program like WinZip or PKZIP for Windows.
    You can get WinZip from http://www.winzip.com, and you can get PKZIP from
    http://www.pkware.com.  It is important not to use the older PKUNZIP
    program for MSDOS, because it will truncate filenames to 8+3 characters.
    Be careful when using these programs - they tend to have an option for
    ignoring the path names we provide, in which case they will dump all of
    the files into a single directory.  For WinZip, make sure the check box
    "Use Folder Names" is checked.  Check that you obtain files in
    directories with names including the following: Macaulay2/m2,
    Macaulay2/bin, Macaulay2/tutorial, Macaulay2/html, Macaulay2/cache, and
    Macaulay2/emacs.

    After unpacking the file, you must follow the instructions in the
    installation section above.  In particular, you will run the 'setup'
    program.  The source code is provided in the file Macaulay2/util/setup.c
    - it's pretty simple.  It produces a script called 'M2.bat' which you can
    use with Windows or in an MS-DOS window.

    We are currently using the Microsoft Visual C/C++ compiler, running under
    Windows NT, to compile Macaulay 2 as a 32 bit application.  It should
    work equally well under Windows 95, Windows 98, and Windows NT.  It
    should also work on both 486 and Pentium machines.

    We don't support Macaulay 2 for Windows 3.1 or for MSDOS, but the source
    code is available on our web site, and you're welcome to try to compile
    it and see whether it can pass all of our tests.

    When running Macaulay 2 under the bash shell obtainable from
    http://www.cygnus.com, it thinks its standard input is closed or
    truncated in random ways.  One can avoid this by using Macaulay 2 only as
    described below, but it is annoying.

    The best way to run Macaulay 2 under Windows is inside emacs, using our
    provided emacs macros, as described below.  The version of emacs that
    works under Windows 95 and Windows NT is is NTemacs.  It is available at
    http://www.cs.washington.edu/homes/voelker/ntemacs.html .  We recommend it
    highly - it includes on line tutorials and documentation, and once you
    learn it, you will probably want to make it your only text editor.

    One problem with running Macaulay2 in a shell window inside emacs running
    bash is that Macaulay2 can't tell it is talking to a tty, and hence won't
    offer input prompts soon enough.  To fix this problem, add "-tty" as an
    argument to the command line when starting the program, like this:

	     M2 -tty

    This problem also affects running Macaulay2 inside emacs using our
    macros.  Assuming you have version 20, add the following line to your
    .emacs file:

	(setq explicit-M2-args '("-tty"))

    Another problem that may occur wehn running Macaulay2 inside emacs is
    the appearance of ASCII carriage return characters on the screen at the
    end of each line as "^M" combinations.  To eliminate this, put the
    following lines in your .emacs file.

	(setq process-coding-system-alist
	      (cons (cons "M2" 'raw-text) process-coding-system-alist))

    or maybe even

	(setq process-coding-system-alist
              (cons (cons "M2" (cons 'raw-text-dos 'raw-text-unix))
	            process-coding-system-alist))

    Eventually you may want to interchange the control key with the caps lock
    key on your Windows keyboard.  This can be done.  For Windows 95, you can
    use the Kernel Toys from Microsoft, available at
  http://www.microsoft.com/windows/downloads/contents/powertoys/w95keybdremap/default.asp
    .  For Windows NT you could use the following procedure, which worked for
    us, and came from Chris McMahan <cmcmahan@teknowledge.com>.

    >> Open the registry editor and go to:
    >> HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\KeyBoard Layout
    >> 
    >> (note:  KeyBoard Layout, not KeyBoard Layouts)
    >> 
    >> Then create a new binary value by right-clicking on the KeyBoard
    >> Layout entry in the left window and selecting New\Binary Value
    >> 
    >> A new entry will appear in the right window called "New Value #1",
    >> rename this to "Scancode Map" (no quotes).
    >> 
    >> Right click on the new entry you just created, select the Modify
    >> entry and you'll see a window with a Value name field and a large
    >> textarea (named Value data:) with the value 0000 in it.
    >> 
    >> Click on the 0000 and a text prompt will appear in the text area.
    >> Then type the following values in this area:
    >> 
    >> 00 00 00 00 00 00 00 00 
    >> 
    >> At this point, a new row will be created with the label
    >> 0008
    >> 
    >> Now type these values:
    >> 
    >> 03 00 00 00 1d 00 3a 00 
    >> 
    >> and a new row will appear with the label
    >> 0010
    >> 
    >> type these values into this row
    >> 
    >> 3a 00 1d 00 00 00 00 00
    >> 
    >> A new row will appear with the label 0018...ignore it.
    >> 
    >> Now review the text area and ensure it looks like this:
    >> 
    >> 0000  00 00 00 00 00 00 00 00
    >> 0008  03 00 00 00 1d 00 3a 00
    >> 0010  3a 00 1d 00 00 00 00 00  
    >> 0018
    >> 
    >> Press the ok key, and VOILA! you have modified the scancode for your
    >> system. Close the registry and reboot your machine, and your CapsLock
    >> and Ctrl key should now be swapped.

    Another way to run Macaulay 2 under Windows is to set up a shortcut for
    opening an MSDOS window that provides you with very wide lines.  Create
    such a shortcut, and then edit the numbers on the Layout tab in the
    Properties window.  Make the width as large as you can, and make the
    height as large as you want.  (Macaulay 2 doesn't wrap lines for you, so
    if you print out a wide matrix, you will want the window not to wrap the
    lines either, for it will tend to separate things that below together,
    like variables and their exponents.  Making the lines very wide will
    allow you to scroll left and right to view large matrices.)
