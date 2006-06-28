document all that somewhere!

Date: Tue, 27 Jun 2006 15:29:39 -0500 (CDT)
From: Dan Grayson <dan@math.uiuc.edu>
To: mike@math.cornell.edu
CC: dan@math.uiuc.edu
Subject: error messages
Reply-to: dan@math.uiuc.edu


I've added the recursionDepth to the prefix attached to error messages, to aid
in debugging.  A bug I had yesterday involved the interpreter screwing up the
computation of the recursion depth.

i3 : new ZZ of Nothing
stdio:3:1:(2):[2]: basic type for 'new' method should have been BasicList or HashTable
	       *
	       *
	       *
	       *
	       *

