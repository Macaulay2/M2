Date: Fri, 7 Jul 2006 09:15:34 -0500 (CDT)
From: Dan Grayson <dan@math.uiuc.edu>
To: mike@math.cornell.edu
CC: dan@math.uiuc.edu
Subject: experiment
Reply-to: dan@math.uiuc.edu


I was wondering what would happen if I repeatedly did "gens gb transpose oo" to
a homogeneous matrix in three variables, where there is no reason to expect
things to stabilize.  Things seem to stabilize, and alternate matrices are ===.
(Is gb now giving a well-defined result that depends only on the submodule?)

Hmmm...

    i37 : R = QQ[x,y,z,MonomialOrder => Position => Up]

    o37 = R

    o37 : PolynomialRing

    i38 : f = random(R^2,R^{4:-2})

    o38 = | 4/5x2+xy-2/7y2+5/3xz-3yz+8/5z2 -9/5x2-4xy-6/5y2-xz-8/3yz-1/3z2     7/8x2+7xy+1/8y2+7/2xz+4yz-5/4z2 -8/3x2-8/5xy+7/4y2+2/9xz-1/2yz+1/7z2 |
	  | 4/5xy+y2+1/7xz+5/7yz-7/8z2     9/5x2+1/2xy+1/6y2-8/9xz-2/7yz-4/5z2 1/3x2+2/3y2+2/5xz-6/5yz+7z2     -5/2x2+xy-3/4y2+9/5xz-2/5yz-7/5z2    |

		  2       4
    o38 : Matrix R  <--- R

    i39 : gens gb transpose oo;

       -- {0}(2)mm{1}(1)m{2}(1)m
		  4       4
    o39 : Matrix R  <--- R

    i40 : gens gb transpose oo;

       -- {2}(4)mmmm{3}(3)mmm{4}(6)mmmmmm{5}(7)mmmmooo{6}(8)mmoooooo{7}(4)oooo
		  4       19
    o40 : Matrix R  <--- R

    i41 : gens gb transpose oo;

       -- {0}(2)mm{1}(2)mo{2}(1)o{3}(2)mm{4}(1)o
		  19       5
    o41 : Matrix R   <--- R

    i42 : gens gb transpose oo;

       -- {2}(4)mmmm{3}(6)mmmooo{4}(12)mmmmmmoooooo{5}(15)mmmmmmmmooooooo{6}(13)mmoooooooo
       -- ooo{7}(4)oooo
		  5       23
    o42 : Matrix R  <--- R

    i43 : gens gb transpose oo;

       -- {0}(2)mm{1}(2)mo{3}(4)mmoo{4}(1)o{5}(1)m
		  23       6
    o43 : Matrix R   <--- R

    i44 : gens gb transpose oo;

       -- {2}(4)mmmm{3}(6)mmmooo{4}(12)mmmmmmoooooo{5}(19)mmmmmmmmooooooooooo{6}(13)mmoooo
       -- ooooooo{7}(4)oooo
		  6       23
    o44 : Matrix R  <--- R

    i45 : gens gb transpose oo;

       -- {0}(2)mm{1}(2)mo{3}(4)mmoo{4}(1)o{5}(2)mo
		  23       6
    o45 : Matrix R   <--- R

    i46 : gens gb transpose oo;

       -- {2}(4)mmmm{3}(6)mmmooo{4}(12)mmmmmmoooooo{5}(19)mmmmmmmmooooooooooo{6}(13)mmoooo
       -- ooooooo{7}(4)oooo
		  6       23
    o46 : Matrix R  <--- R

    i47 : o44 == o46

    o47 = true

    i48 : betti o46

		 0  1
    o48 = total: 6 23
	     -5: 1  .
	     -4: .  .
	     -3: 2  .
	     -2: .  .
	     -1: 1  .
	      0: 2  .
	      1: .  4
	      2: .  3
	      3: .  6
	      4: .  8
	      5: .  2

    o48 : BettiTally

-----------------------------------------------------------------------------

Date: Fri, 7 Jul 2006 09:28:21 -0500 (CDT)
From: Dan Grayson <dan@math.uiuc.edu>
To: mike@math.cornell.edu
CC: dan@math.uiuc.edu
In-reply-to: <200607071415.k67EFYK02195@u00.math.uiuc.edu> (message from Dan
	Grayson on Fri, 7 Jul 2006 09:15:34 -0500 (CDT))
Subject: Re: experiment
Reply-to: dan@math.uiuc.edu


That last 6 by 23 matrix has both its rows and its columns being a gb!

> Date: Fri, 7 Jul 2006 09:15:34 -0500 (CDT)
> From: Dan Grayson <dan@math.uiuc.edu>
> To: mike@math.cornell.edu
> CC: dan@math.uiuc.edu
> Subject: experiment
> Reply-to: dan@math.uiuc.edu
> 
> 
> I was wondering what would happen if I repeatedly did "gens gb transpose oo" to
> a homogeneous matrix in three variables, where there is no reason to expect
> things to stabilize.  Things seem to stabilize, and alternate matrices are ===.
> (Is gb now giving a well-defined result that depends only on the submodule?)
> 
> Hmmm...

  ...

