Date: Thu, 6 Jul 2006 16:37:24 -0500
From: Dan Grayson <dan@math.uiuc.edu>
To: mike@math.cornell.edu
CC: dan@math.uiuc.edu
Subject: StopWithMinimalGenerators broken
Reply-to: dan@math.uiuc.edu


This is a bug:

    i91 : R = QQ[x,y,MonomialOrder => Position => Down ]

    o91 = R

    o91 : PolynomialRing

    i92 : f = transpose matrix {{0,1-y,x+x^2}, {x,y,0}, {x,0,y}}

    o92 = {-1} | 0    x x |
	  {-1} | -y+1 y 0 |
	  {-2} | x2+x 0 y |

		  3       3
    o92 : Matrix R  <--- R

    i93 : g = gens gb (f, StopWithMinimalGenerators=>true, Syzygies=>false, ChangeMatrix=>false)

    o93 = {-1} | 0      x |
	  {-1} | 1      0 |
	  {-2} | x2+x-y y |

		  3       2
    o93 : Matrix R  <--- R

    i94 : image g == image f

    o94 = false

