Date: Thu, 6 Jul 2006 23:39:41 -0500
From: Dan Grayson <dan@math.uiuc.edu>
To: mike@math.cornell.edu
CC: dan@math.uiuc.edu
In-reply-to: <200607070418.k674IKeT010063@u123.math.uiuc.edu> (message from
	Dan Grayson on Thu, 6 Jul 2006 23:18:20 -0500)
Subject: Re: Smith n.f.
Reply-to: dan@math.uiuc.edu


I wrote some code to test that theory, and it seems to hold up, but the code
demonstrate a likely memory leak somwhere.  If I limit it to 200MB then it runs
out of memory after about 10 iterations of the loop below.

Do you have any tools for looking for memory leaks?

gbTrace = 3
R = QQ [x, y, MonomialOrder => Position => Up]
isDiagonal = f -> (
     m := min(rank target f, rank source f);
     R := ring f;
     zr := 0_R;
     f = new MutableMatrix from f;
     scan(m, i -> f_(i,i) = zr);
     f == 0)
scan(100, i -> (
	  f := substitute (random(R^10, R^{10:-2}),y=>1);
	  h := time ultimate(g -> map(R^(rank g), R^(rank g), gens gb transpose g), f);
	  assert isDiagonal h))
