-- This one was fixed previous to (8 Sep 2010)

gbTrace = 3
R = QQ [x, y, MonomialOrder => Position => Up]
scan(1000, i -> (
	  f := substitute (random(R^10, R^{10:-2}),y=>1);
	  g := gens gb(f);
	  h := gens gb(g);
	  k := gens gb(h);
	  ))
end
	  h := time ultimate(g -> map(R^(rank g), R^(rank g), gens gb transpose g), f);
	  collectGarbage()
	  ))

-- finalization bug
R = ZZ/101[a..d]
dogb = () -> (
     m := matrix{{a}};
     gens gb m;)
dogb()
collectGarbage()

-- How does this do? --------------------------------------
gbTrace = 3
R = QQ [x, y, MonomialOrder => Position => Up]
scan(1000, i -> (
	  f := substitute (random(R^10, R^{10:-2}),y=>1);))
-----------------------------------------------------------

-- The following code snippet uses 142 MB of space --------
R = QQ[a..d]
dogb = () -> (
     f = ideal (42*a^3-50*a^2*b-15*a*b^2+30*b^3+39*a^2*c-22*a*b*c+19*b^2*c+45*a*c^2+2*b*c^2-16*c^3+9*a^2*d+50*a*b*d-38*b^2*d-29*a*c*d-4*b*c*d-6*c^2*d-39*a*d^2-36*b*d^2-32*c*d^2+31*d^3,-32*a^3-38*a^2*b-42*a*b^2+37*b^3+31*a^2*c-50*a*b*c-22*b^2*c+15*a*c^2+45*b*c^2-9*c^3+24*a^2*d-41*a*b*d-19*b^2*d+17*a*c*d-8*b*c*d+32*c^2*d-28*a*d^2-31*b*d^2-4*c*d^2+4*d^3,-2*a^3+24*a^2*b+15*a*b^2-42*b^3+a^2*c-15*a*b*c+7*b^2*c-2*a*c^2+35*c^3-45*a^2*d-10*a*b*d+13*b^2*d-49*b*c*d-15*c^2*d+21*a*d^2-41*b*d^2+8*c*d^2+20*d^3,-13*a^3+16*a^2*b+50*a*b^2-45*b^3-44*a^2*c-33*a*b*c+23*b^2*c+a*c^2+9*b*c^2+16*c^3-46*a^2*d+33*a*b*d-6*b^2*d+31*a*c*d-28*b*c*d+c^2*d-50*b*d^2-19*c*d^2-31*d^3);
     gens gb f;)
scan(1000, i -> dogb())
-----------------------------------------------------------

gb f

end
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
