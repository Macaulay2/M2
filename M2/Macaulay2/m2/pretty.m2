--		Copyright 2006 by Daniel R. Grayson

pretty = method(SingleArgumentDispatch => true)

pretty Thing := x -> (
     r := net x;
     if width r <= printWidth then return r;
     f := lookup(Wrap, class x);
     if f =!= null then (
	  r = f r;
     	  if width r <= printWidth then return r;
	  );
     wrap(printWidth,"-",r))

pretty List      := x -> pretty2("{", " ", ",", " ", "}", toSequence x)
pretty Array     := x -> pretty2("[", " ", ",", " ", "]", toSequence x)
pretty Sequence  := x -> pretty2("(", " ", ",", " ", ")", x)
pretty BasicList := x -> pretty2(net class x|"{","   ",", ","}",toSequence x)

pretty2 = (l,i,m,m',r,s) -> (
     if #s == 0 then l|r;
     sav := printWidth;
     printWidth = printWidth - max(#l,#i) - max(#m,#r);
     s = apply(s,pretty);
     printWidth = sav;
     w := j := 0;
     rowstart := true;
     g := x -> (
	  n := plus apply(x,width);
	  if w+n <= printWidth then (
	       w = w + n;
	       rowstart = false;
	       j = j + 1;
	       x)
	  else (
	       w = 0;
	       rowstart = true;
	       break)
	  );
     stack while true list (
	  if j == #s then break;
	  horizontalJoin while true list (
	       if j == 0 then g(l,s#0,m)
	       else (
		    if j<#s-1 then (
			 if rowstart
			 then g(i,s#j,m)
			 else g(m',s#j,m)
			 )
		    else if s#?j then (
			 if rowstart
			 then g(i,s#j,r)
			 else g(m',s#j,r)
			 )
		    else break
		    )
	       )
	  )
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
