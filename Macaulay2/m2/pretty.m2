--		Copyright 2006 by Daniel R. Grayson

pretty = method(SingleArgumentDispatch => true)
pretty Thing := x -> stack pretty2 x

pretty2 = method(SingleArgumentDispatch => true)
pretty2 String := s -> 1 : format s
pretty2 Thing := x -> (
     r := net x;
     if width r <= printWidth then return 1:r;
     f := lookup(Wrap, class x);
     if f =!= null then (
	  r = f r;
     	  if width r <= printWidth then return 1:r; -- get the last bit separate!
	  );
     1: wrap(printWidth,"-",r)				    -- get the last bit separate!
     )

pretty2 List      := x -> pretty3("{", " ", ",", " ", "}", toSequence x)
pretty2 Array     := x -> pretty3("[", " ", ",", " ", "]", toSequence x)
pretty2 Sequence  := x -> pretty3("(", " ", ",", " ", ")", x)
pretty2 MarkUpList := x -> pretty3(net class x|"{","   ",","," ","}",toSequence x)

pretty3 = (l,i,m,m',r,s) -> (
     if #s == 0 then l|r;
     sav := printWidth;
     printWidth = printWidth - max(#l,#i) - max(#m,#r) - 2;
     s = apply(s,pretty2);
     printWidth = sav;
     w := j := 0;
     rowstart := true;
     endrow := false;
     processRow := x -> (
	  if #x#-1 == 1 then horizontalJoin x
	  else (
	       a := horizontalJoin apply(drop(x,-1),unSingleton);
	       b := concatenate(width a : " ");
	       c := stack drop(x#-1,-1);
	       (horizontalJoin(a,c), horizontalJoin(b,x#-1#-1))
	       )
	  );
     item := (l,x,r) -> (
	  if #x > 1 then endrow = true;
	  l' := concatenate(width l : " ");
	  x = append( drop(x,-1), last x | r);
	  x = prepend(l | first x, apply(drop(x,1), t -> l'|t));
	  n := max(width\x);
	  if w+n <= printWidth + 10 then ( w = w + n; rowstart = false; j = j + 1; x)
	  else ( if rowstart then error "pretty: internal error"; w = 0; break));
     splice toSequence while true list (
	  if j == #s then break;
	  endrow = false;
	  rowstart = true;
	  w = 0;
	  processRow while true list (
	       if endrow then break;
	       if j == 0 then item(l,s#0,if #s == 1 then r else m)
	       else if j<#s-1 then if rowstart then item(i,s#j,m) else item(m',s#j,m) 
	       else if s#?j then if rowstart then item(i,s#j,r) else item(m',s#j,r)
	       else break)))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
