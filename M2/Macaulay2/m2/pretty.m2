--		Copyright 2006 by Daniel R. Grayson

needs "nets.m2"
needs "hypertext.m2"

pretty = method(Dispatch => Thing)
pretty2 = method(Dispatch => Thing)
pretty Thing := x -> stack pretty2 x

pr := ou -> x -> (
     r := ou x;
     if width r <= printWidth then return 1:r;
     f := lookup(Wrap, class x);
     if f =!= null then (
	  r = f r;
     	  if width r <= printWidth then return 1:r; -- get the last bit separate!
	  );
     1: wrap("-",r)				    -- get the last bit separate!
     )
pretty2 Thing := pr net

pretty2 String := s -> (				    -- maybe we can get rid of this
     r := wrap format s;
     if class r === Net then toSequence unstack r else 1:r)
pretty2 String := pr toString

pretty2 Nothing := pr toString

pretty2 List      := x -> pretty3("{", " ", ",", " ", "}", toSequence x)
pretty2 Array     := x -> pretty3("[", " ", ",", " ", "]", toSequence x)
pretty2 Sequence  := x -> pretty3("(", " ", ",", " ", ")", x)
pretty2 Hypertext := x -> pretty3(net class x|"{","   ",","," ","}",toSequence x)
pretty2 IntermediateMarkUpType := x -> (
     r := wrap net x;
     if class r === Net then toSequence unstack r else 1:r)
pretty2 Option := o -> (
     sav := printWidth;
     printWidth = (printWidth-4)//2;
     if printWidth <= 0 then error "internal error: printWidth too small";
     o = pretty o#0 | " => " | pretty o#1;
     printWidth = sav;
     1:o)     

pretty3 = (l,i,m,m',r,s) -> (
     if #s == 0 then return 1:l|r;
     sav := printWidth;
     printWidth = printWidth - max(#l,#i) - max(#m,#r);
     if printWidth <= 0 then error "internal error: printWidth too small";
     s' := apply(s,pretty2);
     printWidth = sav;
     w := j := 0;
     rowstart := true;
     endrow := false;
     processRow := x -> (
	  if #x#-1 == 1 then (r :=horizontalJoin x; r )
	  else (
	       a := horizontalJoin apply(drop(x,-1),unsequence);
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
	  if w+n <= printWidth then ( w = w + n; rowstart = false; j += 1; x)
	  else ( if rowstart then error "pretty: internal error"; w = 0; break));
     splice toSequence while true list (
	  if j == #s' then break;
	  endrow = false;
	  rowstart = true;
	  w = 0;
	  processRow while true list (
	       if endrow then break;
	       if j == 0 then item(l,s'#0,if #s' == 1 then r else m)
	       else if j<#s'-1 then if rowstart then item(i,s'#j,m) else item(m',s'#j,m) 
	       else if s'#?j then if rowstart then item(i,s'#j,r) else item(m',s'#j,r)
	       else break)))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
