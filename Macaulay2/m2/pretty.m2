--		Copyright 2006 by Daniel R. Grayson

callCount := new MutableHashTable

on := { CallLimit => 100000, Name => null } >> opts -> f -> (
     fb := functionBody f;
     depth := 0;
     totaltime := 0.;
     if not callCount#?fb then callCount#fb = 0;
     limit := opts.CallLimit;
     if class f =!= Function then error("expected a function");
     fn := if opts.Name =!= null then opts.Name else try toString f else "--function--";
     x -> (
	  saveCallCount := callCount#fb = callCount#fb+1;
     	  << fn << " (" << saveCallCount << ")";
	  if depth > 0 then << " [" << depth << "]";
	  << " called with ";
	  try << class x << " ";
	  << peek(x);
	  << endl;
	  if callCount#fb > limit then error "call limit exceeded";
	  depth = depth + 1;
     	  r := timing f x;
	  timeused := r#0;
	  value := r#1;
	  depth = depth - 1;
	  if depth === 0 then totaltime = totaltime + timeused;
     	  << fn << " (" << saveCallCount << ")";
	  if depth > 0 then << " [" << depth << "]";
	  if timeused > 1. then << " used " << timeused << " seconds";
	  if totaltime > 1. and depth === 0 then << " (total " << totaltime << " seconds)";
	  << " returned " << class value << " " << peek'(5,value) << endl;
     	  value)
     )


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
pretty2 BasicList := x -> pretty3(net class x|"{","   ",","," ","}",toSequence x)

pretty2 = on pretty2

pretty3 = (l,i,m,m',r,s) -> (
     if #s == 0 then l|r;
     sav := printWidth;
     printWidth = printWidth - max(#l,#i) - max(#m,#r) - 2;
     s = apply(s,pretty2);
     printWidth = sav;
     w := j := 0;
     rowstart := true;
     endrow := false;
     h := x -> (
	  if #x#-1 == 1 then horizontalJoin x
	  else (
	       a := horizontalJoin apply(drop(x,-1),unSingleton);
	       b := concatenate(width a : " ");
	       c := stack drop(x#-1,-1);
	       (horizontalJoin(a,c), horizontalJoin(b,x#-1#-1))
	       )
	  );
     g := (l,x,r) -> (
	  if #x > 1 then endrow = true;
	  l' := concatenate(width l : " ");
	  x = append( drop(x,-1), last x | r);
	  x = prepend(l | first x, apply(drop(x,1), t -> l'|t));
	  n := max(width\x);
	  if w+n <= printWidth then ( w = w + n; rowstart = false; j = j + 1; x)
	  else ( w = 0; if rowstart then error "pretty: internal error"; rowstart = true; break));
     h = on(h, Name=>"h");
     g = on(g, Name=>"g");
     splice toSequence while true list (
	  if j == #s then break;
	  endrow = false;
	  h while true list (
	       if endrow then break;
	       if j == 0 then g(l,s#0,if #s == 1 then r else m)
	       else if j<#s-1 then if rowstart then g(i,s#j,m) else g(m',s#j,m) 
	       else if s#?j then if rowstart then g(i,s#j,r) else g(m',s#j,r)
	       else break)))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
