--		Copyright 1993-1999 by Daniel R. Grayson

pad = method()

pad(String,ZZ) := String => (s,n) -> concatenate(s,n-# s)
pad(ZZ,String) := String => (n,s) -> concatenate(n-# s,s)

columnate = (s,w) -> (
     lens := apply(s,i -> #i);
     ncols := (w+1) // (max lens + 1);
     if ncols === 0 then ncols = 1;
     nrows := (# s + ncols - 1)//ncols;
     local newcols;
     local newrows;
     while (
	  ncols < # s and (
	       newcols = ncols+1;
	       newrows = (# s + ncols - 1)//newcols;
	       w >= newcols - 1 + sum apply(pack(newrows, lens), max)
	       )
	  ) do (
	  ncols = newcols;
	  nrows = newrows;
	  );
     horizontalJoin between(" ", apply(pack(nrows,s), col -> stack col)))

net Time := v -> (
     t := "-- " | toString v#0 | " seconds";
     x := v#1;
     if x === null then t else net x || t
     )

truncateOutput = method()
truncateOutput ZZ := maxwidth -> (
     Thing.BeforePrint = x -> (
	  x = net x;
	  if width x <= maxwidth then return x;
	  stack apply( unstack x, row -> (
		    if #row <= maxwidth then return row;
		    concatenate(substring(0,maxwidth - 4,row), " ...")))))
truncateOutput InfiniteNumber := maxwidth -> remove(Thing,BeforePrint)

ParagraphList = new Type of BasicList
net ParagraphList := x -> (
     x = toList x;
     x = net \ x;					    -- convert each to net
     x = select(x, p -> width p > 0);			    -- omit null strings
     x = apply(x,p -> wrap(printWidth - promptWidth(), net p));	-- wrap each one
     stack x)

-- not printing:
Bag = new SelfInitializingType of MutableList
unbag = method()
unbag Bag := first
net Bag := toString Bag := x -> "--a bagged " | synonym class first x | "--"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
