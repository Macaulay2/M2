--		Copyright 1994 by Daniel R. Grayson

pad = method()

pad(String,ZZ) := (s,n) -> concatenate(s,n-# s)
pad(ZZ,String) := (n,s) -> concatenate(n-# s,s)

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
	       w >= newcols - 1 + sum apply(pack(lens, newrows), max)
	       )
	  ) do (
	  ncols = newcols;
	  nrows = newrows;
	  );
     horizontalJoin between(" ", apply(pack(s,nrows), col -> stack col)))

net Time := v -> (
     t := "-- " | string v#0 | " seconds";
     x := v#1;
     if x === null then t else net x || t
     )

