--		Copyright 1994 by Daniel R. Grayson

pad = method()

pad(String,ZZ) := (s,n) -> concatenate(s,n-# s)
pad(ZZ,String) := (n,s) -> concatenate(n-# s,s)

document { quote pad,
     TT "pad(s,n)", " -- pads the string s to length n with spaces on the right.",
     BR,
     NOINDENT, 
     TT "pad(n,s)", " -- pads the string s to length n with spaces on the left."
     }

columnate = (s,w) -> (
     lens := apply(s,i -> #i);
     ncols := (w+1) // (max lens + 1);
     if ncols === 0 then ncols = 1;
     nrows := (# s + ncols - 1)//ncols;
     local newcols, local newrows;
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
     horizontalJoin between(" ", apply(pack(s,nrows), col -> verticalJoin col)))

document { quote columnate,
     TT "columnate(s,w)", " -- arranges the strings in the list s in columns, returning
     a ", TO "Net", " suitable for output to a terminal with a linewidth of w.",
     }

net Time := v -> (
     t := "-- " | string v#0 | " seconds";
     x := v#1;
     if x === null then t else net x || t
     )

BeforePrint Net := BeforePrint String := net
