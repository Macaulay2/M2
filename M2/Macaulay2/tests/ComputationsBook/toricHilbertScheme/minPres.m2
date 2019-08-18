findRedundant=(f)->(
     A := ring(f);
     p := first entries contract(vars A,f);
     i := position(p, g -> g != 0 and first degree g === 0);
     if i === null then
         null
     else (
     	  v := A_i;
     	  c := f_v;
     	  {i,(-1)*(c^(-1)*(f-c*v))}
	  )
     )

removeRedundantVariables = (I) -> (
     A := ring I;
     xmap := new MutableList from gens A;	
     M := gens I;
     findnext := () -> (
     	  p := null;
     	  next := 0;
     	  done := false;
          ngens := numgens source M;
	  while next < ngens and not done do (
       	    p = findRedundant(M_(0,next));
       	    if p =!= null then
	         done = true
       	    else next=next+1;
	  );
          p);
     p := findnext();
     while p =!= null do (
	  xmap#(p#0) = p#1;
	  F1 := map(A,A,toList xmap);
	  F2 := map(A,A, F1 (F1.matrix));
     	  xmap = new MutableList from first entries F2.matrix;
          M = compress(F2 M);
	  p = findnext();
	  );
     map(A,A,toList xmap));







