nextUpper=(d)->(if d_n<n+r then join(d_{0..n-1},{d_n+1}) else (
	  --same but above
	  k:=n; 
	  while (d_k-1==d_(k-1) ) do (k=k-1);
	  join(d_{0..k-2},{d_(k-1)+1},d_{k..n})))

upperRange=(d)->(A:={d};if (
	  --same but above
	  d =!= toList(r..n+r) ) then ( 
          e:=nextUpper(d);
	  A=join(A,upperRange(e)););
     	  A)	  

upperRange1=(d,dep)->(A:={d};if (
	  << " " << dep << flush;
	  --same but above
	  d =!= toList(r..n+r) ) then ( 
          e:=nextUpper(d);
	  A=join(A,upperRange1(e,dep+1)););
     	  A)	  

n = 4
r = 11
e = {10, 11, 12, 15, 16}
upperRange e  -- this crashes
upperRange1 (e, 1) -- this doesn't crash, and gives recursion limit exceeded.
