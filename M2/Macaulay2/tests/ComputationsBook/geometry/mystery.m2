mystery = ringP3 -> (
   kk := coefficientRing ringP3;
   x := local x;
   ringP2 := kk[x_0..x_2];
   idealC2 := ideal(x_0^5+x_1^5+x_2^5);
   ringC2 := ringP2/idealC2;
   ringP5 := kk[x_0..x_5];
   idealC5 := trim kernel map(ringC2, ringP5, 
	gens (ideal vars ringC2)^2);
   ringC5 := ringP5/idealC5;
   use ringC5;
   trim kernel map(ringC5, ringP3,
      matrix{{x_0+x_1,x_2,x_3,x_5}}))
    
prettyPrint = f -> (
   -- accept a matrix f and print its entries prettily,
   -- separated by commas
   wid := 74;
   -- page width
   post := (c,s) -> (
      -- This function concatenates string c to end of each
      -- string in list s except the last one
      concatenate \ pack_2 between_c s);
   strings := post_"," (toString \ flatten entries f);
   -- list of strings, one for each polynomial, with commas
   istate := ("",0);
   -- initial state = (out : output string, col : column number)
   strings = apply(
      strings,
      poly -> first fold(
         -- break each poly into lines
         (state,term) -> (
            (out,col) -> (
               if col + #term > wid -- too wide?
               then (
                  out = out | "\n   "; 
                  col = 3;
                  -- insert line break
                  );
               (out | term, col + #term) -- new state
               )
            ) state,
         istate,
         fold( -- separate poly into terms 
            {"+","-"},
            {poly},
            (delimiter,poly) -> (
		 delimiterEscaped := if delimiter === "+" then "\\+" else delimiter;
	    	 flatten(post_delimiter \ separate_delimiterEscaped \ poly)
		 ))));
   print stack strings;  -- stack them vertically, then print
   )

end

----- to test:
idealX = mystery (ZZ/32003)
print pretty gens idealX
exit 0
