--inputs: 0. a polynomial ring R
        --1. a list of ideals d which give the split subvarieties of dim k
        --2. an ideal I in the list d (for determining which subvariety to work with)
	
--outputs: 1. a new polynomial ring (from the integral closure)
         --2. a list of compatibly split ideals in the new ring which gives the divisor in d_l 


newPackage(
	  "oneStep3", --Same as the filename
	  Version => "1.0",
	  Date => "March 10, 2010",
	  Headline => "OneStep in the algorithm",
	  DebuggingMode => true
	  )
     
export {iteration}    
     
-- to Functoriality
iteration = method()
iteration(Ring,List,Ideal) := (R,L,I) -> (
     --do intersections
     div1 := flatten apply(L, i-> (if (i != I) then flatten decompose (i+I) else flatten {}));
     --find non-R1 locus 
     d:=(dim(I) -1);
     s:= decompose ideal singularLocus(R/I);
     nR1 := flatten apply(s, i->(if (dim(i)==d) then i else {}));
     --figure out the divisor 
     div := flatten {nR1,div1};
     --compute integralClosure
     icId:= ideal integralClosure(R/I);
     icR:= ring icId;
     f:= icFractions(R/I);
     --figure out the divisor inside of icR
     m:= map(icR, R);
     fdiv := apply(div, i->trim(m(i)+icId));
     (I, fdiv, icR, icId,f)   
     )
end