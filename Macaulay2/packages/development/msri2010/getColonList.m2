getColonList = (J,v,resultsSoFar) -> (
     minusv:=toList(set(gens ring J)-set(v));
     outputlist:={};
     finaloutput:={};
     for j from 0 to (length minusv-1) do (
     xvar:=minusv_j;
     minusv2=delete(xvar,minusv);
     f:=(eliminate(J,minusv2))_*;
     assert(f !={});
     f=f_0;
     factorlist=apply(toList factor f, toList);
     outputlist=flatten apply(length(factorlist),i->
	  ( if (J:ideal(f//((factorlist_i)_0^((factorlist_i)_1)))):resultsSoFar==1 
	       then {} 
	       else J:ideal(f//((factorlist_i)_0^((factorlist_i)_1)))
	       )
	       );
     print(outputlist);
     if length(outputlist)>length finaloutput then finaloutput=outputlist;
     );
     return finaloutput;
     )