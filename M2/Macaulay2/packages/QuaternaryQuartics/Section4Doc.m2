doc ///
  Key
     "Finding all possible betti tables for quadratic component of inverse system for quartics in 4 variables"
  Headline
     Material from Section 4 of [QQ]
  Description
     Text
       The following code is a slight modification of the code to find the 16 possible betti tables.
       Simply go thru all the tables, but only resolve the quadratic terms. 
     Example
       GetQuads = (d,k,n)->(
           R=ZZ/2[x_1..x_k]; 
           quartics=super basis(d,R);
           MonList=apply(rank source quartics, i->quartics_(0,i));
           L=subsets(MonList,n);
           J=apply(L, j->(F=gens ideal sum j;
           InvSysF=fromDual F;
           Idegs=degrees source mingens ideal InvSysF;
           if (not ((member({1},Idegs)))) then minimalBetti coker super basis(2, ideal InvSysF)));
           Jlist =drop(unique J,1);
           netList pack(4,Jlist))
     Example
       GetQuads(4,4,2)
  SeeAlso
      "[QQ]"
///
