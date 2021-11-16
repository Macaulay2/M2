doc ///
  Key
     "Finding the 16 betti tables possible for quartic forms in 4 variables, and examples"
  Headline
     Material from Table 6 and 7 of Appendix 1
  Description
     Text
       The following code finds the 16 possible betti tables. 
     Example
       GetInv = (d,k,n)->(R=ZZ/2[x_1..x_k]; 
           quartics=super basis(d,R);
             -- MonList=apply(rank source quartics, i->quartics_(0,i));
           MonList=first entries quartics;
           L=subsets(MonList,n);
           J=apply(L, j->(F=sum j;
                      InvSysF=ideal fromDual F;
                      Idegs=degrees source mingens InvSysF;
                           if (not ((member({1},Idegs)))) then minimalBetti InvSysF));
           Jlist = drop(unique J,1);  --get unique betti table, drop empty table from list
           netList pack(4,Jlist))    --format output to look nice
       GetInv(4,4,2)
     Text
       Get inverse system of all degree d polynomials in k variables, by taking sums of 
       n monomials. By taking sufficiently many monomials we'll get everything. Work over 
       Z/2 for speed, experiments suggest that even over Z/2 we get all betti tables.
       If inverse system has a linear generator, ignore it. Pack results using netlist 
       for pretty display. For k=4=d, we get 16 betti tables by using n=4 terms.
     Text
       A stable of 16 examples, one for each of the betti tables
     Example
       R=ZZ/101[x_1..x_4];
       IdealList={
         {x_2*x_4, x_1*x_4, x_1*x_2^2+x_3*x_4^2-x_4^3, x_1^2, x_2^3, x_3^2},
         {x_2*x_3, x_1*x_3, x_2*x_4^2, x_1*x_4^2, x_1*x_2^2-x_3*x_4^2, x_3*x_4^3-x_4^4, x_1^2, x_2^3, x_3^2},
         {x_2*x_4, x_1*x_4, x_3^2*x_4, x_1*x_2*x_3+x_3*x_4^2-x_4^3, x_1^2, x_2^2, x_3^3},
         {x_2*x_4^2, x_1*x_4^2, x_1*x_2*x_4+x_3*x_4^2-x_4^3, x_1*x_2*x_3-x_3*x_4^2, x_1^2, x_2^2, x_3^2},
         {x_2*x_3, x_1*x_3, x_1*x_2-x_3*x_4, x_3*x_4^3-x_4^4, x_2*x_4^3, x_1*x_4^3, x_1^2, x_2^2, x_3^2},
         {x_2*x_4, x_1*x_4, x_2*x_3, x_1*x_3, x_3*x_4^3-x_4^4, x_1^2*x_2^2-x_4^4, x_1^3, x_2^3, x_3^2},
         {x_2*x_3, x_1*x_3, x_2*x_4^2, x_1*x_4^2, x_3^2*x_4-x_4^3, x_1*x_2^2-x_4^3, x_1^2, x_2^3, x_3^3},
         {x_2*x_4^2, x_1*x_4^2, x_3^2*x_4-x_4^3, x_1*x_2*x_4-x_3*x_4^2, x_2*x_3^2, x_1*x_3^2, x_1*x_2*x_3-x_4^3, x_1^2, x_2^2, x_3^3},
         {x_2*x_3, x_1*x_3, x_1*x_2-x_3^2, x_3^2*x_4-x_4^3, x_1^2, x_2^2, x_3^3},
         {x_3*x_4, x_2*x_4, x_1*x_4, x_1*x_2*x_3-x_3^3, x_3^4-x_4^4, x_1^2, x_2^2},
         {x_1*x_4, x_3^2*x_4, x_2*x_3*x_4-x_4^3, x_1*x_3^2-x_3*x_4^2, x_1*x_2*x_3-x_2*x_4^2, x_1^2, x_2^2, x_3^3},
         {x_1*x_4, x_1*x_3, x_3*x_4^2, x_2*x_4^2, x_2^2*x_4, x_2*x_3^2-x_4^3, x_2^2*x_3, x_1*x_2^2-x_3^2*x_4, x_1^2, x_2^4, x_3^3},
         {x_3*x_4, x_2^2*x_4, x_1^2*x_4, x_2*x_3^2, x_1*x_3^2-x_2*x_4^2, x_2^2*x_3-x_1*x_4^2, x_1*x_2*x_3, x_2^3-x_1^2*x_3, x_1*x_2^2, x_1^2*x_2, x_1^3, x_2^4, x_3^3, x_4^3},
         {x_3*x_4 , x_2^2*x_4, x_3^3-x_1*x_2*x_4, x_2*x_3^2, x_2^2*x_3-x_1^2*x_4, x_1*x_2*x_3, x_1^2*x_3, x_2^3-x_1*x_3^2, x_1*x_2^2, x_1^3, x_2^4, x_3^4, x_4^2},
         {x_3*x_4^2, x_2*x_4^2, x_1*x_4^2, x_3^2*x_4,x_2^2*x_4, x_1^2*x_4, x_3^3-x_1*x_2*x_4, x_2*x_3^2, x_1*x_3^2, x_2^2*x_3, x_1*x_2*x_3-x_4^3, x_1^2*x_3, x_2^3-x_1*x_3*x_4, x_1*x_2^2, x_1^2*x_2,x_1^3-x_2*x_3*x_4},
         {x_1^2,x_2^2,x_3^2,x_4^2}};
     Text
       Display in a nice format
     Example
       netList(pack(4,apply(IdealList,i->minimalBetti ideal i)))
     Example
       Qs = apply(IdealList, I -> first flatten entries inverseSystem(5, ideal I))
       for F in Qs list quarticType F     
  SeeAlso
    (inverseSystem, RingElement)
    (quarticType, RingElement)
    "[QQ]"
///
