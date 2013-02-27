R = ZZ/32003[a..f]
z=poly"ae-bd"
y=poly"-af+cd"
x=poly"bf-ce"
F= -100*z^3+8*x^5*z^2-100*x^2*y*z^2+8*x^7*y*z+120*x^4*y^2*z+450*x*y^3*z-135*y^5-230*x^3*y^4-20*x^6*y^3-8*x^9*y^2;
J = ideal jacobian ideal F;



end

ring r=32003, (a,b,c,d,e,f),ws(3,1,-1,4,2,0);
 
 r;
  
  //   characteristic : 32003 
  //   number of vars : 6 
  //        block   1 : ordering ws 
  //                  : names    a b c d e f 
  //                  : weights  3 1 -1 4 2 0 
  //        block   2 : ordering C 
  poly z=ae-bd;
   
   poly y=-af+cd;
    
    poly x=bf-ce;
     
     poly F=-100*z3 +8*x5*z2-100*x2*y*z2+8*x7*y*z+120*x4*y2*z+450*x*y3*z-135*y5-230*x3*y4-20*x6*y3-8*x9*y2;
      
      ideal j = jacob(F);
       
       qhweight(j);
        
        //3,1,-1,4,2,0 
        