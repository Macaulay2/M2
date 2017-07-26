needsPackage "Bertini"




     R = CC[x,t]; -- include the path variable in the ring     
     H = { (x^2-1)*t + (x^2-2)*(1-t)};
     sol1 = point {{1}};
     sol2 = point {{-1}};
     S1= { sol1, sol2  };--solutions to H when t=1	  
     S0 = bertiniTrackHomotopy (t, H, S1) --solutions to H when t=0
     assert((1.414213562373-abs first coordinates first S0) <1e-7)
     peek S0_0
   Example     
     R=CC[x,y,t]; -- include the path variable in the ring     
     f1=(x^2-y^2);
     f2=(2*x^2-3*x*y+5*y^2);
     H = { f1*t + f2*(1-t)}; --H is a list of polynomials in x,y,t
     sol1=    point{{1,1}}--{{x,y}} coordinates
     sol2=    point{{ -1,1}}
     S1={sol1,sol2}--solutions to H when t=1
     S0=bertiniTrackHomotopy(t, H, S1, IsProjective=>1) --solutions to H when t=0 
     target1=coordinates first S0
     target2=coordinates last S0
     assert(Point===class first S0)
     assert(abs(.6 -((last target1/first target1)    +      (last target2/first target2)))<1e-9)
---
