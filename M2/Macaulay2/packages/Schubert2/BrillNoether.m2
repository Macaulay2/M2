BrillNoetherBundle = (g,r,d) -> (
     B := QQ[theta,Join=>false]/theta^(g+1);
     Pic := abstractVariety(g,B);
     Pic.TangentBundle = OO_Pic^g;
     integral B := f -> (
	  i := g! * coefficient(theta^g,f);
	  try lift(i,ZZ) else lift(i,QQ));
     e := g+1;						 -- minimize this later
     rankA = d+1-g+e;
     kernelBundle(rankA -(r+1), OO_Pic^e, 
	  abstractSheaf(Pic, Rank => rankA,
	       ChernClass => sum for i from 0 to g list (-1)^i/i! * theta^i
	       )))
E = BrillNoetherBundle(5,1,4)
Wrd = variety E
euler Wrd
assert(euler Wrd === -20)
