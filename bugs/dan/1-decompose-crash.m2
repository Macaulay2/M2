R = QQ[x,y,z]
F = 5/4*x^4-3*x^3*y-39/4*x^2*y^2+256/5*x*y^3-297/5*y^4+83/20*x^3*z-41/15*x^2*y*z-1042/15*x*y^2*z+
      1993/15*y^3*z+367/60*x^2*z^2+139/6*x*y*z^2-2073/20*y^2*z^2-37/60*x*z^3+337/10*y*z^3-
      23/6*z^4+221/12*x^2*y-1024/15*x*y^2+943/15*y^3-83/6*x^2*z+1261/15*x*y*z-537/5*y^2*z-
      749/30*x*z^2+241/4*y*z^2-167/15*z^3
I = ideal F + ideal jacobian matrix{{F}}
--I = trim I -- Adding this line results in the next line finishing
irreducibleCharacteristicSeries I -- segmentation fault under 1.1 on intel mac

-- Also, it would be nice if this worked:
assert(set decompose I == set{ideal"-3y+2z-1,3x-z+2", ideal"-y+z,x-z", ideal"2y-z,x"})
