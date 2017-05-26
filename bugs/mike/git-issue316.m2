-- git issue 315
R = ZZ[x, y, MonomialOrder => Lex]
I = ideal(32,16*x^2*y-10*x*y+1*x)
  gI = ideal gens gb I;
  assert( ( (gens I)%gI) == 0 );
  
-- git issue 316
restart
R = ZZ[x, y, MonomialOrder => GRevLex]
I = ideal(-19*x*y+7*y^2-12*x,-5*y^3)
  gI = ideal gens gb I;
  assert( ( (gens I)%gI) == 0 );
 ggI = ideal gens gb gI;
 assert(numColumns (gens gI) == numColumns(gens ggI)); -- fails
gI
ggI

R = ZZ[x, y, MonomialOrder => GRevLex]
I = ideal(8*x^3+6,-22*x^3+8*x*y^2-17*y)
  gI = ideal gens gb I;
  assert( ( (gens I)%gI) == 0 );
 ggI = ideal gens gb gI;
 assert(numColumns (gens gI) == numColumns(gens ggI)); -- fails

end

