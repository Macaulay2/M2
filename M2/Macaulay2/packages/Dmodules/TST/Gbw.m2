TEST///
  -- Tests finding Grobner basis in case when u+v>0
  W = QQ[x,y,dx,dy, WeylAlgebra => {x=>dx,y=>dy}]
  I = ideal(x^2+y^3, x*y);
  J = ideal(x+dy,x*dx+y*dy);
  K = ideal(y*dx, x*dx+y);
  assert (entries gens gbw(I,{2,1,0,0}) == entries matrix {{x*y,x^2+y^3,y^4}});
  assert (entries gens gbw(I,{1,2,0,0}) == entries matrix {{x*y,x^3,x^2+y^3}});
  assert (entries gens gbw(J,{4,3,2,1}) == entries matrix{{1}});
  assert (entries gens gbw(K,{2,1,0,0}) == entries matrix {{y*dx, x*dx+y, y^2}});
  assert (entries gens gbw(K,{1,2,0,0}) == entries matrix {{x*dx^2+dx, x*dx+y}});
  assert (sub(inw(I,{2,1,0,0}),W) == ideal(x*y,x^2,y^4));
  assert (sub(inw(J,{3,2,3,1}),W) == W);
  assert (sub(inw(K,{1,2,0,0}),W) == ideal(x*dx^2,y));
  -- Testing when u+v = 0
  assert (entries gens gbw(K,{2,3,-2,-3}) == entries matrix {{y*dx, x*dx+y,x*dx^2+dx}});
  assert (entries gens gbw(K,{3,2,-3,-2})==entries matrix {{y*dx, x*dx+y,x*dx^2+dx}});
  assert (entries gens gbw(K,{0,0,0,0}) == entries matrix {{y*dx, x*dx+y, y^2}});
  assert (sub(inw(K,{2,3,-2,-3}),W) == ideal(y,x*dx^2+dx));
///

TEST ///
  -- Initial ideals and gb's in the same Grobner cone
  needsPackage "HolonomicSystems"
  W = QQ[x,y,z,dx,dy,dz, WeylAlgebra => {x=>dx,y=>dy,z=>dz}]
  A = matrix{{1,1,1},{0,2,7}};
  b = {1,5};
  I = gkz(A,b);

  -- weight vector of the form (-u,u)
  w1 = {-1,-10,-30,1,10,30};
  w2 = {-1,-10,-31,1,10,31};
  I1 = inw(I, w1);
  G1 = gbw(I, w1);
  assert(I1 == inw(I, w2));
  assert(G1 == gbw(I, w2));
  setHomSwitch false;
  I1' = inw(I, w1);
  G1' = gbw(I, w1);
  assert(I1' == I1);
  assert(G1' == G1);
  assert(I1' == inw(I, w2));
  assert(G1' == gbw(I, w2));
  setHomSwitch true;

  -- weight vector (u,v) with u+v > 0
  w1 = {0,1,2,3,4,100};
  w2 = {0,1,2,3,4,101};
  assert(inw(I,w1) == inw(I, w2));
  assert(gbw(I,w1) == gbw(I, w2));

  -- weight vector (u,v) with some comp's of u+v > 0, others equal to 0.
  w1 = {1,-3,107,-1,4,-5};
  w2 = {1,-3,108,-1,4,-5};
  I1 = inw(I, w1);
  assert(I1 == substitute(inw(I, w2), ring I1));
  assert(gbw(I, w1) == gbw(I, w2));
///
