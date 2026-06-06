TEST ///
  -- Ranks of gkz systems
  A = matrix{{1,1,1,1},{0,1,3,4}};
  assert (holonomicRank(gkz(A, {1,3})) == 4);
  assert (holonomicRank(gkz(A, {1,2})) == 5);
  assert (isHolonomic gkz(A,{-1/2, 5/3}));
///

TEST ///
  -- Initial ideals and gb's in the same Grobner cone
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

TEST ///
  -- Properties of AppellF1
  I = AppellF1 ({2,4,-1,3/2});
  J = substitute (AppellF1 ({3,-1,7/3,-5}), vars ring I);
  K = directSum(cokernel gens I, cokernel gens J);
  assert (Ddim I == Ddim J);
  assert (holonomicRank I == holonomicRank J);
  assert (singLocus I == singLocus J);
  assert (charIdeal I == charIdeal J);
  assert (isHolonomic K);
  assert (holonomicRank K == holonomicRank I + holonomicRank J);
  assert (singLocus K == singLocus I);
  w' = {0,0,1,1}
  assert (inw(I,w') == inw(J,w'));
///
