doc ///
  Key
    "Pfaffians on quadrics"
  Headline
    compute the quartic and betti table corresponding to a pfaffian ideal in a quadric
  Description
    Text
      Having a skew-symmetric map between a bundle and its dual from Proposition 7.2 we determine the Pfaffian ideal I in the ring of the quadric, the corresponding quartic $F$ to which I is fully apolar and the betti table of $F^{\perp}$ . 

    Example
      R=QQ[x,y,z,t,Degrees=>{{1,0},{1,0},{0,1},{0,1}}]
      Q=QQ[a,b,c,d]
      seg=map(R,Q, gens (ideal {x,y}* ideal (z,t)))
    Text 
      We choose a random map between suitable vector bundles as in the list below

    Example
      NTypes = hashTable{
          N100 => random(R^{3:{0,1}, 3:{1,0}, {0,0}}, R^{3:{0,-1}, 3:{-1,0}, {0,0}}), 
          N683 => random(R^{{2,1}, {2,1}, {-1,1}}, R^{{-2,-1}, {-2,-1}, {1,-1}}),
          N550 => random(R^{{2,1}, {1,2}, {0,0}}, R^{{-2,-1}, {-1,-2}, {0,0}}),
          N400 => random(R^{{1,1}, {1,1}, {1,1}}, R^{{-1,-1}, {-1,-1}, {-1,-1}}),
          N300a => random(R^{{1,0}, {0,1}, 2:{1,1},{0,0}}, R^{{-1,0}, {0,-1}, 2:{-1,-1}, {0,0}}),
          N300b => randomBlockMatrix({R^{{1,0},{0,0}},  R^{{0,1}, 2:{1,1}}},{R^{{-1,0},{0,0}},  R^{{0,-1}, 2:{-1,-1}}}, {{0,random},{random,random}}),
          N300c => randomBlockMatrix({R^{{0,1}},R^{{0,0},{1,0}}, R^{ 2:{1,1}}},{R^{{0,-1}},R^{{0,0},{-1,0}}, R^{ 2:{-1,-1}}}, {{0,0,random},{0,random, random},{random, random, random}}),
          N310 => randomBlockMatrix({R^{{0,0}},R^{{0,1},{1,0}}, R^{ 2:{1,1}}},{R^{{0,0}},R^{{0,-1},{-1,0}}, R^{ 2:{-1,-1}}}, {{0,0,random},{0,random, random},{random, random, random}}),
          N430 => randomBlockMatrix({R^{{0,1}},R^{{0,0}},  R^{{0,0}}, R^{{1,1},{2,1}}},{R^{{0,-1}},R^{{0,0}},  R^{{0,0}},R^{ {-1,-1},{-2,-1}}}, {{0,0,random, random},{0,0,0,random},{random,0,random,random},{random, random, random, random}}),
          N320 => random(R^{{1,0}, 2:{0,1}, {2,1},{0,0}}, R^{{-1,0}, 2:{0,-1}, {-2,-1}, {0,0}}),
          N200 => random(R^{2:{1,0}, 2:{0,1}, {1,1}}, R^{2:{-1,0}, 2:{0,-1}, {-1,-1}}),
          N420 => randomBlockMatrix({R^{3:{1,1}},R^{2:{0,0}}},{ R^{3:{-1,-1}},R^{ 2:{0,0}}}, {{random, random},{random,0}}),
          N441a => random(R^{{1,2}, {2,1}, {0,1},{1,0},{-1,-1}}, R^{{-1,-2}, {-2,-1}, {0,-1},{-1,0}, {1,1}}),
          N441b => random(R^{{1,0}, {1,0}, {1,3}}, R^{{-1,0}, {-1,0}, {-1,-3}}),
          N551 => randomBlockMatrix({R^{{1,2}, {2,1},{0,0}}, R^{{1,1},{-1,-1}}},{ R^{{-1,-2}, {-2,-1},{0,0}},R^{{-1,-1},{1,1}}},{{random, random},{random,0}} ),
          N562 => randomBlockMatrix({R^{{1,2}, {2,1},{0,0}},R^{{0,1},{0,-1}}},{ R^{{-1,-2}, {-2,-1},{0,0}}, R^{{0,-1},{0,1}}},{{random, random},{random,0}}),
          };
    Text 
      We define the function BettiPfaffian returning the Betti table of $F^{\perp}$ for $F$ the quartic to which the Pfaffian ideal for a random skewsymmetric map of prescribed shape is fully apolar
    Example 
      BettiPfaffian= N->(
          NN=N-transpose N; -- NN will be a random skew-matrix of the given shape
          K=pfaffians(numrows NN-1,NN);
          I=preimage (seg, K);
          Quartic=(inverseSystem (super basis(4,I)))_0;--commpute the quartic
          Betti=betti resolution inverseSystem Quartic;
          return Betti)

      netList for typeN in keys(NTypes) list {typeN, BettiPfaffian(NTypes#typeN)}
  SeeAlso      
    "[QQ]"
///
