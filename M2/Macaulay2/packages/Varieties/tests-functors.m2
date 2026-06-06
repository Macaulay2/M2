-- TODO: perhaps move these to the documentation
TEST ///
  -- The following examples appear in:
  -- Gregory G. Smith, Computing global extension modules,
  -- Journal of Symbolic Computation, 29 (2000) 729-746.

  -- Example 4.1: the bounds can be sharp.
  S = QQ[w,x,y,z];
  X = Proj S;
  I = monomialCurveIdeal(S,{1,3,4})
  N = S^1/I;
  assert(Ext^1(OO_X,N^~(>=-1)) == prune truncate(-1,Ext^1(truncate(1,S^1),N)))
  assert(Ext^1(OO_X,N^~(>= 0)) == prune truncate(0,Ext^1(truncate(2,S^1),N)))
  assert(Ext^1(OO_X,N^~(>= 0)) != prune truncate(0,Ext^1(truncate(1,S^1),N)))

  -- Example 4.2: locally free sheaves and global Ext.
  S = ZZ/32003[u,v,w,x,y,z];
  I = minors(2,genericSymmetricMatrix(S,u,3));
  X = variety I;
  R = ring X;
  Omega = cotangentSheaf X;
  OmegaDual = dual Omega;
  assert(Ext^1(OmegaDual, OO_X^1(>= 0)) == Ext^1(OO_X^1, Omega(>= 0)))

  -- Example 4.3: Serre-Grothendieck duality.
  S = QQ[v,w,x,y,z];
  X = variety ideal(w*x+y*z,w*y+x*z);
  R = ring X;
  omega = OO_X^{-1};
  G = sheaf cokernel genericSymmetricMatrix(R,R_0,2);
  assert(Ext^2(G,omega) == dual HH^0(G))
  assert(Ext^1(G,omega) == dual HH^1(G))
  assert(Ext^0(G,omega) == dual HH^2(G))
///
