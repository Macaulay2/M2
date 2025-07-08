TEST ///
  A = QQ[a,b,c];
  C = complex A^0
  K = filteredComplex C;
  assert(K_0 == C);
  assert(K_1 == C);
///

TEST ///
  A = QQ[a,b,c];
  D = simplicialComplex {a*b*c};
  F2D = D;
  F1D = simplicialComplex {a*b,a*c,b*c};
  F0D = simplicialComplex {a,b,c};
  K = filteredComplex({F2D,F1D,F0D}, ReducedHomology => false);
  E = prune spectralSequence K;
  e = spectralSequence K;
  assert(all(keys support E^0, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,0)))
  assert(all(keys support E^1, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,1)))
  assert(all(keys support E^2, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,2)))
  assert(all(keys support E^3, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,3)))
  assert(all(keys support E^4, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,4)))
  assert(all(keys support E^5, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,5)))
  assert(all(keys support e^0, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,0)))
  assert(all(keys support e^1, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,1)))
  assert(all(keys support e^2, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,2)))
  assert(all(keys support e^3, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,3)))
  assert(all(keys support e^4, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,4)))
  assert(all(keys support e^5, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,5)))
///

TEST ///
  -- The following example is taken from p. 127, Fig 7.2 of
  -- Zomorodian's "Topology for computing"
  A = ZZ [s,t,u,v,w] ;
  d0 = simplicialComplex {s};
  d1 = simplicialComplex {s,t} ;
  d2 = simplicialComplex {s,t,u} ;
  d3 = simplicialComplex {s*t, u} ;
  d4 = simplicialComplex {s*t, u, v} ;
  d5 = simplicialComplex {s*t, u, v, w} ;
  d6 = simplicialComplex {s*t, s*w ,u, v} ;
  d7 = simplicialComplex {s*t, s*w ,t * w, u, v} ;
  d8 = simplicialComplex {s*t, s*w ,t * w, u * v};
  d9 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v};
  d10 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u};
  d11 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w};
  d12 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u};
  d13 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w};
  d14 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w};
  d15 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u};
  d16 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u, s*u*v};
  d17 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u, s*u*v, s*t*w};
  L = reverse {d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17};
  K = filteredComplex (L, ReducedHomology => false);
  E = prune spectralSequence K
  e = spectralSequence K
  assert(all(keys support E^0, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,0)))
  assert(all(keys support E^1, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,1)))
  assert(all(keys support E^2, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,2)))
  assert(all(keys support E^3, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,3)))
  assert(all(keys support E^4, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,4)))
  assert(all(keys support E^5, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,5)))
  assert(all(keys support E^6, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,6)))
  assert(all(keys support E^7, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,7)))
  assert(all(keys support E^8, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,8)))
  assert(all(keys support E^9, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,9)))
  assert(all(keys support E^10, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,10)))
  assert(all(keys support E^11, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,11)))
  assert(all(keys support E^12, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,12)))
  assert(all(keys support e^0, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,0)))
  assert(all(keys support e^1, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,1)))
  assert(all(keys support e^2, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,2)))
  assert(all(keys support e^3, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,3)))
  assert(all(keys support e^4, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,4)))
  assert(all(keys support e^5, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,5)))
  assert(all(keys support e^6, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,6)))
  assert(all(keys support e^7, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,7)))
  assert(all(keys support e^8, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,8)))
  assert(all(keys support e^9, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,9)))
  assert(all(keys support e^10, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,10)))
  assert(all(keys support e^11, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,11)))
  assert(all(keys support e^12, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,12)))
///

TEST ///
  A = QQ[a,b,c,d];
  D = simplicialComplex {a*d*c, a*b, a*c, b*c};
  F2D = D;
  F1D = simplicialComplex {a*c, d};
  F0D = simplicialComplex {a,d};
  K = filteredComplex({F2D, F1D, F0D},ReducedHomology => false);
  E = spectralSequence(K);
  e = prune E;
  assert(all(keys support E^0, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,0)))
  assert(all(keys support E^1, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,1)))
  assert(all(keys support E^2, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,2)))
  assert(all(keys support E^3, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,3)))
  assert(all(keys support E^4, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,4)))
  assert(all(keys support E^5, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,5)))
  assert(all(keys support E^6, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,6)))
  assert(all(keys support E^7, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,7)))
  assert(all(keys support E^8, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,8)))
  assert(all(keys support E^9, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,9)))
  assert(all(keys support E^10, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,10)))
  assert(all(keys support E^11, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,11)))
  assert(all(keys support E^12, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,12)))
  assert(all(keys support e^0, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,0)))
  assert(all(keys support e^1, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,1)))
  assert(all(keys support e^2, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,2)))
  assert(all(keys support e^3, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,3)))
  assert(all(keys support e^4, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,4)))
  assert(all(keys support e^5, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,5)))
  assert(all(keys support e^6, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,6)))
  assert(all(keys support e^7, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,7)))
  assert(all(keys support e^8, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,8)))
  assert(all(keys support e^9, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,9)))
  assert(all(keys support e^10, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,10)))
  assert(all(keys support e^11, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,11)))
  assert(all(keys support e^12, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,12)))
///

TEST ///
  B = QQ[a..d];
  J = ideal vars B;
  C = res monomialCurveIdeal(B,{1,3,4});
  K = filteredComplex(J,C,4);
  e = prune spectralSequence K;
  assert(all(keys support e^0, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,0)))
  assert(all(keys support e^1, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,1)))
  assert(all(keys support e^2, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,2)))
  assert(all(keys support e^3, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,3)))
  assert(all(keys support e^4, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,4)))
///


TEST ///
  S = ZZ/101[x,y];
  I = ideal(x^2,x*y,y^2);
  R = S/I;
  kR = coker vars R;
  kS = coker vars S;
  CS = res kS;
  CR = res(kR,LengthLimit=>6);
  CS' = CS**R;
  E = prune spectralSequence (CS' ** filteredComplex CR);
  assert(all(keys support E^0, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,0)))
  assert(all(keys support E^1, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,1)))
  assert(all(keys support E^2, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,2)))
  assert(all(keys support E^3, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,3)))
  assert(all(keys support E^4, j -> isIsomorphism homologyIsomorphism(E,j#0,j#1,4)))
///
