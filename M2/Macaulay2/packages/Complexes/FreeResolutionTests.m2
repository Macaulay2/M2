-- Tests of free resolution code

TEST ///
-*
  restart
  needsPackage "Complexes"
*-  
  S = ZZ/101[a..d]
  I = ideal(a*b-c*d, a^3-c^3, a*b^2-c*d^2)
  M = S^1/I
  
  -- default: resolution Strategy => 1
  F1 = freeResolution M
  assert isWellDefined F1
  assert(prune HH F1 == complex M) -- only guaranteed to be isomorphic?
  B1 = betti F1

  I = ideal I_*
  F0 = freeResolution(I, Strategy => 0)
  assert isWellDefined F0
  assert(prune HH F0 == complex M) -- only guaranteed to be isomorphic?
  assert(B1 === betti F0)

  I = ideal I_*
  F2 = freeResolution(I, Strategy => 2)
  assert isWellDefined F2
  assert(prune HH F2 == complex M) -- only guaranteed to be isomorphic?
  assert(B1 === betti F2)
  assert(dd^F2_1 == gens I) -- Strategy=>2 preserves the generators

  I = ideal I_*
  F3 = freeResolution(I, Strategy => 3)
  assert isWellDefined F3
  assert(prune HH F3 == complex M) -- only guaranteed to be isomorphic?
  assert(B1 === betti F3)
  assert(dd^F3_1 == gens I) -- Strategy=>3 preserves the generators
  
  I = ideal I_*
  Fz = freeResolution(I, Strategy => "Syzygies")
  assert isWellDefined Fz
  assert(prune HH Fz == complex M) -- only guaranteed to be isomorphic?
  assert(B1 === betti Fz)

  I = ideal I_*
  Fe = freeResolution(I, Strategy => Engine)
  assert isWellDefined Fe
  assert(prune HH Fe == complex M) -- only guaranteed to be isomorphic?
  assert(B1 === betti Fe)

  -- Homogenization, ZZ, Field don't handle this input
  I = ideal I_*
  assert try(freeResolution(I, Strategy => "Homogenization"); false) else true
  assert try(freeResolution(I, Strategy => "ZZ"); false) else true
  assert try(freeResolution(I, Strategy => "Field"); false) else true

  I = ideal I_*
  Fn = freeResolution(I, Strategy => "Nonminimal")
  assert isWellDefined Fn
  assert(prune HH Fn == complex M) -- only guaranteed to be isomorphic?
  assert(B1 =!= betti Fn) -- Fn is non minimal in this example
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-  
  S = ZZ/1048583[vars(0..9)]
  I = ideal(a^2-b^2, a*b*c, a^3-b*c*e, b^4-a*h^3, a*f*g*h^2-b*c*d*j^2)
  assert(codim I === 2)
  F = freeResolution I

  -- default: resolution Strategy => 1
  I = ideal I_*
  C = freeResolution(I, LengthLimit => 3)
  assert isWellDefined C
  assert(length C == 3)
  C = freeResolution(I, LengthLimit => 4)
  assert isWellDefined C
  assert(length C == 4)
  C = freeResolution(I, LengthLimit => 3)
  assert isWellDefined C  
  assert(length C == 3)

  I = ideal I_*
  C = freeResolution(I, LengthLimit => 3, Strategy => 0)
  assert isWellDefined C
  assert(length C == 3)
  C = freeResolution(I, LengthLimit => 4, Strategy => 0)
  assert isWellDefined C
  assert(length C == 4)
  C = freeResolution(I, LengthLimit => 3, Strategy => 0)
  assert isWellDefined C  
  assert(length C == 3)

  I = ideal I_*
  C = freeResolution(I, LengthLimit => 3, Strategy => 2)
  assert isWellDefined C
  assert(length C == 3)
  C = freeResolution(I, LengthLimit => 4, Strategy => 2)
  assert isWellDefined C
  assert(length C == 4)
  C = freeResolution(I, LengthLimit => 3, Strategy => 2)
  assert isWellDefined C  
  assert(length C == 3)

  I = ideal I_*
  C = freeResolution(I, LengthLimit => 3, Strategy => 3)
  assert isWellDefined C
  assert(length C == 3)
  C = freeResolution(I, LengthLimit => 4, Strategy => 3)
  assert isWellDefined C
  assert(length C == 4)
  C = freeResolution(I, LengthLimit => 3, Strategy => 3)
  assert isWellDefined C  
  assert(length C == 3)

  I = ideal I_*
  C = freeResolution(I, LengthLimit => 3, Strategy => "Syzygies")
  assert isWellDefined C
  assert(length C == 3)
  C = freeResolution(I, LengthLimit => 4, Strategy => "Syzygies")
  assert isWellDefined C
  assert(length C == 4)
  C = freeResolution(I, LengthLimit => 3, Strategy => "Syzygies")
  assert isWellDefined C  
  assert(length C == 3)

  I = ideal I_*
  -- currently this fails, as nonminimal resolutions require too small of a prime?
  -- TODO: remove the next 2 lines once larger primes work.
  S = ZZ/32003[vars(0..9)]
  I = ideal(a^2-b^2, a*b*c, a^3-b*c*e, b^4-a*h^3, a*f*g*h^2-b*c*d*j^2)


  C = freeResolution(I, LengthLimit => 3, Strategy => "Nonminimal")
  assert isWellDefined C
  assert(length C == 3)
  C = freeResolution(I, LengthLimit => 4, Strategy => "Nonminimal")
  assert isWellDefined C
  assert(length C == 4)
  C = freeResolution(I, LengthLimit => 3, Strategy => "Nonminimal")
  assert isWellDefined C  
  assert(length C == 3)
///

TEST ///
  -- originally from tests/normal/res.m2
-*
  restart
  needsPackage "Complexes"
*-  
  R = ZZ/101[a .. e]/(c^4,b^3)
  m = matrix {
     { -14*e, -38*e, 0, 25*a*d, 24*d, -49*d, 0, -38*d, -47*d, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -12*a*d-24*b*e-26*c*e, 14*b*e, 38*b*e, 7*b*d-5*c*d, -31*a*b*d+38*a*c*d, 44*a*b*d-7*a*c*d, -25*a*b*d, -c^2, -46*b*c-16*c^2, -7*b*c-2*c^2, -11*b*c+38*c^2, 6*b*c-14*c^2, -38*b*c-18*c^2, -19*b^2+38*b*c, 0, 0, 0, 0, 0, 0, 7*a*b*d-6*a*c*d-48*b*c*e+21*c^2*e, -a*b*d+21*a*c*d-19*b*c*e+38*c^2*e, 12*a*b*d+19*b*c*e-42*c^2*e, 18*b^2*d+26*b*c*d-17*c^2*d, 7*b^2*d+26*b*c*d-38*c^2*d, -6*a*b^2*d-2*a*b*c*d-18*a*c^2*d, 38*a*b^2*d+45*a*b*c*d, 7*b*c^2-5*c^3, 18*b^2*c+26*b*c^2-17*c^3, 26*b^2*c-17*b*c^2-10*c^3, -50*c^3, -7*a*b*d^2+6*a*c*d^2+7*b^2*d*e+43*b*c*d*e-21*c^2*d*e, 10*a*b^2*d-28*a*b*c*d-16*b^2*c*e-3*b*c^2*e+22*c^3*e, 2*a*b^2*d-23*a*b*c*d-38*b^2*c*e-45*b*c^2*e+16*c^3*e, 29*b^2*c*d+37*b*c^2*d-26*c^3*d, 0, 0, 12*a*c^3*d, a*c^3*d, 23*a*c^3*d},
     { 0, 41*e, -48*e, -30*a*d, 19*d, 15*d, 48*d, 17*d, -35*d, 41*b, 48*b, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7*a*d-37*c*e, 0, 0, 22*c*d, 44*a*c*d, -18*a*c*d, 0, 0, 11*c^2, 0, -11*c^2, 3*c^2, -41*b*c+49*c^2, -14*b*c+33*c^2, 0, 0, 0, 0, 0, 0, -40*a*c*d+14*c^2*e, -23*a*c*d-14*c^2*e, -7*c^2*e, -30*b*c*d-21*c^2*d, 22*b*c*d-44*c^2*d, 10*a*b*c*d-9*a*c^2*d, 4*a*b*c*d+33*a*c^2*d, 22*c^3, -30*b*c^2-21*c^3, -30*b^2*c-21*b*c^2+12*c^3, -c^3, 40*a*c*d^2+22*b*c*d*e-14*c^2*d*e, 17*a*b*c*d+13*a*c^2*d-7*b*c^2*e+14*c^3*e, -37*a*b*c*d-23*a*c^2*d-4*b*c^2*e+c^3*e, 19*b*c^2*d+11*c^3*d, 0, 0, -7*a*c^3*d, -39*a*c^3*d, -9*a*c^3*d},
     { 0, 0, e, -26*a*d, 7*d, 5*d, -d, -50*d, -41*d, -41*c, -b, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 41*a*d, 0, -41*c*e, 42*c*d, -27*a*c*d, -9*a*c*d, -35*a*c*d, 0, 0, 0, 0, 0, -27*b*c+14*c^2, 17*b*c-36*c^2, 0, 0, 0, 0, 0, 0, 6*a*c*d, 23*a*c*d+17*c^2*e, 45*a*c*d+12*c^2*e, 7*b*c*d-17*c^2*d, 42*b*c*d+27*c^2*d, -36*a*b*c*d+13*a*c^2*d, 26*a*b*c*d-36*a*c^2*d, 42*c^3, 7*b*c^2-17*c^3, 7*b^2*c-17*b*c^2-30*c^3, 0, -6*a*c*d^2+42*b*c*d*e, -41*a*b*c*d-3*a*c^2*d+5*b*c^2*e-35*c^3*e, 12*a*b*c*d-39*a*c^2*d-26*b*c^2*e+48*c^3*e, -28*b*c^2*d+23*c^3*d, 0, 0, -41*a*c^3*d, 36*a*c^3*d, -29*a*c^3*d},
     { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
     { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
     { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
     { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
     }
  M = coker m
  isHomogeneous M
  C = freeResolution(M, LengthLimit => 6)
  assert( betti C === new BettiTally from {(0, {0}, 0) => 7, (1, {0}, 0) => 3, (1, {1}, 1) => 12, (1, {2}, 2) => 5, (2, {2}, 2) => 13,
	  (2, {3}, 3) => 18, (2, {4}, 4) => 4, (3, {3}, 3) => 6, (3, {4}, 4) => 25, (3, {5}, 5) => 17,
	  (3, {6}, 6) => 5, (4, {4}, 4) => 1, (4, {5}, 5) => 17, (4, {6}, 6) => 31, (4, {7}, 7) => 18,
	  (4, {8}, 8) => 4, (5, {6}, 6) => 6, (5, {7}, 7) => 31, (5, {8}, 8) => 30, (5, {9}, 9) => 17,
	  (5, {10}, 10) => 5, (6, {7}, 7) => 1, (6, {8}, 8) => 18, (6, {9}, 9) => 35, (6, {10}, 10) => 31,
	  (6, {11}, 11) => 18, (6, {12}, 12) => 4})

  m = map(R^{2,2,2,0,0,0,0},,m)
  assert isHomogeneous m
  M = coker m
  isHomogeneous M
  C = freeResolution(M, LengthLimit => 6)
///

TEST ///
  -- originally from tests/normal/res.m2
  A=QQ[x,y] 
  C = complex{
      map(A^1,A^{3:-2},{{x^2,x*y,y^2}}),
      map(A^{3:-2},A^{2:-3},{{y,0},{-x,y},{0,-x}})
      }
  assert isWellDefined C
  M = HH_0 C 
  C1 = freeResolution M
  dd^C1
  dd^C
  freeResolution M === C

  -- TODO: a function to install a free resolution of a module
  --  res M = C 
  --  assert( res M === C )

///
