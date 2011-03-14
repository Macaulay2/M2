booleanRing = method()
booleanRing ZZ := (nvars) -> (
   --R1 := ZZ/2[vars(0..nvars-1)];
   R1 := ZZ/2[vars(0..nvars-1), MonomialOrder=>Lex];
   L := ideal apply(gens R1, x -> x^2+x);
   R1/L
)

-- make 3 rings R0, R1, R2
doMyExample = (I) -> (
  R := ring I;
  R0 = newRing(R, MonomialOrder=>GRevLex);
  
  --S1 = (coefficientRing R)[gens R, z];
  --L1 = sub(ideal R, S1);
  --R1 = S1/homogenize(L1,z);

  S2 = (coefficientRing R)[gens R, z, MonomialOrder=>Lex];
  L2 = sub(ideal R, S2);
  R2 = S2/homogenize(L2,z);

  I0 = sub(I,R0);
  --t1 := timing(G0 = gens gb (I0, Strategy=>LongPolynomial) );
  t1 := timing(G0 = gens gb (I0, Algorithm=>Sugarless, Strategy=>LongPolynomial) );
  LT = sub(leadTerm G0, R2);
  GLT := forceGB LT;
  t2 := timing(hf = poincare coker gens GLT);
  --t2 := timing(hf = poincare ideal LT); -- can improve, by not doing GB in R2
  I2 = homogenize(sub(G0, R2), z);
  t3 := timing (G2 = gens gb(I2, Hilbert=>hf));
  G2d = sub(G2, vars R | matrix{{1_R}});
  << "   " << "grevlex gb: " << t1#0 << " sec, hf: " << t2#0 << " sec, lexgb: " << t3#0 << " sec" << endl;
  G2dred = gens forceGB G2d
  )

eliminate1 = method()
eliminate1 (List, Ideal) := (v,I) -> (
     R := ring I;
     h := local h;
     S := (coefficientRing R)[gens R, h, MonomialSize => 8];
     IS := homogenize(sub(trim I,S), h);
     phi := map(R,S,vars R | matrix{{1_R}});
     v = v/(v1 -> sub(v1,S));
     eS := eliminate(v,IS);
     phi eS
     )

-- Game of Life polynomial if x1 has 8 neighbors x2..x9
cgl = (x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1*x2*x3*x4*x5*x6*x7*x8+x1*x2*x3*x4*x5*x6*x7*x9+x1*x2*x3*x4*x5*x6*x8*x9+x1*x2*x3*x4*x5*x7*x8*x9+x1*x2*x3*x4*x6*x7*x8*x9+x1*x2*x3*x5*x6*x7*x8*x9+x1*x2*x4*x5*x6*x7*x8*x9+x1*x3*x4*x5*x6*x7*x8*x9+x1*x2*x3*x4*x5*x6*x7+x1*x2*x3*x4*x5*x6*x8+x1*x2*x3*x4*x5*x7*x8+x1*x2*x3*x4*x6*x7*x8+x1*x2*x3*x5*x6*x7*x8+x1*x2*x4*x5*x6*x7*x8+x1*x3*x4*x5*x6*x7*x8+x2*x3*x4*x5*x6*x7*x8+x1*x2*x3*x4*x5*x6*x9+x1*x2*x3*x4*x5*x7*x9+x1*x2*x3*x4*x6*x7*x9+x1*x2*x3*x5*x6*x7*x9+x1*x2*x4*x5*x6*x7*x9+x1*x3*x4*x5*x6*x7*x9+x2*x3*x4*x5*x6*x7*x9+x1*x2*x3*x4*x5*x8*x9+x1*x2*x3*x4*x6*x8*x9+x1*x2*x3*x5*x6*x8*x9+x1*x2*x4*x5*x6*x8*x9+x1*x3*x4*x5*x6*x8*x9+x2*x3*x4*x5*x6*x8*x9+x1*x2*x3*x4*x7*x8*x9+x1*x2*x3*x5*x7*x8*x9+x1*x2*x4*x5*x7*x8*x9+x1*x3*x4*x5*x7*x8*x9+x2*x3*x4*x5*x7*x8*x9+x1*x2*x3*x6*x7*x8*x9+x1*x2*x4*x6*x7*x8*x9+x1*x3*x4*x6*x7*x8*x9+x2*x3*x4*x6*x7*x8*x9+x1*x2*x5*x6*x7*x8*x9+x1*x3*x5*x6*x7*x8*x9+x2*x3*x5*x6*x7*x8*x9+x1*x4*x5*x6*x7*x8*x9+x2*x4*x5*x6*x7*x8*x9+x3*x4*x5*x6*x7*x8*x9+x1*x2*x3*x4+x1*x2*x3*x5+x1*x2*x4*x5+x1*x3*x4*x5+x1*x2*x3*x6+x1*x2*x4*x6+x1*x3*x4*x6+x1*x2*x5*x6+x1*x3*x5*x6+x1*x4*x5*x6+x1*x2*x3*x7+x1*x2*x4*x7+x1*x3*x4*x7+x1*x2*x5*x7+x1*x3*x5*x7+x1*x4*x5*x7+x1*x2*x6*x7+x1*x3*x6*x7+x1*x4*x6*x7+x1*x5*x6*x7+x1*x2*x3*x8+x1*x2*x4*x8+x1*x3*x4*x8+x1*x2*x5*x8+x1*x3*x5*x8+x1*x4*x5*x8+x1*x2*x6*x8+x1*x3*x6*x8+x1*x4*x6*x8+x1*x5*x6*x8+x1*x2*x7*x8+x1*x3*x7*x8+x1*x4*x7*x8+x1*x5*x7*x8+x1*x6*x7*x8+x1*x2*x3*x9+x1*x2*x4*x9+x1*x3*x4*x9+x1*x2*x5*x9+x1*x3*x5*x9+x1*x4*x5*x9+x1*x2*x6*x9+x1*x3*x6*x9+x1*x4*x6*x9+x1*x5*x6*x9+x1*x2*x7*x9+x1*x3*x7*x9+x1*x4*x7*x9+x1*x5*x7*x9+x1*x6*x7*x9+x1*x2*x8*x9+x1*x3*x8*x9+x1*x4*x8*x9+x1*x5*x8*x9+x1*x6*x8*x9+x1*x7*x8*x9+x1*x2*x3+x1*x2*x4+x1*x3*x4+x2*x3*x4+x1*x2*x5+x1*x3*x5+x2*x3*x5+x1*x4*x5+x2*x4*x5+x3*x4*x5+x1*x2*x6+x1*x3*x6+x2*x3*x6+x1*x4*x6+x2*x4*x6+x3*x4*x6+x1*x5*x6+x2*x5*x6+x3*x5*x6+x4*x5*x6+x1*x2*x7+x1*x3*x7+x2*x3*x7+x1*x4*x7+x2*x4*x7+x3*x4*x7+x1*x5*x7+x2*x5*x7+x3*x5*x7+x4*x5*x7+x1*x6*x7+x2*x6*x7+x3*x6*x7+x4*x6*x7+x5*x6*x7+x1*x2*x8+x1*x3*x8+x2*x3*x8+x1*x4*x8+x2*x4*x8+x3*x4*x8+x1*x5*x8+x2*x5*x8+x3*x5*x8+x4*x5*x8+x1*x6*x8+x2*x6*x8+x3*x6*x8+x4*x6*x8+x5*x6*x8+x1*x7*x8+x2*x7*x8+x3*x7*x8+x4*x7*x8+x5*x7*x8+x6*x7*x8+x1*x2*x9+x1*x3*x9+x2*x3*x9+x1*x4*x9+x2*x4*x9+x3*x4*x9+x1*x5*x9+x2*x5*x9+x3*x5*x9+x4*x5*x9+x1*x6*x9+x2*x6*x9+x3*x6*x9+x4*x6*x9+x5*x6*x9+x1*x7*x9+x2*x7*x9+x3*x7*x9+x4*x7*x9+x5*x7*x9+x6*x7*x9+x1*x8*x9+x2*x8*x9+x3*x8*x9+x4*x8*x9+x5*x8*x9+x6*x8*x9+x7*x8*x9

-- get index of neighbors for this labeling and periodic bc
-- 0  1  2  3 
-- 4  5  6  7
-- 8  9  10 11
-- 12 13 14 15
getNeighbors = method()
getNeighbors(ZZ,ZZ) := List => (i,n) -> (
  x := floor (i / n);
  y := i % n;
  l := { i,
      x*n + (y+1) %n,
      ( (x+1) %n )*n + (y+1) %n,
      ( (x+1) %n )*n + y,
      ( (x+1) %n )*n + (y-1)%n,
      x*n + (y-1)%n,
      ( (x-1) %n )*n + (y-1)%n,
      ( (x-1) %n )*n + y,
      ( (x-1) %n )*n + (y+1)%n
      }
)

end 

restart
load "GameOfLife.m2";
numvars := 5; -- make an nxn grid with periodic boundary conditions
R = booleanRing(numvars*numvars);
F = apply( numvars*numvars, j ->  cgl (toSequence apply (getNeighbors(j,numvars), i -> R_i)))
-- Fixed points
I = ideal( apply( #F, i-> F#i - R_i) )
gbTrace = 3
--time gbBoolean I
J = time doMyExample I

restart
load "GameOfLife.m2";
numvars := 6; -- make an nxn grid with periodic boundary conditions
R = booleanRing(numvars*numvars);
F = apply( numvars*numvars, j ->  cgl (toSequence apply (getNeighbors(j,numvars), i -> R_i)))
-- Fixed points
I = ideal( apply( #F, i-> F#i - R_i) )
gbTrace = 3
--time gbBoolean I
J = time doMyExample I


L = time primaryDecomposition ideal flatten entries J
apply(L, i-> gens i)
numvars := 4; -- make an nxn grid with periodic boundary conditions
numvars := 3; -- make an nxn grid with periodic boundary conditions
R = booleanRing(numvars*numvars);
F = apply( numvars*numvars, j ->  cgl (toSequence apply (getNeighbors(j,numvars), i -> R_i)))
-- Fixed points
I = ideal( apply( #F, i-> F#i - R_i) )
gbTrace = 3
--time gbBoolean I
J = time doMyExample I
L = time primaryDecomposition ideal flatten entries J
apply(L, i-> gens i)

L = flatten value get "tmp.txt"

J = ideal L
apply(flatten entries gens I, i -> i % J)

apply(L, ff -> sub(ff, {a=>1, 
  b=>1_R, 
  c=>1, 
  d=>1, 
  e=>0, 
  f=>0, 
  g=>0, 
  h=>0, 
  i=>1, 
  j=>1, 
  k=>1, 
  l=>1, 
  m=>0, 
  n=>0, 
  o=>0, 
  p=>0
  })
)



restart
load "GameOfLife.m2";
numvars := 3; -- make an nxn grid with periodic boundary conditions
R = booleanRing(numvars*numvars);
F = apply( numvars*numvars, j ->  cgl (toSequence apply (getNeighbors(j,numvars), i -> R_i)))
-- all cells dead
I = ideal( apply( #F, i-> F#i) )
time doMyExample I
-- all cells alive
I = ideal( apply( #F, i-> F#i-1) )
time doMyExample I

restart
load "GameOfLife.m2";
numvars := 4; -- make an nxn grid with periodic boundary conditions
R = booleanRing(numvars*numvars);
F = apply( numvars*numvars, j ->  cgl (toSequence apply (getNeighbors(j,numvars), i -> R_i)))
-- configuration as in Y
Y = {0,0,0,0,0,1,1,0,0,1,1,0,0,0,0,0}
I = ideal( apply( #F, i-> F#i - Y#i) )
gbTrace = 3
time doMyExample I
