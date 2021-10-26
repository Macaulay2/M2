export {
    "testMonomialOrderingToMatrix"
    }

debug Core
testMonomialOrderingToMatrix = () -> (
    R := QQ[vars(0..3)];
    mo := monomialOrderMatrix R;
    ans := (matrix {{1, 1, 1, 1}},
        RevLex,
        Position => Up, 
        "ComponentBefore" => -1);
    assert(mo === ans);
    -----------
    R = QQ[vars(0..3), MonomialOrder=>Lex];
    mo = monomialOrderMatrix R;
    ans = (map(ZZ^0, ZZ^4, {}),
        Lex,
        Position => Up, 
        "ComponentBefore" => -1);
    assert(mo === ans);
    -----------
    R = QQ[vars(0..3), MonomialOrder=>{Weights=>{1,2,3,4}, 2, Position=>Up, 2}];
    mo = monomialOrderMatrix R;
    ans = (matrix {{1, 2, 3, 4}, {1, 1, 0, 0}, {0, -1, 0, 0}, {0, 0, 1, 1}},
        RevLex,
        Position => Up,
        "ComponentBefore" => 3);
    assert(mo === ans);
    -----------
    R = QQ[];
    mo = monomialOrderMatrix R;
    ans = (map(ZZ^0, ZZ^0, {}), 
        RevLex, 
        Position => Up, 
        "ComponentBefore" => 0);  -- BUT: want ComponentBefore to be -1 here.
    assert(mo === ans);
    -----------
    R = QQ[vars(0..3), MonomialOrder=>{1, Weights=>{1,2,3,4}, Position=>Down}];
    mo = monomialOrderMatrix R; -- this one is NOT CORRECT YET...
    ans = (
        matrix {{1, 0, 0, 0}, {0, 1, 2, 3}, {0, 1, 1, 1}},
        RevLex,
        Position => Down,
        "ComponentBefore" => 2);
    assert(mo === ans);
    )

TEST ///
    testMonomialOrderingToMatrix()
///

testRawMGB = method()
testRawMGB Ideal := (I) -> (
     time G2 := flatten entries gens if isHomogeneous I then gb(I, Algorithm=>LinearAlgebra) else gb I;
     time G3 := groebnerBasis(I, Strategy=>"MGB"); -- flatten entries map(ring I, rawMGB(raw gens I, 0, 1, ""));
     time G4 := groebnerBasis(I, Strategy=>"F4"); -- flatten entries map(ring I, rawMGB(raw gens I, 1, 1, ""));
     assert(#G2 == #G3);
     assert(#G2 == #G4);
     assert G2 == G3;
     assert G2 == G4;
     )

TEST ///
  R = ZZ/101[a..d]
  I = ideal(a^2*b-c^2-1, 2*a*d-c, a^4-1)
  g1 = groebnerBasis(I, Strategy=>"MGB")
  g2 = groebnerBasis(I, Strategy=>"F4")
  assert(g1 == g2)

  -- the following cases are not covered by MGB
  -- But: it quietly changes to the default algorithm.
  R = QQ[a..d]
  I = ideal(a^2*b-c^2-1, 2*a*d-c, a^4-1)
  assert (try (g1 = groebnerBasis(I, Strategy=>"MGB"); true) else false)
  
  -- This one quietly uses the default algorithm too
  R = ZZ/101[a..d]/(a^2+b^2)
  I = ideal(a^2*b-c^2-1, 2*a*d-c, a^4-1)
  assert (try (g1 = groebnerBasis(I, Strategy=>"MGB"); true) else false)
///

TEST ///
  -- module orders
  R = ZZ/101[a..d]
  m = matrix"ad-1,ba-c,c;a,d-1,b-1" 
  g1 = gens gb m
  leadTerm oo
  g2 = groebnerBasis(m, Strategy=>"MGB")
  assert(g1 == g2)
///

if false then
TEST ///
 -- this next example FAILS: I think that MGB is trying to use lcm criterion for modules...
  R = ZZ/101[a..d, MonomialOrder=>{Position=>Down, 4}]
  --monomialOrderMatrix R
  m = matrix"ad-1,ba-c,c;a,d-1,b-1" 
  g1 = gens gb m
  leadTerm oo
  g2 = groebnerBasis(m, Strategy=>"MGB") -- this appears incorrect
  assert(g1 == g2)
  time g3 = groebnerBasis(m, Strategy=>"F4", "MGBOptions"=>{"Log"=>"all"})
  time g3 = groebnerBasis(m, Strategy=>"MGB", "MGBOptions"=>{"Log"=>"all"})
///

if false then
TEST ///
  -- module orders
  R = ZZ/101[a..d,z]
  m = matrix"zad-z,zba-zc,zc;za,zd-z,bz-z" 
  g1 = gens gb m
  leadTerm oo
  g2 = groebnerBasis(m, Strategy=>"MGB")
  assert(g1 == g2)

  R = ZZ/101[a..d,z, MonomialOrder=>{Position=>Down, 5}]
  --monomialOrderMatrix R
  m = matrix"zad-z,zba-zc,zc;za,zd-z,bz-z" 
  g1 = gens gb m
  leadTerm oo
  g2 = groebnerBasis(m, Strategy=>"MGB") -- this appears incorrect
  assert(g1 == g2)
  time g3 = groebnerBasis(m, Strategy=>"F4", "MGBOptions"=>{"Log"=>"all"})
  assert(g1 == g3)
///

if false then
TEST ///
  -- let's compute syzygies using module orders
  R = ZZ/101[vars(0..7)]
  I = ideal(a*b*c-d^3, a*c*e-d*f^2, a*e^2-c*d^2)
  syz gens I
  g1 = gens gb I
  g2 = groebnerBasis(I, Strategy=>"MGB")
  assert(g1 == g2)
  
  R = ZZ/101[vars(0..7), MonomialOrder=>{Position=>Up, 8}]
  M = id_(R^3) || sub((gens I), R)
  g2 = groebnerBasis(M, Strategy=>"MGB", "MGBOptions"=>{"Log"=>"all"}) -- CRASHES
  g2 = groebnerBasis(M, Strategy=>"MGB") -- CRASHES
  g2 = groebnerBasis(M, Strategy=>"F4", "MGBOptions"=>{"Log"=>"all"}) -- CRASHES
  
///

  -- Here is the file we need to use to create the input to mgb for this example
///
    101
      8
      revlex, lex or schreyer
      <number of gradings, including component>
      (for each component:)
      [component or revcomponent] or [w1 w2 ... wn], n = #vars
      
    3
    abc<3>-d3<3>-<0>
    ace<3>-df2<3>-<1>
    ae2<3>-cd2<3>-<2>
///

TEST ///
  R1 = ZZ/32003[w,x,y,z,MonomialOrder => Lex]
  J1 = ideal"
    -2w2+9wx+8x2+9wy+9xy+6y2-7wz-3xz-7yz-6z2-4w+8x+4y+8z+2,
    3w2-5wx+4x2-3wy+2xy+9y2-6wz-2xz+6yz+7z2+9w+7x+5y+7z+5,
    7w2+5wx+2x2+3wy+9xy-4y2-5wz-7xz-5yz-4z2-5w+4x+6y-9z+2,
    8w2+5wx+5x2-4wy+2xy+7y2+2wz-7xz-8yz+7z2+3w-7x-7y-8z+8"
  time g2 = groebnerBasis(J1, Strategy=>"F4", "MGBOptions"=>{"Log"=>"all"})
  time g2 = groebnerBasis(ideal J1_*, Strategy=>"MGB");
  time g3 = groebnerBasis(ideal J1_*, Strategy=>"F4");
  time g1 = gens gb ideal(J1_*);
  assert(g1 == g2)
  assert(g1 == g3)

  R1 = ZZ/32003[w,x,y,z,MonomialOrder => {1,1,1,1}] -- this one is much slower for F4, seems
    -- TODO: to give more complicated GB elements?? why?
  J1 = ideal"
    -2w2+9wx+8x2+9wy+9xy+6y2-7wz-3xz-7yz-6z2-4w+8x+4y+8z+2,
    3w2-5wx+4x2-3wy+2xy+9y2-6wz-2xz+6yz+7z2+9w+7x+5y+7z+5,
    7w2+5wx+2x2+3wy+9xy-4y2-5wz-7xz-5yz-4z2-5w+4x+6y-9z+2,
    8w2+5wx+5x2-4wy+2xy+7y2+2wz-7xz-8yz+7z2+3w-7x-7y-8z+8"
  time g2 = groebnerBasis(J1, Strategy=>"MGB");
  time g3 = groebnerBasis(J1, Strategy=>"F4");
  --time groebnerBasis(J1, Strategy=>"F4", "MGBOptions"=>{"Log"=>"all"})
  time g1 = gens gb (ideal J1_*);
  assert(g1 == g2)
  assert(g1 == g3)
  
  R1 = ZZ/32003[w,x,y,z,MonomialOrder => Eliminate 3]
  J1 = sub(J1, R1)
  time g2 = groebnerBasis(J1, Strategy=>"MGB");
  time g3 = groebnerBasis(J1, Strategy=>"F4");
  time g1 = gens gb J1;
  assert(g1 == g2)
  assert(g1 == g3)
///

-- TODO:
--   a. if mathicgb throws an exception, we need to catch it (FIXED)
--   b. ^C doesn't seem to have any effect.
--     actually, it does.  Sometimes just takes awhile.
--     can we get any useful info out of it, or just quit.
--     need to give an error message (interrupted).
--   c. I recall a problem with module orders, but need to track that down (so far, haven't found it).
--   d. need to handle all module orders
--   e. use module orders to find syzygies, and I:f
--   f. we need to make certain the monomial order used in MGB matches what we think.
--   g. interface isn't perfect.
--     - always call forceGB?  i.e. allow an option to bypass this
--     - stash the result?
--     - expose more aspects of the mgb computation (e.g. other reducers, and sig GB)
