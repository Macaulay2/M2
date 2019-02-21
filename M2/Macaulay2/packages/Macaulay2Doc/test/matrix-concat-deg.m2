-- unit test for preserving the degree and homogeneity of matrices under concatenation
-- for context, see issues #607 and #736

testConcatDegree = () -> (
  S  := ZZ[x,y];
  f0 := map(S^2,S^{1},matrix{{x},{y}},Degree=>{2});
  f1 := matrix{{x}};
  f2 := x*(id_(S^1));
  f3 := matrix{{x},{y}};
  f4 := map(S^{0,-1},S^1,matrix{{x^2},{y}},Degree=>{2});

  test := op -> (degree op, isHomogeneous(op), degrees op);

  -- sanity check for preserving the degree
  assert( test(f0 | f0) === ({2}, true, {{{0}, {0}}, {{-1}, {-1}}}) );
  -- f1 and f2 are homogeneous and so is the concatenation;
  -- the degree and twists are determined by the first operand
  assert( test(f1 | f2) === ({0}, true, {{{0}}, {{1}, {1}}}) );
  assert( test(f2 | f1) === ({1}, true, {{{0}}, {{0}, {0}}}) );
  -- f3 and f4 are homogeneous but incompatible
  assert( test(f3 | f4) === ({0}, false, {{{0}, {0}}, {{1}, {2}}}) );
  assert( test(f4 | f3) === ({2}, false, {{{0}, {1}}, {{0}, {-1}}}) );
  )
testConcatDegree()