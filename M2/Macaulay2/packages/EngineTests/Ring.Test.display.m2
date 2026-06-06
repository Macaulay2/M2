TEST ///
  -- this was a bug for a long time, fixed 2 June 2014, linalg branch
  debug Core;

  R = GF(2,4)
  testit = (R) -> (
      S = R[x,y];
      F = a*x^3+(a^3+a)*(y+1)+x;
      M = mutableMatrix {{F}};
      assert(net M ==        "| ax3+x+(a3+a)y+a3+a |");
      assert(net matrix M == "| ax3+x+(a3+a)y+a3+a |");
      assert(toString raw F == "ax3+x+(a3+a)y+a3+a");
      );
  testit(GF(2,4))
  testit(GF(2,4,Strategy=>"New"))
  testit(GF(2,4,Strategy=>"Old"))
  testit(GF(2,4,Strategy=>"Flint"))
  testit(GF(2,4,Strategy=>"FlintBig"))
///

TEST ///
  -- test that p_one does the right thing
  debug Core

  testit = (R) -> (
      S := R[x];
      f := x^2 - x + 1;
      assert Equation(toString raw f, "x2-x+1"))

  testit ZZ
  testit(ZZ/101)
  testit GF(3,4)
  testit QQ
  testit RR
  testit CC
  testit CC_100
  testit(ZZ[y])
///

TEST ///
  -- test that p_parens does the right thing
  debug Core

  testit = (R) -> (
      S := R[x];
      f := x^3 + (1 + ii)*x^2 + (ii - 1)*x + ii;
      assert Equation(toString raw f, "x3+(1+i)x2-(1-i)x+i"))

  testit CC
  testit CC_100
  testit(CC[y])
  testit(CC_100[y])
///
