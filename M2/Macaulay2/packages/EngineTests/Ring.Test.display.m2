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

