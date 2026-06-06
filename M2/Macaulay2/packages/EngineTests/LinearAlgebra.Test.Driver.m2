-- -*- coding: utf-8 -*-

load "./LinearAlgebra.Test.Base.m2"
load "./LinearAlgebra.Test.FFPACK.m2"
load "./LinearAlgebra.Test.FLINT.m2"
load "./LinearAlgebra.Test.ZZp.m2"
load "./LinearAlgebra.Test.EngineGF.m2"

load "./LinearAlgebra.Test.all.m2"

load "EngineTests/LinearAlgebra.Test.eigen.m2"

if hasFFPACK then 
TEST ///
  -- Which rings have linear algebra routines defined?
  debug Core
  hasLinAlgRank = (R) -> (
      M = mutableMatrix(R, 4, 4);
      fillMatrix M;
      rawLinAlgRank raw M
      );

  hasEngineLinearAlgebra(ZZ)
  assert hasEngineLinearAlgebra(ZZFlint)
  assert hasEngineLinearAlgebra(QQ)
  assert hasEngineLinearAlgebra(ZZp(101, Strategy=>"Flint"))
  assert hasEngineLinearAlgebra(ZZp(101, Strategy=>"Ffpack"))
  hasEngineLinearAlgebra(ZZ/101)
  hasEngineLinearAlgebra (GF(2^3, Strategy=>null))
  hasEngineLinearAlgebra (GF(2^3, Strategy=>"Flint"))
  hasEngineLinearAlgebra (GF(2^3, Strategy=>"FlintBig"))
  hasEngineLinearAlgebra (GF(2^3, Strategy=>"Old"))
  hasEngineLinearAlgebra (GF(2^3, Strategy=>"New"))

  hasLinAlgRank ZZ  -- NO
  hasLinAlgRank QQ  -- NO
  hasLinAlgRank (ZZp(101, Strategy=>"Flint")) -- yes, this one works!
  hasLinAlgRank (ZZp(101, Strategy=>"Ffpack")) -- yes, this one works!
  hasLinAlgRank (ZZp(101, Strategy=>null)) -- NO

  debug Core
  initializeEngineLinearAlgebra QQ
///


TEST ///
  -- eigenvalues
  M = mutableMatrix(CC_100,3,3)
  M = matrix fillMatrix M
  eigenvalues M
  LUdecomposition M
///

