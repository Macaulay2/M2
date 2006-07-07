-- MES yanked this from test/engine/LU.m2, 6/19/06
debug Core
R = QQ
m = matrix(R, {{1,2,3,4,5},
	  {2,3,4,5,1},
	  {3,4,5,1,2},
	  {4,5,1,2,3},
	  {5,1,2,3,4}})
m1 = mutableMatrix(m, Dense=>false)
rawFFLU raw m1
m1  -- crashes
det m
det matrix m1

