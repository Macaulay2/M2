-- 0
TEST ///
rayList={{1,0},{0,1},{-1,-1},{0,-1}}
coneList={{0,1},{1,2},{2,3},{3,0}}
X = normalToricVariety(rayList,coneList)
-- toricCycle constructor
D1 = toricCycle({{2,3} => 1,{0,3} => 4}, X)
assert(D1 == X_{3,2} + 4*X_{3,0})
assert(toricCycle(X_0) == toricCycle({0 => 1}, X))
-- Scalar multiplication and addition
D2 = (-2)*D1
assert(D2 == toricCycle({{2,3} => -2,{0,3} => -8}, X))
assert(D1 + D2 == ((-1)*D1))
assert(D1 - D2 == 3*D1)
assert(-D1 == (-1)*D1)
///

-- 1
TEST ///
rayList={{1,0},{1,2},{-1,-1}}
coneList={{0,1},{1,2},{2,0}}
X = normalToricVariety(rayList,coneList)
-- toricCycle constructor
D1 = toricCycle({{0,2} => 1,{0,1} => 4}, X)
assert(D1 == X_{2,0} + 4*X_{1,0})
assert(toricCycle(X_0) == toricCycle({0 => 1}, X))
-- Scalar multiplication and addition
D2 = (-2)*D1
assert(D2 == toricCycle({{0,2} => -2,{0,1} => -8}, X))
assert(D1 + D2 == ((-1)*D1))
assert(D1 - D2 == 3*D1)
assert(-D1 == (-1)*D1)
///

-- 2
TEST ///
rayList={{1,0,0},{0,1,0},{0,0,1},{1,1,1},{-1,-1,-1}}
coneList={{0,1,3},{0,2,3},{1,2,3},{0,1,4},{0,2,4},{1,2,4}}
X = normalToricVariety(rayList,coneList)
-- toricCycle constructor
D1 = toricCycle({{0,1,2} => -1,{0,2,3} => 2}, X)
assert(D1 == -X_{1,2,0} + 2*X_{3,0,2})
assert(toricCycle(X_0) == toricCycle({0 => 1}, X))
-- Scalar multiplication and addition
D2 = (-2)*D1
assert(D2 == toricCycle({{1,2,0} => 2,{3,0,2} => -4}, X))
assert(D1 + D2 == ((-1)*D1))
assert(D1 - D2 == 3*D1)
assert(-D1 == (-1)*D1)
///

-- 3
TEST ///
rayList={{1,0},{0,1},{-1,-1},{0,-1}}
coneList={{0,1},{1,2},{2,3},{3,0}}
X = normalToricVariety(rayList,coneList)
D1 = X_3
D2 = X_2
-- isTransverse
assert(not isTransverse(D1,{3}))
assert(isTransverse(D1,{0}))
assert(not isTransverse(D2,{2}))
assert(isTransverse(D2,{0}))
-- makeTransverse
assert( makeTransverse(D1,{0,3}) == toricDivisor( toList(4:promote(0,QQ)),X) + X_1 - X_2 )
assert( makeTransverse(D2,{2,3}) == toricDivisor( toList(4:promote(0,QQ)),X) + X_0)
-- ToricDivisor * List
assert(D1*{3} == - toricCycle({({2,3} => 1)},X))
assert(D1*{2} == toricCycle({({2,3} => 1)},X))
assert(D1*{1} == toricCycle({},X))
assert(D1*{0} == toricCycle({({0,3} => 1)},X))
-- ToricDivisor * ToricCycle
assert(D1*X_{3} == - toricCycle({({2,3} => 1)},X))
assert(D1*(X_{2}+X_{3}) == toricCycle({},X))
///

--4
TEST ///
rayList={{1,0},{1,2},{-1,-1}}
coneList={{0,1},{1,2},{2,0}}
X = normalToricVariety(rayList,coneList)
D1 = X_0
D2 = X_2
-- isTransverse
assert(not isTransverse(D1,{0}))
assert(isTransverse(D1,{1}))
assert(not isTransverse(D2,{2}))
assert(isTransverse(D2,{0}))
-- makeTransverse
assert( makeTransverse(D1,{0,1}) == toricDivisor( toList(3:promote(0,QQ)),X) + (1/2) * X_2 )
assert( makeTransverse(D2,{2,0}) == toricDivisor( toList(3:promote(0,QQ)),X) + 2 * X_1)
-- ToricDivisor * List
assert(D1*{2} == toricCycle({({0,2} => 1)},X))
assert(D1*{0} == toricCycle({({0,2} => 1/2)},X))
assert(D1*{1} == toricCycle({({0,1} => 1/2)},X))
assert(D2*{2} == toricCycle({({1,2} => 2)},X))
-- ToricDivisor * ToricCycle
assert(D1*X_{2} == toricCycle({({0,2} => 1)},X))
assert(D1*(X_{0}+X_{1}) == toricCycle({{0,1} => 1/2, {0,2} => 1/2},X))
///

-- 5
TEST ///
rayList={{1,0,0},{0,1,0},{0,0,1},{1,1,1},{-1,-1,-1}}
coneList={{0,1,3},{0,2,3},{1,2,3},{0,1,4},{0,2,4},{1,2,4}}
X = normalToricVariety(rayList,coneList)
D1 = X_0
D2 = X_1
-- isTransverse
assert(not isTransverse(D1,{0}))
assert(isTransverse(D1,{1}))
assert(not isTransverse(D2,{1}))
assert(isTransverse(D2,{0}))
-- makeTransverse
assert( makeTransverse(D1,{0,1,2}) == toricDivisor( toList(5:promote(0,QQ)),X) - X_3 + X_4)
assert( makeTransverse(D2,{4,0,1}) == toricDivisor( toList(5:promote(0,QQ)),X) + X_2)
-- ToricDivisor * List
assert(D1*{0} == -X_{0,3}+X_{0,4})
assert(D1*{1} == X_{0,1})
assert(D1*{2} == X_{0,2})
assert(D1*{3} == X_{0,3})
assert(D1*{4} == X_{0,4})
-- ToricDivisor * ToricCycle
assert(D1*X_{3} == X_{0,3})
assert(D1*(X_{2}+X_{3}) == X_{0,2}+X_{0,3})
--degCycle
assert(degCycle(X_{0,1,2}) == 1)
assert(degCycle(X_{0,1,2} - X_{0,1,3}) == 0)
///
