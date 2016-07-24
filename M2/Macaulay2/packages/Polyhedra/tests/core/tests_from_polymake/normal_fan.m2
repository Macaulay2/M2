-- Test ambDim: 2, dim: 2, nvert: 4
-- Checking normal_fan + several booleans
TEST ///
verticesP = matrix {{0,1,0,1},{0,0,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
raysdesired = matrix {{0,1,0,-1},{1,0,-1,0}};
linealitydesired = map(QQ^2, QQ^0, 0);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
maxConesdesired = {{0,1},{0,3},{1,2},{2,3}};
desired = fan (raysdesired ,linealitydesired ,maxConesdesired);
computed = normalFan P;
assert(computed == desired)
assert(isPolytopal computed)
assert(isSmooth computed)
assert(isPure computed)
assert(isSimplicial computed)
assert(isComplete computed)
///

-- Test ambDim: 2, dim: 0, nvert: 1
-- Checking normal_fan + several booleans
TEST ///
verticesP = matrix {{3},{4}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
raysdesired = matrix {{0},{0}};
linealitydesired = matrix {{1,0},{0,1}};
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
maxConesdesired = {{}};
desired = fan (raysdesired ,linealitydesired ,maxConesdesired);
computed = normalFan P;
assert(computed == desired)
assert(isPolytopal computed)
assert(not isSmooth computed)
assert(isPure computed)
assert(isSimplicial computed)
assert(isComplete computed)
///

-- Test ambDim: 4, dim: 4, nvert: 11
-- Checking normal_fan + several booleans
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1,0,1,0},{0,0,1,1,0,0,1,1,0,0,1},{1,1,1,1,0,0,0,0,0,0,0},{0,0,0,0,1,1,1,1,0,0,0}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
raysdesired = matrix {{0,1,0,-1,0,0,-1,0},{0,0,1,-1,0,-1,0,0},{1,0,0,1,0,0,0,-1},{0,0,0,1,1,0,0,-1}};
linealitydesired = map(QQ^4, QQ^0, 0);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
maxConesdesired = {{1,2,4,7},{2,4,6,7},{1,4,5,7},{3,4,5,6,7},{0,1,2,7},{0,2,6,7},{0,1,5,7},{0,3,5,6,7},{0,1,2,4},{0,2,3,4,6},{0,1,3,4,5}};
desired = fan (raysdesired ,linealitydesired ,maxConesdesired);
computed = normalFan P;
assert(computed == desired)
assert(isPolytopal computed)
assert(not isSmooth computed)
assert(isPure computed)
assert(not isSimplicial computed)
assert(isComplete computed)
///

-- Test ambDim: 5, dim: 4, nvert: 11
-- Checking normal_fan + several booleans
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1,0,1,0},{0,0,1,1,0,0,1,1,0,0,1},{1,1,1,1,0,0,0,0,0,0,0},{0,0,0,0,1,1,1,1,0,0,0},{0,0,0,0,0,0,0,0,1,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
raysdesired = matrix {{0,1,0,-1,0,0,-1,0},{0,0,1,-1,0,-1,0,0},{1,0,0,1,0,0,0,-1},{0,0,0,1,1,0,0,-1},{0,0,0,0,0,0,0,0}};
linealitydesired = matrix {{0},{0},{1},{1},{1}};
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
maxConesdesired = {{1,2,4,7},{2,4,6,7},{1,4,5,7},{3,4,5,6,7},{0,1,2,7},{0,2,6,7},{0,1,5,7},{0,3,5,6,7},{0,1,2,4},{0,2,3,4,6},{0,1,3,4,5}};
desired = fan (raysdesired ,linealitydesired ,maxConesdesired);
computed = normalFan P;
assert(computed == desired)
assert(isPolytopal computed)
assert(not isSmooth computed)
assert(isPure computed)
assert(not isSimplicial computed)
assert(isComplete computed)
///

-- Test ambDim: 5, dim: 5, nvert: 7
-- Checking normal_fan + several booleans
TEST ///
verticesP = matrix {{0,1,0,1,1,1,3},{0,0,1,1,0,0,4},{1,1,1,1,0,0,0},{0,0,0,0,1,0,0},{0,0,0,0,0,1,0}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
raysdesired = matrix {{1,0,-1,0,0,0,0},{0,1,0,-1,0,0,0},{3,4,-2,-3,0,0,-1},{2,4,-2,-4,0,1,-1},{2,4,-2,-4,1,0,-1}};
linealitydesired = map(QQ^5, QQ^0, 0);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
maxConesdesired = {{0,1,4,5,6},{1,2,4,5,6},{0,3,4,5,6},{2,3,4,5,6},{0,1,2,3,4,6},{0,1,2,3,5,6},{0,1,2,3,4,5}};
desired = fan (raysdesired ,linealitydesired ,maxConesdesired);
computed = normalFan P;
assert(computed == desired)
assert(isPolytopal computed)
assert(not isSmooth computed)
assert(isPure computed)
assert(not isSimplicial computed)
assert(isComplete computed)
///

-- Test ambDim: 2, dim: 2, nvert: 3
-- Checking normal_fan + several booleans
TEST ///
verticesP = matrix {{0,1,0},{0,0,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
raysdesired = matrix {{-1,1,0},{-1,0,1}};
linealitydesired = map(QQ^2, QQ^0, 0);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
maxConesdesired = {{1,2},{0,2},{0,1}};
desired = fan (raysdesired ,linealitydesired ,maxConesdesired);
computed = normalFan P;
assert(computed == desired)
assert(isPolytopal computed)
assert(isSmooth computed)
assert(isPure computed)
assert(isSimplicial computed)
assert(isComplete computed)
///


-- Test ambDim: 7, dim: 7, nvert: 21
-- Checking normal_fan + several booleans
TEST ///
verticesP = matrix {{0,-2/3,1/3,-1/3,1/3,-1/3,2/3},{0,-2/3,1/3,-1/3,1/3,-1/3,2/3},{0,1/3,-2/3,-1/3,1/3,2/3,-1/3},{0,1/3,-2/3,-1/3,1/3,2/3,-1/3},{0,1/3,1/3,2/3,1/3,2/3,2/3},{0,1/3,1/3,2/3,-2/3,-1/3,-1/3},{0,1/3,1/3,2/3,-2/3,-1/3,-1/3}};
raysP = matrix {{0,1,-1,1,0,0,1,1,0,1,1,1,0,0},{0,1,1/2,-2,0,0,1,1,1,0,1,1,0,0},{0,-2,1/2,1,0,1,1,1,0,0,1,1,0,0},{0,1,1/2,1,1,0,-2,1,0,0,1,1,0,0},{0,1,1/2,1,0,0,1,1,0,0,1,-2,1,0},{0,1,1/2,1,0,0,1,-2,0,0,1,1,0,1},{1,1,1/2,1,0,0,1,1,0,0,-2,1,0,0}};
linealityP = map(QQ^7, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
raysdesired = matrix {{0,1,1,0,1,0,0,1,1,1,1,1,0,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,0},{1,0,0,1,0,1,0,1,1,1,1,1,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,1,0,0,1},{0,0,0,0,0,0,0,1,0,0,0,0,1,1,1,0,0,1,1,1,1,0,1,1,0,0,1,1,1,0,0,1,1,0,0},{0,0,0,0,0,0,0,0,1,0,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,0,0,0,1,1,0,0,1,1},{0,0,1,1,1,1,1,0,0,1,0,0,1,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0},{1,1,0,0,1,1,1,0,0,0,0,1,0,0,0,0,0,0,0,1,0,1,1,1,1,0,0,1,1,1,1,0,0,0,0},{1,1,1,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,0,0,0,0,1,1,1,1}};
linealitydesired = map(QQ^7, QQ^0, 0);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
maxConesdesired = {{2,3,4,5,15,16,17,18,23,24,25,26,27,28,29,30,31,32,33,34},{2,3,4,5,7,8,9,10,11,15,16,17,18,27,28,29,30,31,32,33,34},{12,13,14,15,16,17,18,19,20,23,24,25,26,27,28,29,30,31,32,33,34},{7,8,13,14,15,16,17,18,27,28,29,30,31,32,33,34},{0,1,2,3,4,5,6,21,22,23,24,25,26,27,28,29,30,31,32,33,34},{0,1,2,3,4,5,10,11,27,28,29,30,31,32,33,34},{19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34}};
desired = fan (raysdesired ,linealitydesired ,maxConesdesired);
computed = normalFan P;
assert(computed == desired)
assert(isPolytopal computed)
assert(not isSmooth computed)
assert(isPure computed)
assert(not isSimplicial computed)
assert(not isComplete computed)
///

-- Test ambDim: 4, dim: 4, nvert: 14
-- Checking normal_fan + several booleans
TEST ///
verticesP = matrix {{9,7,8,3,3,7,2,5,2,5,4,5,9,5},{0,7,5,6,4,0,3,3,5,0,6,2,5,1},{9,8,1,3,2,3,2,9,8,0,2,7,9,7},{0,7,2,3,6,5,3,1,2,8,6,9,6,0}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
raysdesired = matrix {{-1,-1,-1,1,1,1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,-1,-1,-1,-1,0,1,1,-1,0,-1,-1,-1,-1,1,-1,-1,-1},{5,-23/5,17/3,20/3,252/97,18/13,-47/6,-50/33,-110/171,18/17,-16/31,-17/47,-1/2,-1/2,-1/2,13/89,-1/2,-1/2,13/44,189/190,162/185,-23/18,-41/42,-31/14,-1,-23/32,-85/28,-50/7,-109/142,-10,1,6/37,-5/37,-7/36,-1,-63/164,19/180,-123/56,25/39,24/13,30/37,3/16,-24/13},{9/2,-3/10,-4/3,4/3,-68/97,-17/13,29/6,-2/11,-46/171,-1,17/31,4/47,1/2,1/28,11/6,-10/89,-15/76,3/8,-1/44,-51/190,-57/185,17/6,157/42,6/7,0,37/32,-6/7,-6/7,113/142,2,1,-131/37,-119/37,1/4,-8/5,81/164,11/36,-5/7,-1/13,-92/13,-17/74,13/64,-67/104},{5,49/5,-2,2,-28/97,-2/13,-109/6,-13/33,-127/171,-2/17,70/31,-10/47,-1/6,-9/28,-1/6,-34/89,-51/76,-13/8,-19/44,-21/190,-18/185,11/18,13/42,11/2,2,109/32,31/14,27/7,-143/142,1,1,-32/37,-35/37,-49/36,2/5,-121/164,-13/36,47/28,-29/39,-20/13,-25/37,-5/32,20/13}};
linealitydesired = map(QQ^4, QQ^0, 0);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
maxConesdesired = {{0,1,2,3,4,5,39,40,41,42},{6,7,8,26,27,28,29,32,33,34,35,37},{0,1,21,22,23,24,25,26,27,28,29,30,35,36,37,41,42},{7,10,11,12,13,21,23,25,27,29},{11,12,13,14,15,16,17,18},{0,2,3,30,36,38,40,41},{10,11,12,14,15,18,19,20,21,22,25,30},{1,5,9,24,26,31,34,37,39,42},{7,8,9,10,11,13,15,16,20,23,24,26,27,31,32,34},{2,3,4,6,14,17,18,19,22,28,30,33,35,36,38},{6,7,8,12,13,14,16,17,21,22,28,29},{2,4,5,6,8,9,15,16,17,18,19,20,31,32,33,38,39,40},{31,32,33,34,35,36,37,38,39,40,41,42},{0,1,3,4,5,9,10,19,20,23,24,25,30}};
desired = fan (raysdesired ,linealitydesired ,maxConesdesired);
computed = normalFan P;
assert(computed == desired)
assert(isPolytopal computed)
assert(not isSmooth computed)
assert(isPure computed)
assert(not isSimplicial computed)
assert(isComplete computed)
///

-- Test ambDim: 4, dim: 4, nvert: 42
-- Checking normal_fan + several booleans
TEST ///
verticesP = matrix {{33,34,19,9,9,1,1,1,1,1,4,10,4,9,1,1,1,1,1,1,1,1,1,9,4,4,4,10,34,19,10,35,20,20,10,4,4,10,4,10,4,4},{4,1,1,31,16,4,4,34,10,19,1,1,1,4,4,4,4,10,10,20,35,20,10,4,1,1,1,1,4,4,1,1,1,1,1,9,19,31,34,16,19,9},{1,4,31,1,25,33,9,1,1,25,10,4,34,1,34,19,9,16,1,1,1,25,31,1,35,20,10,4,1,1,19,4,4,31,34,16,1,1,1,25,25,31},{31,30,15,25,15,15,30,25,33,15,30,33,15,34,15,25,31,25,34,31,25,15,15,35,15,25,31,34,31,34,25,30,33,15,15,25,31,25,25,15,15,15}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
raysdesired = matrix {{0,-1,1,1,-1,1,-1,0,0,1,0,-1,0,0},{0,-3,1,0,-3,4/3,-3,0,0,8/3,1,-2,1,1},{1,-6,0,0,-5,1,-6,1,0,5,0,-10/3,1,5/2},{0,-10,0,0,-15/2,0,-9,12/5,1,8,0,-5,0,9/2}};
linealitydesired = map(QQ^4, QQ^0, 0);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
maxConesdesired = {{0,9,12,13},{9,10,12,13},{8,9,10,13},{0,7,9,13},{7,8,9,13},{2,3,8,9},{2,3,5,9},{0,3,7,9},{0,3,5,9},{3,7,8,9},{2,5,9,10},{5,9,10,12},{2,8,9,10},{0,5,9,12},{2,3,6,8},{1,2,3,6},{1,2,3,5},{1,3,4,6},{0,1,3,5},{0,1,3,4},{0,3,4,7},{3,4,7,8},{3,4,6,8},{0,1,5,12},{2,6,8,10},{1,2,6,10},{1,2,5,10},{1,5,10,12},{0,11,12,13},{0,1,11,12},{1,6,10,11},{10,11,12,13},{1,10,11,12},{8,10,11,13},{6,8,10,11},{1,4,6,11},{0,1,4,11},{0,7,11,13},{0,4,7,11},{7,8,11,13},{4,7,8,11},{4,6,8,11}};
desired = fan (raysdesired ,linealitydesired ,maxConesdesired);
computed = normalFan P;
assert(computed == desired)
assert(isPolytopal computed)
assert(not isSmooth computed)
assert(isPure computed)
assert(isSimplicial computed)
assert(isComplete computed)
///
