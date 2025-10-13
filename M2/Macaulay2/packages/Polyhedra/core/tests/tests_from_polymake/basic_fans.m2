TEST ///
-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF0 = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF0 = map(QQ^3, QQ^0, 0);
raysF0 = promote(raysF0, QQ);
linealityF0 = promote(linealityF0, QQ);
maxConesF0 = {{0,1},{0,2}};
F0 = fan (raysF0 ,linealityF0 ,maxConesF0);
assert(dim F0 ==2)
assert(ambDim F0 ==3)
assert(#(maxCones F0) ==2)
assert(isSmooth F0)
assert(isPure F0)
assert(isSimplicial F0)
assert(not isComplete F0)
assert(fVector F0 == {1, 3, 2})
assert(not isPolytopal F0)

-- Test ambDim: 3, dim: 1, nrays: 2, n_max_cones: 2
-- Checking misc tests for fan
raysF1 = matrix {{1,0},{0,1},{0,0}};
linealityF1 = map(QQ^3, QQ^0, 0);
raysF1 = promote(raysF1, QQ);
linealityF1 = promote(linealityF1, QQ);
maxConesF1 = {{0},{1}};
F1 = fan (raysF1 ,linealityF1 ,maxConesF1);
assert(dim F1 ==1)
assert(ambDim F1 ==3)
assert(#(maxCones F1) ==2)
assert(isSmooth F1)
assert(isPure F1)
assert(isSimplicial F1)
assert(not isComplete F1)
assert(fVector F1 == {1, 2})
assert(not isPolytopal F1)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
raysF2 = matrix {{1,0,-1,0},{0,1,0,-1}};
linealityF2 = map(QQ^2, QQ^0, 0);
raysF2 = promote(raysF2, QQ);
linealityF2 = promote(linealityF2, QQ);
maxConesF2 = {{0,1},{1,2},{2,3},{0,3}};
F2 = fan (raysF2 ,linealityF2 ,maxConesF2);
assert(dim F2 ==2)
assert(ambDim F2 ==2)
assert(#(maxCones F2) ==4)
assert(isSmooth F2)
assert(isPure F2)
assert(isSimplicial F2)
assert(isComplete F2)
assert(fVector F2 == {1, 4, 4})
assert(isPolytopal F2)

-- Test ambDim: 3, dim: 3, nrays: 5, n_max_cones: 3
-- Checking misc tests for fan
raysF3 = matrix {{1,0,0,-1,0},{0,1,0,0,-1},{0,0,1,0,0}};
linealityF3 = map(QQ^3, QQ^0, 0);
raysF3 = promote(raysF3, QQ);
linealityF3 = promote(linealityF3, QQ);
maxConesF3 = {{0,1,2},{1,3},{4}};
F3 = fan (raysF3 ,linealityF3 ,maxConesF3);
assert(dim F3 ==3)
assert(ambDim F3 ==3)
assert(#(maxCones F3) ==3)
assert(isSmooth F3)
assert(not isPure F3)
assert(isSimplicial F3)
assert(not isComplete F3)
assert(fVector F3 == {1, 5, 4, 1})
assert(not isPolytopal F3)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
raysF4 = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF4 = map(QQ^2, QQ^0, 0);
raysF4 = promote(raysF4, QQ);
linealityF4 = promote(linealityF4, QQ);
maxConesF4 = {{0,1},{2},{3}};
F4 = fan (raysF4 ,linealityF4 ,maxConesF4);
assert(dim F4 ==2)
assert(ambDim F4 ==2)
assert(#(maxCones F4) ==3)
assert(isSmooth F4)
assert(not isPure F4)
assert(isSimplicial F4)
assert(not isComplete F4)
assert(fVector F4 == {1, 4, 1})
assert(not isPolytopal F4)

-- Test ambDim: 3, dim: 3, nrays: 3, n_max_cones: 1
-- Checking misc tests for fan
raysF5 = matrix {{1,0,0},{0,1,0},{0,0,1}};
linealityF5 = map(QQ^3, QQ^0, 0);
raysF5 = promote(raysF5, QQ);
linealityF5 = promote(linealityF5, QQ);
maxConesF5 = {{0,1,2}};
F5 = fan (raysF5 ,linealityF5 ,maxConesF5);
assert(dim F5 ==3)
assert(ambDim F5 ==3)
assert(#(maxCones F5) ==1)
assert(isSmooth F5)
assert(isPure F5)
assert(isSimplicial F5)
assert(not isComplete F5)
assert(fVector F5 == {1, 3, 3, 1})
assert(not isPolytopal F5)

-- Test ambDim: 3, dim: 3, nrays: 7, n_max_cones: 4
-- Checking misc tests for fan
raysF6 = matrix {{-1,0,-1,0,1,1,1},{-1,1,-2,0,1,0,-1},{-1,0,-1,1,-1,0,1}};
linealityF6 = map(QQ^3, QQ^0, 0);
raysF6 = promote(raysF6, QQ);
linealityF6 = promote(linealityF6, QQ);
maxConesF6 = {{0},{1,3,4,5},{2,6},{3,5,6}};
F6 = fan (raysF6 ,linealityF6 ,maxConesF6);
assert(dim F6 ==3)
assert(ambDim F6 ==3)
assert(#(maxCones F6) ==4)
assert(not isSmooth F6)
assert(not isPure F6)
assert(not isSimplicial F6)
assert(not isComplete F6)
assert(fVector F6 == {1, 7, 7, 2})
assert(not isPolytopal F6)

-- Test ambDim: 4, dim: 4, nrays: 12, n_max_cones: 4
-- Checking misc tests for fan
raysF7 = matrix {{1,1,1,1,1,1,1,1,1,1,1,1},{0,1,1,0,0,1,1,0,-1,-1,1/2,-2},{0,0,1,1,0,0,1,1,0,-1,1/2,0},{0,0,0,0,1,1,1,1,0,0,-1/4,0}};
linealityF7 = map(QQ^4, QQ^0, 0);
raysF7 = promote(raysF7, QQ);
linealityF7 = promote(linealityF7, QQ);
maxConesF7 = {{0,1,2,3,4,5,6,7},{0,3,8,9},{0,1,2,3,10},{8,11}};
F7 = fan (raysF7 ,linealityF7 ,maxConesF7);
assert(dim F7 ==4)
assert(ambDim F7 ==4)
assert(#(maxCones F7) ==4)
assert(not isSmooth F7)
assert(not isPure F7)
assert(not isSimplicial F7)
assert(not isComplete F7)
assert(fVector F7 == {1, 12, 20, 11, 2})
assert(not isPolytopal F7)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF8 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF8 = map(QQ^3, QQ^0, 0);
raysF8 = promote(raysF8, QQ);
linealityF8 = promote(linealityF8, QQ);
maxConesF8 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F8 = fan (raysF8 ,linealityF8 ,maxConesF8);
assert(dim F8 ==3)
assert(ambDim F8 ==3)
assert(#(maxCones F8) ==8)
assert(isSmooth F8)
assert(isPure F8)
assert(isSimplicial F8)
assert(isComplete F8)
assert(fVector F8 == {1, 6, 12, 8})
assert(isPolytopal F8)

-- Test ambDim: 3, dim: 3, nrays: 4, n_max_cones: 2
-- Checking misc tests for fan
raysF9 = matrix {{1,0,0,0},{0,1,0,0},{0,0,1,-1}};
linealityF9 = map(QQ^3, QQ^0, 0);
raysF9 = promote(raysF9, QQ);
linealityF9 = promote(linealityF9, QQ);
maxConesF9 = {{0,1,2},{0,1,3}};
F9 = fan (raysF9 ,linealityF9 ,maxConesF9);
assert(dim F9 ==3)
assert(ambDim F9 ==3)
assert(#(maxCones F9) ==2)
assert(isSmooth F9)
assert(isPure F9)
assert(isSimplicial F9)
assert(not isComplete F9)
assert(fVector F9 == {1, 4, 5, 2})
assert(not isPolytopal F9)

///

TEST ///
-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF10 = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF10 = map(QQ^3, QQ^0, 0);
raysF10 = promote(raysF10, QQ);
linealityF10 = promote(linealityF10, QQ);
maxConesF10 = {{0,1},{0,2}};
F10 = fan (raysF10 ,linealityF10 ,maxConesF10);
assert(dim F10 ==2)
assert(ambDim F10 ==3)
assert(#(maxCones F10) ==2)
assert(isSmooth F10)
assert(isPure F10)
assert(isSimplicial F10)
assert(not isComplete F10)
assert(fVector F10 == {1, 3, 2})
assert(not isPolytopal F10)

-- Test ambDim: 4, dim: 4, nrays: 12, n_max_cones: 4
-- Checking misc tests for fan
raysF11 = matrix {{1,1,1,1,1,1,1,1,1,1,1,1},{0,1,1,0,0,1,1,0,-1,-1,1/2,-2},{0,0,1,1,0,0,1,1,0,-1,1/2,0},{0,0,0,0,1,1,1,1,0,0,-1/4,0}};
linealityF11 = map(QQ^4, QQ^0, 0);
raysF11 = promote(raysF11, QQ);
linealityF11 = promote(linealityF11, QQ);
maxConesF11 = {{0,1,2,3,4,5,6,7},{0,3,8,9},{0,1,2,3,10},{8,11}};
F11 = fan (raysF11 ,linealityF11 ,maxConesF11);
assert(dim F11 ==4)
assert(ambDim F11 ==4)
assert(#(maxCones F11) ==4)
assert(not isSmooth F11)
assert(not isPure F11)
assert(not isSimplicial F11)
assert(not isComplete F11)
assert(fVector F11 == {1, 12, 20, 11, 2})
assert(not isPolytopal F11)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
raysF12 = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF12 = map(QQ^2, QQ^0, 0);
raysF12 = promote(raysF12, QQ);
linealityF12 = promote(linealityF12, QQ);
maxConesF12 = {{0,1},{2},{3}};
F12 = fan (raysF12 ,linealityF12 ,maxConesF12);
assert(dim F12 ==2)
assert(ambDim F12 ==2)
assert(#(maxCones F12) ==3)
assert(isSmooth F12)
assert(not isPure F12)
assert(isSimplicial F12)
assert(not isComplete F12)
assert(fVector F12 == {1, 4, 1})
assert(not isPolytopal F12)

-- Test ambDim: 3, dim: 3, nrays: 7, n_max_cones: 4
-- Checking misc tests for fan
raysF13 = matrix {{-1,0,-1,0,1,1,1},{-1,1,-2,0,1,0,-1},{-1,0,-1,1,-1,0,1}};
linealityF13 = map(QQ^3, QQ^0, 0);
raysF13 = promote(raysF13, QQ);
linealityF13 = promote(linealityF13, QQ);
maxConesF13 = {{0},{1,3,4,5},{2,6},{3,5,6}};
F13 = fan (raysF13 ,linealityF13 ,maxConesF13);
assert(dim F13 ==3)
assert(ambDim F13 ==3)
assert(#(maxCones F13) ==4)
assert(not isSmooth F13)
assert(not isPure F13)
assert(not isSimplicial F13)
assert(not isComplete F13)
assert(fVector F13 == {1, 7, 7, 2})
assert(not isPolytopal F13)

-- Test ambDim: 3, dim: 3, nrays: 4, n_max_cones: 2
-- Checking misc tests for fan
raysF14 = matrix {{1,0,0,0},{0,1,0,0},{0,0,1,-1}};
linealityF14 = map(QQ^3, QQ^0, 0);
raysF14 = promote(raysF14, QQ);
linealityF14 = promote(linealityF14, QQ);
maxConesF14 = {{0,1,2},{0,1,3}};
F14 = fan (raysF14 ,linealityF14 ,maxConesF14);
assert(dim F14 ==3)
assert(ambDim F14 ==3)
assert(#(maxCones F14) ==2)
assert(isSmooth F14)
assert(isPure F14)
assert(isSimplicial F14)
assert(not isComplete F14)
assert(fVector F14 == {1, 4, 5, 2})
assert(not isPolytopal F14)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF15 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF15 = map(QQ^3, QQ^0, 0);
raysF15 = promote(raysF15, QQ);
linealityF15 = promote(linealityF15, QQ);
maxConesF15 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F15 = fan (raysF15 ,linealityF15 ,maxConesF15);
assert(dim F15 ==3)
assert(ambDim F15 ==3)
assert(#(maxCones F15) ==8)
assert(isSmooth F15)
assert(isPure F15)
assert(isSimplicial F15)
assert(isComplete F15)
assert(fVector F15 == {1, 6, 12, 8})
assert(isPolytopal F15)

-- Test ambDim: 2, dim: 1, nrays: 1, n_max_cones: 1
-- Checking misc tests for fan
raysF16 = matrix {{1},{0}};
linealityF16 = map(QQ^2, QQ^0, 0);
raysF16 = promote(raysF16, QQ);
linealityF16 = promote(linealityF16, QQ);
maxConesF16 = {{0}};
F16 = fan (raysF16 ,linealityF16 ,maxConesF16);
assert(dim F16 ==1)
assert(ambDim F16 ==2)
assert(#(maxCones F16) ==1)
assert(isSmooth F16)
assert(isPure F16)
assert(isSimplicial F16)
assert(not isComplete F16)
assert(fVector F16 == {1, 1})
assert(not isPolytopal F16)

-- Test ambDim: 2, dim: 2, nrays: 6, n_max_cones: 6
-- Checking misc tests for fan
raysF17 = matrix {{1,0,-1,-1,0,1},{1,1,0,-1,-1,0}};
linealityF17 = map(QQ^2, QQ^0, 0);
raysF17 = promote(raysF17, QQ);
linealityF17 = promote(linealityF17, QQ);
maxConesF17 = {{0,1},{1,2},{2,3},{3,4},{4,5},{0,5}};
F17 = fan (raysF17 ,linealityF17 ,maxConesF17);
assert(dim F17 ==2)
assert(ambDim F17 ==2)
assert(#(maxCones F17) ==6)
assert(isSmooth F17)
assert(isPure F17)
assert(isSimplicial F17)
assert(isComplete F17)
assert(fVector F17 == {1, 6, 6})
assert(isPolytopal F17)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
raysF18 = matrix {{1,0,-1,0},{0,1,0,-1}};
linealityF18 = map(QQ^2, QQ^0, 0);
raysF18 = promote(raysF18, QQ);
linealityF18 = promote(linealityF18, QQ);
maxConesF18 = {{0,1},{1,2},{2,3},{0,3}};
F18 = fan (raysF18 ,linealityF18 ,maxConesF18);
assert(dim F18 ==2)
assert(ambDim F18 ==2)
assert(#(maxCones F18) ==4)
assert(isSmooth F18)
assert(isPure F18)
assert(isSimplicial F18)
assert(isComplete F18)
assert(fVector F18 == {1, 4, 4})
assert(isPolytopal F18)

-- Test ambDim: 3, dim: 3, nrays: 5, n_max_cones: 5
-- Checking misc tests for fan
raysF19 = matrix {{1,0,-1,0,0},{0,1,0,-1,0},{1,2,1,2,-1}};
linealityF19 = map(QQ^3, QQ^0, 0);
raysF19 = promote(raysF19, QQ);
linealityF19 = promote(linealityF19, QQ);
maxConesF19 = {{0,1,2,3},{0,1,4},{1,2,4},{2,3,4},{0,3,4}};
F19 = fan (raysF19 ,linealityF19 ,maxConesF19);
assert(dim F19 ==3)
assert(ambDim F19 ==3)
assert(#(maxCones F19) ==5)
assert(not isSmooth F19)
assert(isPure F19)
assert(not isSimplicial F19)
assert(isComplete F19)
assert(fVector F19 == {1, 5, 8, 5})
assert(isPolytopal F19)

///

TEST ///
-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF20 = matrix {{-1,1,0,-1,1,0},{-1,0,1,-1,0,1},{-1,-1,-1,1,1,1}};
linealityF20 = map(QQ^3, QQ^0, 0);
raysF20 = promote(raysF20, QQ);
linealityF20 = promote(linealityF20, QQ);
maxConesF20 = {{0,1,2},{3,4,5},{0,1,4},{0,3,4},{1,2,5},{1,4,5},{0,2,3},{2,3,5}};
F20 = fan (raysF20 ,linealityF20 ,maxConesF20);
assert(dim F20 ==3)
assert(ambDim F20 ==3)
assert(#(maxCones F20) ==8)
assert(not isSmooth F20)
assert(isPure F20)
assert(isSimplicial F20)
assert(isComplete F20)
assert(fVector F20 == {1, 6, 12, 8})
assert(not isPolytopal F20)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
raysF21 = matrix {{1,0,-1,0},{1,-1,1,1}};
linealityF21 = map(QQ^2, QQ^0, 0);
raysF21 = promote(raysF21, QQ);
linealityF21 = promote(linealityF21, QQ);
maxConesF21 = {{0,1},{1,2},{2,3},{0,3}};
F21 = fan (raysF21 ,linealityF21 ,maxConesF21);
assert(dim F21 ==2)
assert(ambDim F21 ==2)
assert(#(maxCones F21) ==4)
assert(isSmooth F21)
assert(isPure F21)
assert(isSimplicial F21)
assert(isComplete F21)
assert(fVector F21 == {1, 4, 4})
assert(isPolytopal F21)

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF22 = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF22 = map(QQ^3, QQ^0, 0);
raysF22 = promote(raysF22, QQ);
linealityF22 = promote(linealityF22, QQ);
maxConesF22 = {{0,1},{0,2}};
F22 = fan (raysF22 ,linealityF22 ,maxConesF22);
assert(dim F22 ==2)
assert(ambDim F22 ==3)
assert(#(maxCones F22) ==2)
assert(isSmooth F22)
assert(isPure F22)
assert(isSimplicial F22)
assert(not isComplete F22)
assert(fVector F22 == {1, 3, 2})
assert(not isPolytopal F22)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
raysF23 = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF23 = map(QQ^2, QQ^0, 0);
raysF23 = promote(raysF23, QQ);
linealityF23 = promote(linealityF23, QQ);
maxConesF23 = {{0,1},{2},{3}};
F23 = fan (raysF23 ,linealityF23 ,maxConesF23);
assert(dim F23 ==2)
assert(ambDim F23 ==2)
assert(#(maxCones F23) ==3)
assert(isSmooth F23)
assert(not isPure F23)
assert(isSimplicial F23)
assert(not isComplete F23)
assert(fVector F23 == {1, 4, 1})
assert(not isPolytopal F23)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF24 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF24 = map(QQ^3, QQ^0, 0);
raysF24 = promote(raysF24, QQ);
linealityF24 = promote(linealityF24, QQ);
maxConesF24 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F24 = fan (raysF24 ,linealityF24 ,maxConesF24);
assert(dim F24 ==3)
assert(ambDim F24 ==3)
assert(#(maxCones F24) ==8)
assert(isSmooth F24)
assert(isPure F24)
assert(isSimplicial F24)
assert(isComplete F24)
assert(fVector F24 == {1, 6, 12, 8})
assert(isPolytopal F24)

-- Test ambDim: 2, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF25 = matrix {{1,1,0},{1,0,-1}};
linealityF25 = map(QQ^2, QQ^0, 0);
raysF25 = promote(raysF25, QQ);
linealityF25 = promote(linealityF25, QQ);
maxConesF25 = {{0,1},{1,2}};
F25 = fan (raysF25 ,linealityF25 ,maxConesF25);
assert(dim F25 ==2)
assert(ambDim F25 ==2)
assert(#(maxCones F25) ==2)
assert(isSmooth F25)
assert(isPure F25)
assert(isSimplicial F25)
assert(not isComplete F25)
assert(fVector F25 == {1, 3, 2})
assert(not isPolytopal F25)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 2
-- Checking misc tests for fan
raysF26 = matrix {{1,0,-1,0},{0,1,-1,-1}};
linealityF26 = map(QQ^2, QQ^0, 0);
raysF26 = promote(raysF26, QQ);
linealityF26 = promote(linealityF26, QQ);
maxConesF26 = {{0,1},{2,3}};
F26 = fan (raysF26 ,linealityF26 ,maxConesF26);
assert(dim F26 ==2)
assert(ambDim F26 ==2)
assert(#(maxCones F26) ==2)
assert(isSmooth F26)
assert(isPure F26)
assert(isSimplicial F26)
assert(not isComplete F26)
assert(fVector F26 == {1, 4, 2})
assert(not isPolytopal F26)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
raysF27 = matrix {{1,1,0,0},{0,1,1,-1}};
linealityF27 = map(QQ^2, QQ^0, 0);
raysF27 = promote(raysF27, QQ);
linealityF27 = promote(linealityF27, QQ);
maxConesF27 = {{0,1},{1,2},{0,3}};
F27 = fan (raysF27 ,linealityF27 ,maxConesF27);
assert(dim F27 ==2)
assert(ambDim F27 ==2)
assert(#(maxCones F27) ==3)
assert(isSmooth F27)
assert(isPure F27)
assert(isSimplicial F27)
assert(not isComplete F27)
assert(fVector F27 == {1, 4, 3})
assert(not isPolytopal F27)

-- Test ambDim: 2, dim: 2, nrays: 2, n_max_cones: 1
-- Checking misc tests for fan
raysF28 = matrix {{1,0},{0,1}};
linealityF28 = map(QQ^2, QQ^0, 0);
raysF28 = promote(raysF28, QQ);
linealityF28 = promote(linealityF28, QQ);
maxConesF28 = {{0,1}};
F28 = fan (raysF28 ,linealityF28 ,maxConesF28);
assert(dim F28 ==2)
assert(ambDim F28 ==2)
assert(#(maxCones F28) ==1)
assert(isSmooth F28)
assert(isPure F28)
assert(isSimplicial F28)
assert(not isComplete F28)
assert(fVector F28 == {1, 2, 1})
assert(not isPolytopal F28)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF29 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF29 = map(QQ^3, QQ^0, 0);
raysF29 = promote(raysF29, QQ);
linealityF29 = promote(linealityF29, QQ);
maxConesF29 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F29 = fan (raysF29 ,linealityF29 ,maxConesF29);
assert(dim F29 ==3)
assert(ambDim F29 ==3)
assert(#(maxCones F29) ==8)
assert(isSmooth F29)
assert(isPure F29)
assert(isSimplicial F29)
assert(isComplete F29)
assert(fVector F29 == {1, 6, 12, 8})
assert(isPolytopal F29)

///

TEST ///
-- Test ambDim: 4, dim: 4, nrays: 6, n_max_cones: 4
-- Checking misc tests for fan
raysF30 = matrix {{1,1,0,0,0,0},{0,1,1,0,0,0},{0,0,0,1,1,0},{0,0,0,0,1,1}};
linealityF30 = map(QQ^4, QQ^0, 0);
raysF30 = promote(raysF30, QQ);
linealityF30 = promote(linealityF30, QQ);
maxConesF30 = {{0,3},{0,4,5},{1,2,3},{1,2,4,5}};
F30 = fan (raysF30 ,linealityF30 ,maxConesF30);
assert(dim F30 ==4)
assert(ambDim F30 ==4)
assert(#(maxCones F30) ==4)
assert(isSmooth F30)
assert(not isPure F30)
assert(isSimplicial F30)
assert(not isComplete F30)
assert(fVector F30 == {1, 6, 11, 6, 1})
assert(not isPolytopal F30)

-- Test ambDim: 2, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF31 = matrix {{1,1,0},{0,1,1}};
linealityF31 = map(QQ^2, QQ^0, 0);
raysF31 = promote(raysF31, QQ);
linealityF31 = promote(linealityF31, QQ);
maxConesF31 = {{0},{1,2}};
F31 = fan (raysF31 ,linealityF31 ,maxConesF31);
assert(dim F31 ==2)
assert(ambDim F31 ==2)
assert(#(maxCones F31) ==2)
assert(isSmooth F31)
assert(not isPure F31)
assert(isSimplicial F31)
assert(not isComplete F31)
assert(fVector F31 == {1, 3, 1})
assert(not isPolytopal F31)

-- Test ambDim: 1, dim: 1, nrays: 2, n_max_cones: 2
-- Checking misc tests for fan
raysF32 = matrix {{-1,1}};
linealityF32 = map(QQ^1, QQ^0, 0);
raysF32 = promote(raysF32, QQ);
linealityF32 = promote(linealityF32, QQ);
maxConesF32 = {{1},{0}};
F32 = fan (raysF32 ,linealityF32 ,maxConesF32);
assert(dim F32 ==1)
assert(ambDim F32 ==1)
assert(#(maxCones F32) ==2)
assert(isSmooth F32)
assert(isPure F32)
assert(isSimplicial F32)
assert(isComplete F32)
assert(fVector F32 == {1, 2})
assert(isPolytopal F32)

-- Test ambDim: 4, dim: 4, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF33 = matrix {{-1,1,0,0,0,0},{0,0,-1,1,0,0},{0,0,-1,0,1,0},{0,0,-1,0,0,1}};
linealityF33 = map(QQ^4, QQ^0, 0);
raysF33 = promote(raysF33, QQ);
linealityF33 = promote(linealityF33, QQ);
maxConesF33 = {{1,3,4,5},{1,2,4,5},{1,2,3,5},{1,2,3,4},{0,3,4,5},{0,2,4,5},{0,2,3,5},{0,2,3,4}};
F33 = fan (raysF33 ,linealityF33 ,maxConesF33);
assert(dim F33 ==4)
assert(ambDim F33 ==4)
assert(#(maxCones F33) ==8)
assert(isSmooth F33)
assert(isPure F33)
assert(isSimplicial F33)
assert(isComplete F33)
assert(fVector F33 == {1, 6, 14, 16, 8})
assert(isPolytopal F33)

-- Test ambDim: 3, dim: 3, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
raysF34 = matrix {{-1,1,0,0},{-1,0,1,0},{-1,0,0,1}};
linealityF34 = map(QQ^3, QQ^0, 0);
raysF34 = promote(raysF34, QQ);
linealityF34 = promote(linealityF34, QQ);
maxConesF34 = {{1,2,3},{0,2,3},{0,1,3},{0,1,2}};
F34 = fan (raysF34 ,linealityF34 ,maxConesF34);
assert(dim F34 ==3)
assert(ambDim F34 ==3)
assert(#(maxCones F34) ==4)
assert(isSmooth F34)
assert(isPure F34)
assert(isSimplicial F34)
assert(isComplete F34)
assert(fVector F34 == {1, 4, 6, 4})
assert(isPolytopal F34)

-- Test ambDim: 3, dim: 3, nrays: 4, n_max_cones: 2
-- Checking misc tests for fan
raysF35 = matrix {{1,0,0,0},{0,1,0,0},{0,0,1,-1}};
linealityF35 = map(QQ^3, QQ^0, 0);
raysF35 = promote(raysF35, QQ);
linealityF35 = promote(linealityF35, QQ);
maxConesF35 = {{0,1,2},{0,1,3}};
F35 = fan (raysF35 ,linealityF35 ,maxConesF35);
assert(dim F35 ==3)
assert(ambDim F35 ==3)
assert(#(maxCones F35) ==2)
assert(isSmooth F35)
assert(isPure F35)
assert(isSimplicial F35)
assert(not isComplete F35)
assert(fVector F35 == {1, 4, 5, 2})
assert(not isPolytopal F35)

-- Test ambDim: 4, dim: 4, nrays: 12, n_max_cones: 4
-- Checking misc tests for fan
raysF36 = matrix {{1,1,1,1,1,1,1,1,1,1,1,1},{0,1,1,0,0,1,1,0,-1,-1,1/2,-2},{0,0,1,1,0,0,1,1,0,-1,1/2,0},{0,0,0,0,1,1,1,1,0,0,-1/4,0}};
linealityF36 = map(QQ^4, QQ^0, 0);
raysF36 = promote(raysF36, QQ);
linealityF36 = promote(linealityF36, QQ);
maxConesF36 = {{0,1,2,3,4,5,6,7},{0,3,8,9},{0,1,2,3,10},{8,11}};
F36 = fan (raysF36 ,linealityF36 ,maxConesF36);
assert(dim F36 ==4)
assert(ambDim F36 ==4)
assert(#(maxCones F36) ==4)
assert(not isSmooth F36)
assert(not isPure F36)
assert(not isSimplicial F36)
assert(not isComplete F36)
assert(fVector F36 == {1, 12, 20, 11, 2})
assert(not isPolytopal F36)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF37 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF37 = map(QQ^3, QQ^0, 0);
raysF37 = promote(raysF37, QQ);
linealityF37 = promote(linealityF37, QQ);
maxConesF37 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F37 = fan (raysF37 ,linealityF37 ,maxConesF37);
assert(dim F37 ==3)
assert(ambDim F37 ==3)
assert(#(maxCones F37) ==8)
assert(isSmooth F37)
assert(isPure F37)
assert(isSimplicial F37)
assert(isComplete F37)
assert(fVector F37 == {1, 6, 12, 8})
assert(isPolytopal F37)

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF38 = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF38 = map(QQ^3, QQ^0, 0);
raysF38 = promote(raysF38, QQ);
linealityF38 = promote(linealityF38, QQ);
maxConesF38 = {{0,1},{0,2}};
F38 = fan (raysF38 ,linealityF38 ,maxConesF38);
assert(dim F38 ==2)
assert(ambDim F38 ==3)
assert(#(maxCones F38) ==2)
assert(isSmooth F38)
assert(isPure F38)
assert(isSimplicial F38)
assert(not isComplete F38)
assert(fVector F38 == {1, 3, 2})
assert(not isPolytopal F38)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
raysF39 = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF39 = map(QQ^2, QQ^0, 0);
raysF39 = promote(raysF39, QQ);
linealityF39 = promote(linealityF39, QQ);
maxConesF39 = {{0,1},{2},{3}};
F39 = fan (raysF39 ,linealityF39 ,maxConesF39);
assert(dim F39 ==2)
assert(ambDim F39 ==2)
assert(#(maxCones F39) ==3)
assert(isSmooth F39)
assert(not isPure F39)
assert(isSimplicial F39)
assert(not isComplete F39)
assert(fVector F39 == {1, 4, 1})
assert(not isPolytopal F39)

///

TEST ///
-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF40 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF40 = map(QQ^3, QQ^0, 0);
raysF40 = promote(raysF40, QQ);
linealityF40 = promote(linealityF40, QQ);
maxConesF40 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F40 = fan (raysF40 ,linealityF40 ,maxConesF40);
assert(dim F40 ==3)
assert(ambDim F40 ==3)
assert(#(maxCones F40) ==8)
assert(isSmooth F40)
assert(isPure F40)
assert(isSimplicial F40)
assert(isComplete F40)
assert(fVector F40 == {1, 6, 12, 8})
assert(isPolytopal F40)

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF41 = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF41 = map(QQ^3, QQ^0, 0);
raysF41 = promote(raysF41, QQ);
linealityF41 = promote(linealityF41, QQ);
maxConesF41 = {{0,1},{0,2}};
F41 = fan (raysF41 ,linealityF41 ,maxConesF41);
assert(dim F41 ==2)
assert(ambDim F41 ==3)
assert(#(maxCones F41) ==2)
assert(isSmooth F41)
assert(isPure F41)
assert(isSimplicial F41)
assert(not isComplete F41)
assert(fVector F41 == {1, 3, 2})
assert(not isPolytopal F41)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
raysF42 = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF42 = map(QQ^2, QQ^0, 0);
raysF42 = promote(raysF42, QQ);
linealityF42 = promote(linealityF42, QQ);
maxConesF42 = {{0,1},{2},{3}};
F42 = fan (raysF42 ,linealityF42 ,maxConesF42);
assert(dim F42 ==2)
assert(ambDim F42 ==2)
assert(#(maxCones F42) ==3)
assert(isSmooth F42)
assert(not isPure F42)
assert(isSimplicial F42)
assert(not isComplete F42)
assert(fVector F42 == {1, 4, 1})
assert(not isPolytopal F42)

-- Test ambDim: 3, dim: 3, nrays: 7, n_max_cones: 4
-- Checking misc tests for fan
raysF43 = matrix {{-1,0,-1,0,1,1,1},{-1,1,-2,0,1,0,-1},{-1,0,-1,1,-1,0,1}};
linealityF43 = map(QQ^3, QQ^0, 0);
raysF43 = promote(raysF43, QQ);
linealityF43 = promote(linealityF43, QQ);
maxConesF43 = {{0},{1,3,4,5},{2,6},{3,5,6}};
F43 = fan (raysF43 ,linealityF43 ,maxConesF43);
assert(dim F43 ==3)
assert(ambDim F43 ==3)
assert(#(maxCones F43) ==4)
assert(not isSmooth F43)
assert(not isPure F43)
assert(not isSimplicial F43)
assert(not isComplete F43)
assert(fVector F43 == {1, 7, 7, 2})
assert(not isPolytopal F43)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF44 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF44 = map(QQ^3, QQ^0, 0);
raysF44 = promote(raysF44, QQ);
linealityF44 = promote(linealityF44, QQ);
maxConesF44 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F44 = fan (raysF44 ,linealityF44 ,maxConesF44);
assert(dim F44 ==3)
assert(ambDim F44 ==3)
assert(#(maxCones F44) ==8)
assert(isSmooth F44)
assert(isPure F44)
assert(isSimplicial F44)
assert(isComplete F44)
assert(fVector F44 == {1, 6, 12, 8})
assert(isPolytopal F44)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF45 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF45 = map(QQ^3, QQ^0, 0);
raysF45 = promote(raysF45, QQ);
linealityF45 = promote(linealityF45, QQ);
maxConesF45 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F45 = fan (raysF45 ,linealityF45 ,maxConesF45);
assert(dim F45 ==3)
assert(ambDim F45 ==3)
assert(#(maxCones F45) ==8)
assert(isSmooth F45)
assert(isPure F45)
assert(isSimplicial F45)
assert(isComplete F45)
assert(fVector F45 == {1, 6, 12, 8})
assert(isPolytopal F45)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF46 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF46 = map(QQ^3, QQ^0, 0);
raysF46 = promote(raysF46, QQ);
linealityF46 = promote(linealityF46, QQ);
maxConesF46 = {{1,3,4},{0,2,5},{1,2,5},{0,2,4},{1,2,4},{0,3,4},{0,3,5},{1,3,5}};
F46 = fan (raysF46 ,linealityF46 ,maxConesF46);
assert(dim F46 ==3)
assert(ambDim F46 ==3)
assert(#(maxCones F46) ==8)
assert(isSmooth F46)
assert(isPure F46)
assert(isSimplicial F46)
assert(isComplete F46)
assert(fVector F46 == {1, 6, 12, 8})
assert(isPolytopal F46)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
raysF47 = matrix {{-1,1,-1,1},{-1,-1,1,1}};
linealityF47 = map(QQ^2, QQ^0, 0);
raysF47 = promote(raysF47, QQ);
linealityF47 = promote(linealityF47, QQ);
maxConesF47 = {{0,2},{1,3},{0,1},{2,3}};
F47 = fan (raysF47 ,linealityF47 ,maxConesF47);
assert(dim F47 ==2)
assert(ambDim F47 ==2)
assert(#(maxCones F47) ==4)
assert(not isSmooth F47)
assert(isPure F47)
assert(isSimplicial F47)
assert(isComplete F47)
assert(fVector F47 == {1, 4, 4})
assert(isPolytopal F47)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
raysF48 = matrix {{-1,1,-1,1},{-1,-1,1,1}};
linealityF48 = map(QQ^2, QQ^0, 0);
raysF48 = promote(raysF48, QQ);
linealityF48 = promote(linealityF48, QQ);
maxConesF48 = {{0,2},{1,3},{0,1},{2,3}};
F48 = fan (raysF48 ,linealityF48 ,maxConesF48);
assert(dim F48 ==2)
assert(ambDim F48 ==2)
assert(#(maxCones F48) ==4)
assert(not isSmooth F48)
assert(isPure F48)
assert(isSimplicial F48)
assert(isComplete F48)
assert(fVector F48 == {1, 4, 4})
assert(isPolytopal F48)

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF49 = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF49 = map(QQ^3, QQ^0, 0);
raysF49 = promote(raysF49, QQ);
linealityF49 = promote(linealityF49, QQ);
maxConesF49 = {{0,1},{0,2}};
F49 = fan (raysF49 ,linealityF49 ,maxConesF49);
assert(dim F49 ==2)
assert(ambDim F49 ==3)
assert(#(maxCones F49) ==2)
assert(isSmooth F49)
assert(isPure F49)
assert(isSimplicial F49)
assert(not isComplete F49)
assert(fVector F49 == {1, 3, 2})
assert(not isPolytopal F49)

///

TEST ///
-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
raysF50 = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF50 = map(QQ^2, QQ^0, 0);
raysF50 = promote(raysF50, QQ);
linealityF50 = promote(linealityF50, QQ);
maxConesF50 = {{0,1},{2},{3}};
F50 = fan (raysF50 ,linealityF50 ,maxConesF50);
assert(dim F50 ==2)
assert(ambDim F50 ==2)
assert(#(maxCones F50) ==3)
assert(isSmooth F50)
assert(not isPure F50)
assert(isSimplicial F50)
assert(not isComplete F50)
assert(fVector F50 == {1, 4, 1})
assert(not isPolytopal F50)

-- Test ambDim: 3, dim: 3, nrays: 7, n_max_cones: 4
-- Checking misc tests for fan
raysF51 = matrix {{-1,0,-1,0,1,1,1},{-1,1,-2,0,1,0,-1},{-1,0,-1,1,-1,0,1}};
linealityF51 = map(QQ^3, QQ^0, 0);
raysF51 = promote(raysF51, QQ);
linealityF51 = promote(linealityF51, QQ);
maxConesF51 = {{0},{1,3,4,5},{2,6},{3,5,6}};
F51 = fan (raysF51 ,linealityF51 ,maxConesF51);
assert(dim F51 ==3)
assert(ambDim F51 ==3)
assert(#(maxCones F51) ==4)
assert(not isSmooth F51)
assert(not isPure F51)
assert(not isSimplicial F51)
assert(not isComplete F51)
assert(fVector F51 == {1, 7, 7, 2})
assert(not isPolytopal F51)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF52 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF52 = map(QQ^3, QQ^0, 0);
raysF52 = promote(raysF52, QQ);
linealityF52 = promote(linealityF52, QQ);
maxConesF52 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F52 = fan (raysF52 ,linealityF52 ,maxConesF52);
assert(dim F52 ==3)
assert(ambDim F52 ==3)
assert(#(maxCones F52) ==8)
assert(isSmooth F52)
assert(isPure F52)
assert(isSimplicial F52)
assert(isComplete F52)
assert(fVector F52 == {1, 6, 12, 8})
assert(isPolytopal F52)

-- Test ambDim: 3, dim: 3, nrays: 3, n_max_cones: 1
-- Checking misc tests for fan
raysF53 = matrix {{1,0,0},{0,1,0},{0,0,1}};
linealityF53 = map(QQ^3, QQ^0, 0);
raysF53 = promote(raysF53, QQ);
linealityF53 = promote(linealityF53, QQ);
maxConesF53 = {{0,1,2}};
F53 = fan (raysF53 ,linealityF53 ,maxConesF53);
assert(dim F53 ==3)
assert(ambDim F53 ==3)
assert(#(maxCones F53) ==1)
assert(isSmooth F53)
assert(isPure F53)
assert(isSimplicial F53)
assert(not isComplete F53)
assert(fVector F53 == {1, 3, 3, 1})
assert(not isPolytopal F53)

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF54 = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF54 = map(QQ^3, QQ^0, 0);
raysF54 = promote(raysF54, QQ);
linealityF54 = promote(linealityF54, QQ);
maxConesF54 = {{0,1},{0,2}};
F54 = fan (raysF54 ,linealityF54 ,maxConesF54);
assert(dim F54 ==2)
assert(ambDim F54 ==3)
assert(#(maxCones F54) ==2)
assert(isSmooth F54)
assert(isPure F54)
assert(isSimplicial F54)
assert(not isComplete F54)
assert(fVector F54 == {1, 3, 2})
assert(not isPolytopal F54)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
raysF55 = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF55 = map(QQ^2, QQ^0, 0);
raysF55 = promote(raysF55, QQ);
linealityF55 = promote(linealityF55, QQ);
maxConesF55 = {{0,1},{2},{3}};
F55 = fan (raysF55 ,linealityF55 ,maxConesF55);
assert(dim F55 ==2)
assert(ambDim F55 ==2)
assert(#(maxCones F55) ==3)
assert(isSmooth F55)
assert(not isPure F55)
assert(isSimplicial F55)
assert(not isComplete F55)
assert(fVector F55 == {1, 4, 1})
assert(not isPolytopal F55)

-- Test ambDim: 3, dim: 3, nrays: 7, n_max_cones: 4
-- Checking misc tests for fan
raysF56 = matrix {{-1,0,-1,0,1,1,1},{-1,1,-2,0,1,0,-1},{-1,0,-1,1,-1,0,1}};
linealityF56 = map(QQ^3, QQ^0, 0);
raysF56 = promote(raysF56, QQ);
linealityF56 = promote(linealityF56, QQ);
maxConesF56 = {{0},{1,3,4,5},{2,6},{3,5,6}};
F56 = fan (raysF56 ,linealityF56 ,maxConesF56);
assert(dim F56 ==3)
assert(ambDim F56 ==3)
assert(#(maxCones F56) ==4)
assert(not isSmooth F56)
assert(not isPure F56)
assert(not isSimplicial F56)
assert(not isComplete F56)
assert(fVector F56 == {1, 7, 7, 2})
assert(not isPolytopal F56)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF57 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF57 = map(QQ^3, QQ^0, 0);
raysF57 = promote(raysF57, QQ);
linealityF57 = promote(linealityF57, QQ);
maxConesF57 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F57 = fan (raysF57 ,linealityF57 ,maxConesF57);
assert(dim F57 ==3)
assert(ambDim F57 ==3)
assert(#(maxCones F57) ==8)
assert(isSmooth F57)
assert(isPure F57)
assert(isSimplicial F57)
assert(isComplete F57)
assert(fVector F57 == {1, 6, 12, 8})
assert(isPolytopal F57)

-- Test ambDim: 3, dim: 3, nrays: 8, n_max_cones: 6
-- Checking misc tests for fan
raysF58 = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,2},{-1,-1,-1,-1,1,1,1,3}};
linealityF58 = map(QQ^3, QQ^0, 0);
raysF58 = promote(raysF58, QQ);
linealityF58 = promote(linealityF58, QQ);
maxConesF58 = {{0,2,4,6},{1,3,5,7},{0,1,4,5},{2,3,6,7},{0,1,2,3},{4,5,6,7}};
F58 = fan (raysF58 ,linealityF58 ,maxConesF58);
assert(dim F58 ==3)
assert(ambDim F58 ==3)
assert(#(maxCones F58) ==6)
assert(not isSmooth F58)
assert(isPure F58)
assert(not isSimplicial F58)
assert(isComplete F58)
assert(fVector F58 == {1, 8, 12, 6})
assert(not isPolytopal F58)

-- Test ambDim: 4, dim: 4, nrays: 6, n_max_cones: 9
-- Checking misc tests for fan
raysF59 = matrix {{1,0,0,0,1,-1},{0,1,0,0,0,-1},{0,0,1,0,-1,0},{0,0,0,1,-1,0}};
linealityF59 = map(QQ^4, QQ^0, 0);
raysF59 = promote(raysF59, QQ);
linealityF59 = promote(linealityF59, QQ);
maxConesF59 = {{1,2,3,5},{1,2,4,5},{1,3,4,5},{0,3,4,5},{0,1,3,4},{0,1,2,3},{0,2,3,5},{0,1,2,4},{0,2,4,5}};
F59 = fan (raysF59 ,linealityF59 ,maxConesF59);
assert(dim F59 ==4)
assert(ambDim F59 ==4)
assert(#(maxCones F59) ==9)
assert(isSmooth F59)
assert(isPure F59)
assert(isSimplicial F59)
assert(isComplete F59)
assert(fVector F59 == {1, 6, 15, 18, 9})
assert(isPolytopal F59)

///

TEST ///
-- Test ambDim: 3, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
raysF60 = matrix {{1,0,-1,0},{0,1,0,-1},{0,0,0,0}};
linealityF60 = map(QQ^3, QQ^0, 0);
raysF60 = promote(raysF60, QQ);
linealityF60 = promote(linealityF60, QQ);
maxConesF60 = {{0,1},{1,2},{2,3},{0,3}};
F60 = fan (raysF60 ,linealityF60 ,maxConesF60);
assert(dim F60 ==2)
assert(ambDim F60 ==3)
assert(#(maxCones F60) ==4)
assert(isSmooth F60)
assert(isPure F60)
assert(isSimplicial F60)
assert(not isComplete F60)
assert(fVector F60 == {1, 4, 4})
assert(not isPolytopal F60)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF61 = matrix {{-1,1,0,-1,1,0},{-1,0,1,-1,0,1},{-1,-1,-1,1,1,1}};
linealityF61 = map(QQ^3, QQ^0, 0);
raysF61 = promote(raysF61, QQ);
linealityF61 = promote(linealityF61, QQ);
maxConesF61 = {{0,1,2},{3,4,5},{0,1,4},{0,3,4},{1,2,5},{1,4,5},{0,2,3},{2,3,5}};
F61 = fan (raysF61 ,linealityF61 ,maxConesF61);
assert(dim F61 ==3)
assert(ambDim F61 ==3)
assert(#(maxCones F61) ==8)
assert(not isSmooth F61)
assert(isPure F61)
assert(isSimplicial F61)
assert(isComplete F61)
assert(fVector F61 == {1, 6, 12, 8})
assert(not isPolytopal F61)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF62 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF62 = map(QQ^3, QQ^0, 0);
raysF62 = promote(raysF62, QQ);
linealityF62 = promote(linealityF62, QQ);
maxConesF62 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F62 = fan (raysF62 ,linealityF62 ,maxConesF62);
assert(dim F62 ==3)
assert(ambDim F62 ==3)
assert(#(maxCones F62) ==8)
assert(isSmooth F62)
assert(isPure F62)
assert(isSimplicial F62)
assert(isComplete F62)
assert(fVector F62 == {1, 6, 12, 8})
assert(isPolytopal F62)

-- Test ambDim: 4, dim: 4, nrays: 42, n_max_cones: 47
-- Checking misc tests for fan
raysF63 = matrix {{0,1,1,1,1,1,1,1,0,1,0,0,1,1,1,0,0,1,1,1,1,0,1,1,1,1,0,0,1,1,0,0,0,0,1,1,1,1,0,0,1,0},{1,1,1,3,0,1,1,1/3,0,0,1,1,0,1,0,1,1,0,1,0,1/3,1,3,1,3,1/3,0,0,0,0,1,1,1,1,0,0,1/3,3,0,0,0,1},{1,1,3,1,1,0,1,1/3,1,0,0,1,1,0,1,1,3,3,0,1/3,0,1/3,0,3,1,1/3,1,1,0,0,0,0,3,1/3,3,1/3,0,0,0,1,0,0},{1,3,1,1,1,1,0,1/3,1,1,1,0,0,0,3,3,1,1,3,1/3,1/3,1/3,1,0,0,0,3,1/3,3,1/3,3,1/3,0,0,0,0,0,0,1,0,0,0}};
linealityF63 = map(QQ^4, QQ^0, 0);
raysF63 = promote(raysF63, QQ);
linealityF63 = promote(linealityF63, QQ);
maxConesF63 = {{0,1,2,3,4,5,6,7},{0,1,2,4,8},{1,4,5,7,9},{0,1,3,5,10},{0,2,3,6,11},{2,4,6,7,12},{3,5,6,7,13},{1,4,8,14},{0,1,8,15},{0,2,8,16},{2,4,8,17},{1,4,9,14},{1,5,9,18},{4,7,9,19},{5,7,9,20},{0,1,10,15},{1,5,10,18},{0,3,10,21},{3,5,10,22},{0,2,11,16},{0,3,11,21},{2,6,11,23},{3,6,11,24},{2,4,12,17},{4,7,12,19},{2,6,12,23},{6,7,12,25},{5,7,13,20},{3,5,13,22},{3,6,13,24},{6,7,13,25},{1,8,14,15,26},{2,8,16,17,27},{1,9,14,18,28},{7,9,19,20,29},{1,10,15,18,30},{3,10,21,22,31},{2,11,16,23,32},{3,11,21,24,33},{2,12,17,23,34},{7,12,19,25,35},{7,13,20,25,36},{3,13,22,24,37},{1,14,15,18,26,28,30,38},{2,16,17,23,27,32,34,39},{7,19,20,25,29,35,36,40},{3,21,22,24,31,33,37,41}};
F63 = fan (raysF63 ,linealityF63 ,maxConesF63);
assert(dim F63 ==4)
assert(ambDim F63 ==4)
assert(#(maxCones F63) ==47)
assert(not isSmooth F63)
assert(isPure F63)
assert(not isSimplicial F63)
assert(not isComplete F63)
assert(fVector F63 == {1, 42, 132, 138, 47})
assert(not isPolytopal F63)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF64 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF64 = map(QQ^3, QQ^0, 0);
raysF64 = promote(raysF64, QQ);
linealityF64 = promote(linealityF64, QQ);
maxConesF64 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F64 = fan (raysF64 ,linealityF64 ,maxConesF64);
assert(dim F64 ==3)
assert(ambDim F64 ==3)
assert(#(maxCones F64) ==8)
assert(isSmooth F64)
assert(isPure F64)
assert(isSimplicial F64)
assert(isComplete F64)
assert(fVector F64 == {1, 6, 12, 8})
assert(isPolytopal F64)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF65 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF65 = map(QQ^3, QQ^0, 0);
raysF65 = promote(raysF65, QQ);
linealityF65 = promote(linealityF65, QQ);
maxConesF65 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F65 = fan (raysF65 ,linealityF65 ,maxConesF65);
assert(dim F65 ==3)
assert(ambDim F65 ==3)
assert(#(maxCones F65) ==8)
assert(isSmooth F65)
assert(isPure F65)
assert(isSimplicial F65)
assert(isComplete F65)
assert(fVector F65 == {1, 6, 12, 8})
assert(isPolytopal F65)

-- Test ambDim: 2, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF66 = matrix {{1,0,-1},{0,1,-1}};
linealityF66 = map(QQ^2, QQ^0, 0);
raysF66 = promote(raysF66, QQ);
linealityF66 = promote(linealityF66, QQ);
maxConesF66 = {{0,1},{2}};
F66 = fan (raysF66 ,linealityF66 ,maxConesF66);
assert(dim F66 ==2)
assert(ambDim F66 ==2)
assert(#(maxCones F66) ==2)
assert(isSmooth F66)
assert(not isPure F66)
assert(isSimplicial F66)
assert(not isComplete F66)
assert(fVector F66 == {1, 3, 1})
assert(not isPolytopal F66)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF67 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF67 = map(QQ^3, QQ^0, 0);
raysF67 = promote(raysF67, QQ);
linealityF67 = promote(linealityF67, QQ);
maxConesF67 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F67 = fan (raysF67 ,linealityF67 ,maxConesF67);
assert(dim F67 ==3)
assert(ambDim F67 ==3)
assert(#(maxCones F67) ==8)
assert(isSmooth F67)
assert(isPure F67)
assert(isSimplicial F67)
assert(isComplete F67)
assert(fVector F67 == {1, 6, 12, 8})
assert(isPolytopal F67)

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF68 = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF68 = map(QQ^3, QQ^0, 0);
raysF68 = promote(raysF68, QQ);
linealityF68 = promote(linealityF68, QQ);
maxConesF68 = {{0,1},{0,2}};
F68 = fan (raysF68 ,linealityF68 ,maxConesF68);
assert(dim F68 ==2)
assert(ambDim F68 ==3)
assert(#(maxCones F68) ==2)
assert(isSmooth F68)
assert(isPure F68)
assert(isSimplicial F68)
assert(not isComplete F68)
assert(fVector F68 == {1, 3, 2})
assert(not isPolytopal F68)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
raysF69 = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF69 = map(QQ^2, QQ^0, 0);
raysF69 = promote(raysF69, QQ);
linealityF69 = promote(linealityF69, QQ);
maxConesF69 = {{0,1},{2},{3}};
F69 = fan (raysF69 ,linealityF69 ,maxConesF69);
assert(dim F69 ==2)
assert(ambDim F69 ==2)
assert(#(maxCones F69) ==3)
assert(isSmooth F69)
assert(not isPure F69)
assert(isSimplicial F69)
assert(not isComplete F69)
assert(fVector F69 == {1, 4, 1})
assert(not isPolytopal F69)

///

TEST ///
-- Test ambDim: 3, dim: 3, nrays: 7, n_max_cones: 4
-- Checking misc tests for fan
raysF70 = matrix {{-1,0,-1,0,1,1,1},{-1,1,-2,0,1,0,-1},{-1,0,-1,1,-1,0,1}};
linealityF70 = map(QQ^3, QQ^0, 0);
raysF70 = promote(raysF70, QQ);
linealityF70 = promote(linealityF70, QQ);
maxConesF70 = {{0},{1,3,4,5},{2,6},{3,5,6}};
F70 = fan (raysF70 ,linealityF70 ,maxConesF70);
assert(dim F70 ==3)
assert(ambDim F70 ==3)
assert(#(maxCones F70) ==4)
assert(not isSmooth F70)
assert(not isPure F70)
assert(not isSimplicial F70)
assert(not isComplete F70)
assert(fVector F70 == {1, 7, 7, 2})
assert(not isPolytopal F70)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF71 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF71 = map(QQ^3, QQ^0, 0);
raysF71 = promote(raysF71, QQ);
linealityF71 = promote(linealityF71, QQ);
maxConesF71 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F71 = fan (raysF71 ,linealityF71 ,maxConesF71);
assert(dim F71 ==3)
assert(ambDim F71 ==3)
assert(#(maxCones F71) ==8)
assert(isSmooth F71)
assert(isPure F71)
assert(isSimplicial F71)
assert(isComplete F71)
assert(fVector F71 == {1, 6, 12, 8})
assert(isPolytopal F71)

-- Test ambDim: 3, dim: 2, nrays: 6, n_max_cones: 9
-- Checking misc tests for fan
raysF72 = matrix {{1,1,1,-1,1,-1},{818794080519290712218637983454656/829031252525274642799173400372205,-1708395506221045828793844317065/2509884118711564596175832378997,-89861355972553928970973245336569/17042562087395757815312397098412,1049518079883784126812314321891552/450042039024864499809788074031143,-57294552649102853504594142035824/101936291723111614375366730069055,-665091906365730277342873147817696/824708073670045873632314373349197},{508502279829950177343385417632574/829031252525274642799173400372205,-1554287588558661024133351439231/5019768237423129192351664757994,-117560758327001996748584699487539/17042562087395757815312397098412,729269897957095196268750301204717/1800168156099457999239152296124572,32205519897594406205837799691473/33978763907703871458455576689685,575327871058798074206062443400391/183268460815565749696069860744266}};
linealityF72 = map(QQ^3, QQ^0, 0);
raysF72 = promote(raysF72, QQ);
linealityF72 = promote(linealityF72, QQ);
maxConesF72 = {{0,1},{1,2},{2,3},{0,3},{1,4},{0,4},{2,5},{4,5},{3,5}};
F72 = fan (raysF72 ,linealityF72 ,maxConesF72);
assert(dim F72 ==2)
assert(ambDim F72 ==3)
assert(#(maxCones F72) ==9)
assert(not isSmooth F72)
assert(isPure F72)
assert(isSimplicial F72)
assert(not isComplete F72)
assert(fVector F72 == {1, 6, 9})
assert(not isPolytopal F72)

-- Test ambDim: 3, dim: 3, nrays: 26, n_max_cones: 24
-- Checking misc tests for fan
raysF73 = matrix {{1,1,1,1,0,0,0,-1,-1,-1,-1,1,1,0,0,-1,-1,1,1,0,0,-1,-1,1,0,-1},{0,1,1,0,1,1,0,0,1,1,0,-1,-1,-1,-1,-1,-1,1,0,1,0,1,0,-1,-1,-1},{0,1,0,1,0,1,1,0,1,0,1,1,0,0,1,1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1}};
linealityF73 = map(QQ^3, QQ^0, 0);
raysF73 = promote(raysF73, QQ);
linealityF73 = promote(linealityF73, QQ);
maxConesF73 = {{0,1,2,3},{1,2,4,5},{1,3,5,6},{7,8,9,10},{4,5,8,9},{5,6,8,10},{0,3,11,12},{11,12,13,14},{3,6,11,14},{7,10,15,16},{13,14,15,16},{6,10,14,15},{0,2,17,18},{2,4,17,19},{17,18,19,20},{7,9,21,22},{4,9,19,21},{19,20,21,22},{0,12,18,23},{12,13,23,24},{18,20,23,24},{7,16,22,25},{13,16,24,25},{20,22,24,25}};
F73 = fan (raysF73 ,linealityF73 ,maxConesF73);
assert(dim F73 ==3)
assert(ambDim F73 ==3)
assert(#(maxCones F73) ==24)
assert(not isSmooth F73)
assert(isPure F73)
assert(not isSimplicial F73)
assert(isComplete F73)
assert(fVector F73 == {1, 26, 48, 24})
assert(isPolytopal F73)

-- Test ambDim: 2, dim: 1, nrays: 2, n_max_cones: 2
-- Checking misc tests for fan
raysF74 = matrix {{1,-1},{1,-1}};
linealityF74 = map(QQ^2, QQ^0, 0);
raysF74 = promote(raysF74, QQ);
linealityF74 = promote(linealityF74, QQ);
maxConesF74 = {{0},{1}};
F74 = fan (raysF74 ,linealityF74 ,maxConesF74);
assert(dim F74 ==1)
assert(ambDim F74 ==2)
assert(#(maxCones F74) ==2)
assert(isSmooth F74)
assert(isPure F74)
assert(isSimplicial F74)
assert(not isComplete F74)
assert(fVector F74 == {1, 2})
assert(not isPolytopal F74)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
raysF75 = matrix {{1,0,-1,0},{0,1,0,-1}};
linealityF75 = map(QQ^2, QQ^0, 0);
raysF75 = promote(raysF75, QQ);
linealityF75 = promote(linealityF75, QQ);
maxConesF75 = {{0,1},{1,2},{2,3},{0,3}};
F75 = fan (raysF75 ,linealityF75 ,maxConesF75);
assert(dim F75 ==2)
assert(ambDim F75 ==2)
assert(#(maxCones F75) ==4)
assert(isSmooth F75)
assert(isPure F75)
assert(isSimplicial F75)
assert(isComplete F75)
assert(fVector F75 == {1, 4, 4})
assert(isPolytopal F75)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF76 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF76 = map(QQ^3, QQ^0, 0);
raysF76 = promote(raysF76, QQ);
linealityF76 = promote(linealityF76, QQ);
maxConesF76 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F76 = fan (raysF76 ,linealityF76 ,maxConesF76);
assert(dim F76 ==3)
assert(ambDim F76 ==3)
assert(#(maxCones F76) ==8)
assert(isSmooth F76)
assert(isPure F76)
assert(isSimplicial F76)
assert(isComplete F76)
assert(fVector F76 == {1, 6, 12, 8})
assert(isPolytopal F76)

-- Test ambDim: 3, dim: 3, nrays: 4, n_max_cones: 1
-- Checking misc tests for fan
raysF77 = matrix {{1,1,1,1},{2,2,-2,-2},{2,-2,2,-2}};
linealityF77 = map(QQ^3, QQ^0, 0);
raysF77 = promote(raysF77, QQ);
linealityF77 = promote(linealityF77, QQ);
maxConesF77 = {{0,1,2,3}};
F77 = fan (raysF77 ,linealityF77 ,maxConesF77);
assert(dim F77 ==3)
assert(ambDim F77 ==3)
assert(#(maxCones F77) ==1)
assert(not isSmooth F77)
assert(isPure F77)
assert(not isSimplicial F77)
assert(not isComplete F77)
assert(fVector F77 == {1, 4, 4, 1})
assert(not isPolytopal F77)

-- Test ambDim: 3, dim: 2, nrays: 6, n_max_cones: 9
-- Checking misc tests for fan
raysF78 = matrix {{-1,-1,-1,-1,1,1},{-1205394900074469874373270529299145/1219924955620354784582205359200148,254259858377295999839568073773176/148046853252149346413819602783863,2243462179548695050242687861482460/1504544056605790353640250717076991,2611761947406237914344776369881788/836649136082077893811605221470929,-1464482936131543532935907488283297/2245754515867165641441598773070946,-7492543451517178896179028037619435/9685081344024379534412048185367294},{2067170872801727856163589081805493/2439849911240709569164410718400296,2714624376991390424158764699813095/1184374826017194771310556822270904,-4175583222893067296766283081716952/1504544056605790353640250717076991,253610316070904946732597996386840/278883045360692631270535073823643,-150681337723949333338936635687700/1122877257933582820720799386535473,-1216361637388816490414928053374456/4842540672012189767206024092683647}};
linealityF78 = map(QQ^3, QQ^0, 0);
raysF78 = promote(raysF78, QQ);
linealityF78 = promote(linealityF78, QQ);
maxConesF78 = {{0,1},{2,3},{0,2},{1,3},{4,5},{0,5},{1,4},{2,5},{3,4}};
F78 = fan (raysF78 ,linealityF78 ,maxConesF78);
assert(dim F78 ==2)
assert(ambDim F78 ==3)
assert(#(maxCones F78) ==9)
assert(not isSmooth F78)
assert(isPure F78)
assert(isSimplicial F78)
assert(not isComplete F78)
assert(fVector F78 == {1, 6, 9})
assert(not isPolytopal F78)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
raysF79 = matrix {{1,-1,0,0},{0,0,1,-1}};
linealityF79 = map(QQ^2, QQ^0, 0);
raysF79 = promote(raysF79, QQ);
linealityF79 = promote(linealityF79, QQ);
maxConesF79 = {{0,2},{0,3},{1,2},{1,3}};
F79 = fan (raysF79 ,linealityF79 ,maxConesF79);
assert(dim F79 ==2)
assert(ambDim F79 ==2)
assert(#(maxCones F79) ==4)
assert(isSmooth F79)
assert(isPure F79)
assert(isSimplicial F79)
assert(isComplete F79)
assert(fVector F79 == {1, 4, 4})
assert(isPolytopal F79)

///

TEST ///
-- Test ambDim: 2, dim: 2, nrays: 2, n_max_cones: 1
-- Checking misc tests for fan
raysF80 = matrix {{1,-1},{0,1}};
linealityF80 = map(QQ^2, QQ^0, 0);
raysF80 = promote(raysF80, QQ);
linealityF80 = promote(linealityF80, QQ);
maxConesF80 = {{0,1}};
F80 = fan (raysF80 ,linealityF80 ,maxConesF80);
assert(dim F80 ==2)
assert(ambDim F80 ==2)
assert(#(maxCones F80) ==1)
assert(isSmooth F80)
assert(isPure F80)
assert(isSimplicial F80)
assert(not isComplete F80)
assert(fVector F80 == {1, 2, 1})
assert(not isPolytopal F80)

-- Test ambDim: 3, dim: 3, nrays: 4, n_max_cones: 1
-- Checking misc tests for fan
raysF81 = matrix {{1,1,1,1},{1,1,-1,-1},{1,-1,1,-1}};
linealityF81 = map(QQ^3, QQ^0, 0);
raysF81 = promote(raysF81, QQ);
linealityF81 = promote(linealityF81, QQ);
maxConesF81 = {{0,1,2,3}};
F81 = fan (raysF81 ,linealityF81 ,maxConesF81);
assert(dim F81 ==3)
assert(ambDim F81 ==3)
assert(#(maxCones F81) ==1)
assert(not isSmooth F81)
assert(isPure F81)
assert(not isSimplicial F81)
assert(not isComplete F81)
assert(fVector F81 == {1, 4, 4, 1})
assert(not isPolytopal F81)

-- Test ambDim: 3, dim: 1, nrays: 9, n_max_cones: 9
-- Checking misc tests for fan
raysF82 = matrix {{-1,-1,-1,1,-1,1,1,1,1},{1293979728731322079588387147955424/1712438211560724056148889068914273,30462351624619330260226372681081792/10196478602115919324126891460274647,152068420200329849224235051568195/134531827657486826638733806293982,-100051411100103709949459821946216/150381214932541824351341188556277,-992743498110247554806868862142581/303910908676062961508377082662302,-114930824870322725859722800237178/715216571008431039965238822244085,-1371725916620064457370316118978907/1854472312296316275632005978817650,3937954451592994032648807182828/265228143643495962897125993788577,3128926505495789804278361302472768/92545684373883375414959080116119},{24363516342674736410105952776873329/13699505692485792449191112551314184,6181275973857700312240096535829328/10196478602115919324126891460274647,-604899197274149933741600370862463/269063655314973653277467612587964,-22092236501875462380317377220987/150381214932541824351341188556277,81963292302580260335198188354388/50651818112677160251396180443717,4927520449762444076188034690180639/5721732568067448319721910577952680,-365617381734550053009221157097704/927236156148158137816002989408825,1533472706659392667347660977368/20402164895653535607471230291429,988675603333370524929137049198952/92545684373883375414959080116119}};
linealityF82 = map(QQ^3, QQ^0, 0);
raysF82 = promote(raysF82, QQ);
linealityF82 = promote(linealityF82, QQ);
maxConesF82 = {{0},{1},{2},{3},{4},{5},{6},{7},{8}};
F82 = fan (raysF82 ,linealityF82 ,maxConesF82);
assert(dim F82 ==1)
assert(ambDim F82 ==3)
assert(#(maxCones F82) ==9)
assert(isSmooth F82)
assert(isPure F82)
assert(isSimplicial F82)
assert(not isComplete F82)
assert(fVector F82 == {1, 9})
assert(not isPolytopal F82)

-- Test ambDim: 2, dim: 2, nrays: 8, n_max_cones: 8
-- Checking misc tests for fan
raysF83 = matrix {{1,0,-1,0,1,-1,-1,1},{0,1,0,-1,1,1,-1,-1}};
linealityF83 = map(QQ^2, QQ^0, 0);
raysF83 = promote(raysF83, QQ);
linealityF83 = promote(linealityF83, QQ);
maxConesF83 = {{1,4},{0,4},{1,5},{2,5},{2,6},{3,6},{3,7},{0,7}};
F83 = fan (raysF83 ,linealityF83 ,maxConesF83);
assert(dim F83 ==2)
assert(ambDim F83 ==2)
assert(#(maxCones F83) ==8)
assert(isSmooth F83)
assert(isPure F83)
assert(isSimplicial F83)
assert(isComplete F83)
assert(fVector F83 == {1, 8, 8})
assert(isPolytopal F83)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
raysF84 = matrix {{1,-1,-1,1},{1,1,-1,-1}};
linealityF84 = map(QQ^2, QQ^0, 0);
raysF84 = promote(raysF84, QQ);
linealityF84 = promote(linealityF84, QQ);
maxConesF84 = {{0,1},{1,2},{2,3},{0,3}};
F84 = fan (raysF84 ,linealityF84 ,maxConesF84);
assert(dim F84 ==2)
assert(ambDim F84 ==2)
assert(#(maxCones F84) ==4)
assert(not isSmooth F84)
assert(isPure F84)
assert(isSimplicial F84)
assert(isComplete F84)
assert(fVector F84 == {1, 4, 4})
assert(isPolytopal F84)

-- Test ambDim: 3, dim: 3, nrays: 8, n_max_cones: 6
-- Checking misc tests for fan
raysF85 = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
linealityF85 = map(QQ^3, QQ^0, 0);
raysF85 = promote(raysF85, QQ);
linealityF85 = promote(linealityF85, QQ);
maxConesF85 = {{0,2,4,6},{1,3,5,7},{0,1,4,5},{2,3,6,7},{0,1,2,3},{4,5,6,7}};
F85 = fan (raysF85 ,linealityF85 ,maxConesF85);
assert(dim F85 ==3)
assert(ambDim F85 ==3)
assert(#(maxCones F85) ==6)
assert(not isSmooth F85)
assert(isPure F85)
assert(not isSimplicial F85)
assert(isComplete F85)
assert(fVector F85 == {1, 8, 12, 6})
assert(isPolytopal F85)

-- Test ambDim: 2, dim: 2, nrays: 8, n_max_cones: 8
-- Checking misc tests for fan
raysF86 = matrix {{1,0,-1,0,1,-1,-1,1},{0,1,0,-1,1,1,-1,-1}};
linealityF86 = map(QQ^2, QQ^0, 0);
raysF86 = promote(raysF86, QQ);
linealityF86 = promote(linealityF86, QQ);
maxConesF86 = {{1,4},{1,5},{2,5},{2,6},{3,6},{3,7},{0,4},{0,7}};
F86 = fan (raysF86 ,linealityF86 ,maxConesF86);
assert(dim F86 ==2)
assert(ambDim F86 ==2)
assert(#(maxCones F86) ==8)
assert(isSmooth F86)
assert(isPure F86)
assert(isSimplicial F86)
assert(isComplete F86)
assert(fVector F86 == {1, 8, 8})
assert(isPolytopal F86)

-- Test ambDim: 3, dim: 3, nrays: 4, n_max_cones: 1
-- Checking misc tests for fan
raysF87 = matrix {{1,1,1,1},{1,1,-1,-1},{1,-1,-1,1}};
linealityF87 = map(QQ^3, QQ^0, 0);
raysF87 = promote(raysF87, QQ);
linealityF87 = promote(linealityF87, QQ);
maxConesF87 = {{0,1,2,3}};
F87 = fan (raysF87 ,linealityF87 ,maxConesF87);
assert(dim F87 ==3)
assert(ambDim F87 ==3)
assert(#(maxCones F87) ==1)
assert(not isSmooth F87)
assert(isPure F87)
assert(not isSimplicial F87)
assert(not isComplete F87)
assert(fVector F87 == {1, 4, 4, 1})
assert(not isPolytopal F87)

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF88 = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF88 = map(QQ^3, QQ^0, 0);
raysF88 = promote(raysF88, QQ);
linealityF88 = promote(linealityF88, QQ);
maxConesF88 = {{0,1},{0,2}};
F88 = fan (raysF88 ,linealityF88 ,maxConesF88);
assert(dim F88 ==2)
assert(ambDim F88 ==3)
assert(#(maxCones F88) ==2)
assert(isSmooth F88)
assert(isPure F88)
assert(isSimplicial F88)
assert(not isComplete F88)
assert(fVector F88 == {1, 3, 2})
assert(not isPolytopal F88)

-- Test ambDim: 3, dim: 1, nrays: 2, n_max_cones: 2
-- Checking misc tests for fan
raysF89 = matrix {{1,0},{0,1},{0,0}};
linealityF89 = map(QQ^3, QQ^0, 0);
raysF89 = promote(raysF89, QQ);
linealityF89 = promote(linealityF89, QQ);
maxConesF89 = {{0},{1}};
F89 = fan (raysF89 ,linealityF89 ,maxConesF89);
assert(dim F89 ==1)
assert(ambDim F89 ==3)
assert(#(maxCones F89) ==2)
assert(isSmooth F89)
assert(isPure F89)
assert(isSimplicial F89)
assert(not isComplete F89)
assert(fVector F89 == {1, 2})
assert(not isPolytopal F89)

///

TEST ///
-- Test ambDim: 1, dim: 1, nrays: 2, n_max_cones: 2
-- Checking misc tests for fan
raysF90 = matrix {{1,-1}};
linealityF90 = map(QQ^1, QQ^0, 0);
raysF90 = promote(raysF90, QQ);
linealityF90 = promote(linealityF90, QQ);
maxConesF90 = {{0},{1}};
F90 = fan (raysF90 ,linealityF90 ,maxConesF90);
assert(dim F90 ==1)
assert(ambDim F90 ==1)
assert(#(maxCones F90) ==2)
assert(isSmooth F90)
assert(isPure F90)
assert(isSimplicial F90)
assert(isComplete F90)
assert(fVector F90 == {1, 2})
assert(isPolytopal F90)

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF91 = matrix {{1,0,0},{0,1,0},{0,0,1}};
linealityF91 = map(QQ^3, QQ^0, 0);
raysF91 = promote(raysF91, QQ);
linealityF91 = promote(linealityF91, QQ);
maxConesF91 = {{0,1},{0,2}};
F91 = fan (raysF91 ,linealityF91 ,maxConesF91);
assert(dim F91 ==2)
assert(ambDim F91 ==3)
assert(#(maxCones F91) ==2)
assert(isSmooth F91)
assert(isPure F91)
assert(isSimplicial F91)
assert(not isComplete F91)
assert(fVector F91 == {1, 3, 2})
assert(not isPolytopal F91)

-- Test ambDim: 4, dim: 1, nrays: 1, n_max_cones: 1
-- Checking misc tests for fan
raysF92 = matrix {{0},{0},{0},{0}};
linealityF92 = map(QQ^4, QQ^0, 0);
raysF92 = promote(raysF92, QQ);
linealityF92 = promote(linealityF92, QQ);
maxConesF92 = {{0}};
F92 = fan (raysF92 ,linealityF92 ,maxConesF92);
assert(dim F92 ==0)
assert(ambDim F92 ==4)
assert(#(maxCones F92) ==1)
assert(isSmooth F92)
assert(isPure F92)
assert(isSimplicial F92)
assert(not isComplete F92)
assert(fVector F92 == {1})
assert(not isPolytopal F92)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF93 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF93 = map(QQ^3, QQ^0, 0);
raysF93 = promote(raysF93, QQ);
linealityF93 = promote(linealityF93, QQ);
maxConesF93 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F93 = fan (raysF93 ,linealityF93 ,maxConesF93);
assert(dim F93 ==3)
assert(ambDim F93 ==3)
assert(#(maxCones F93) ==8)
assert(isSmooth F93)
assert(isPure F93)
assert(isSimplicial F93)
assert(isComplete F93)
assert(fVector F93 == {1, 6, 12, 8})
assert(isPolytopal F93)

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF94 = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF94 = map(QQ^3, QQ^0, 0);
raysF94 = promote(raysF94, QQ);
linealityF94 = promote(linealityF94, QQ);
maxConesF94 = {{0,1},{0,2}};
F94 = fan (raysF94 ,linealityF94 ,maxConesF94);
assert(dim F94 ==2)
assert(ambDim F94 ==3)
assert(#(maxCones F94) ==2)
assert(isSmooth F94)
assert(isPure F94)
assert(isSimplicial F94)
assert(not isComplete F94)
assert(fVector F94 == {1, 3, 2})
assert(not isPolytopal F94)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
raysF95 = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF95 = map(QQ^2, QQ^0, 0);
raysF95 = promote(raysF95, QQ);
linealityF95 = promote(linealityF95, QQ);
maxConesF95 = {{0,1},{2},{3}};
F95 = fan (raysF95 ,linealityF95 ,maxConesF95);
assert(dim F95 ==2)
assert(ambDim F95 ==2)
assert(#(maxCones F95) ==3)
assert(isSmooth F95)
assert(not isPure F95)
assert(isSimplicial F95)
assert(not isComplete F95)
assert(fVector F95 == {1, 4, 1})
assert(not isPolytopal F95)

-- Test ambDim: 3, dim: 3, nrays: 7, n_max_cones: 4
-- Checking misc tests for fan
raysF96 = matrix {{-1,0,-1,0,1,1,1},{-1,1,-2,0,1,0,-1},{-1,0,-1,1,-1,0,1}};
linealityF96 = map(QQ^3, QQ^0, 0);
raysF96 = promote(raysF96, QQ);
linealityF96 = promote(linealityF96, QQ);
maxConesF96 = {{0},{1,3,4,5},{2,6},{3,5,6}};
F96 = fan (raysF96 ,linealityF96 ,maxConesF96);
assert(dim F96 ==3)
assert(ambDim F96 ==3)
assert(#(maxCones F96) ==4)
assert(not isSmooth F96)
assert(not isPure F96)
assert(not isSimplicial F96)
assert(not isComplete F96)
assert(fVector F96 == {1, 7, 7, 2})
assert(not isPolytopal F96)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF97 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF97 = map(QQ^3, QQ^0, 0);
raysF97 = promote(raysF97, QQ);
linealityF97 = promote(linealityF97, QQ);
maxConesF97 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F97 = fan (raysF97 ,linealityF97 ,maxConesF97);
assert(dim F97 ==3)
assert(ambDim F97 ==3)
assert(#(maxCones F97) ==8)
assert(isSmooth F97)
assert(isPure F97)
assert(isSimplicial F97)
assert(isComplete F97)
assert(fVector F97 == {1, 6, 12, 8})
assert(isPolytopal F97)

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF98 = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF98 = map(QQ^3, QQ^0, 0);
raysF98 = promote(raysF98, QQ);
linealityF98 = promote(linealityF98, QQ);
maxConesF98 = {{0,2},{0,1}};
F98 = fan (raysF98 ,linealityF98 ,maxConesF98);
assert(dim F98 ==2)
assert(ambDim F98 ==3)
assert(#(maxCones F98) ==2)
assert(isSmooth F98)
assert(isPure F98)
assert(isSimplicial F98)
assert(not isComplete F98)
assert(fVector F98 == {1, 3, 2})
assert(not isPolytopal F98)

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF99 = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF99 = map(QQ^3, QQ^0, 0);
raysF99 = promote(raysF99, QQ);
linealityF99 = promote(linealityF99, QQ);
maxConesF99 = {{0,1},{0,2}};
F99 = fan (raysF99 ,linealityF99 ,maxConesF99);
assert(dim F99 ==2)
assert(ambDim F99 ==3)
assert(#(maxCones F99) ==2)
assert(isSmooth F99)
assert(isPure F99)
assert(isSimplicial F99)
assert(not isComplete F99)
assert(fVector F99 == {1, 3, 2})
assert(not isPolytopal F99)

///

TEST ///
-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF100 = matrix {{1,0,0},{0,-1,1},{0,0,0}};
linealityF100 = map(QQ^3, QQ^0, 0);
raysF100 = promote(raysF100, QQ);
linealityF100 = promote(linealityF100, QQ);
maxConesF100 = {{0,2},{0,1}};
F100 = fan (raysF100 ,linealityF100 ,maxConesF100);
assert(dim F100 ==2)
assert(ambDim F100 ==3)
assert(#(maxCones F100) ==2)
assert(isSmooth F100)
assert(isPure F100)
assert(isSimplicial F100)
assert(not isComplete F100)
assert(fVector F100 == {1, 3, 2})
assert(not isPolytopal F100)

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF101 = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF101 = map(QQ^3, QQ^0, 0);
raysF101 = promote(raysF101, QQ);
linealityF101 = promote(linealityF101, QQ);
maxConesF101 = {{0,1},{0,2}};
F101 = fan (raysF101 ,linealityF101 ,maxConesF101);
assert(dim F101 ==2)
assert(ambDim F101 ==3)
assert(#(maxCones F101) ==2)
assert(isSmooth F101)
assert(isPure F101)
assert(isSimplicial F101)
assert(not isComplete F101)
assert(fVector F101 == {1, 3, 2})
assert(not isPolytopal F101)

-- Test ambDim: 3, dim: 3, nrays: 4, n_max_cones: 2
-- Checking misc tests for fan
raysF102 = matrix {{1,0,0,0},{0,1,0,0},{0,0,1,-1}};
linealityF102 = map(QQ^3, QQ^0, 0);
raysF102 = promote(raysF102, QQ);
linealityF102 = promote(linealityF102, QQ);
maxConesF102 = {{0,1,2},{0,1,3}};
F102 = fan (raysF102 ,linealityF102 ,maxConesF102);
assert(dim F102 ==3)
assert(ambDim F102 ==3)
assert(#(maxCones F102) ==2)
assert(isSmooth F102)
assert(isPure F102)
assert(isSimplicial F102)
assert(not isComplete F102)
assert(fVector F102 == {1, 4, 5, 2})
assert(not isPolytopal F102)

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF103 = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF103 = map(QQ^3, QQ^0, 0);
raysF103 = promote(raysF103, QQ);
linealityF103 = promote(linealityF103, QQ);
maxConesF103 = {{0,1},{0,2}};
F103 = fan (raysF103 ,linealityF103 ,maxConesF103);
assert(dim F103 ==2)
assert(ambDim F103 ==3)
assert(#(maxCones F103) ==2)
assert(isSmooth F103)
assert(isPure F103)
assert(isSimplicial F103)
assert(not isComplete F103)
assert(fVector F103 == {1, 3, 2})
assert(not isPolytopal F103)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
raysF104 = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF104 = map(QQ^2, QQ^0, 0);
raysF104 = promote(raysF104, QQ);
linealityF104 = promote(linealityF104, QQ);
maxConesF104 = {{0,1},{2},{3}};
F104 = fan (raysF104 ,linealityF104 ,maxConesF104);
assert(dim F104 ==2)
assert(ambDim F104 ==2)
assert(#(maxCones F104) ==3)
assert(isSmooth F104)
assert(not isPure F104)
assert(isSimplicial F104)
assert(not isComplete F104)
assert(fVector F104 == {1, 4, 1})
assert(not isPolytopal F104)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF105 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF105 = map(QQ^3, QQ^0, 0);
raysF105 = promote(raysF105, QQ);
linealityF105 = promote(linealityF105, QQ);
maxConesF105 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F105 = fan (raysF105 ,linealityF105 ,maxConesF105);
assert(dim F105 ==3)
assert(ambDim F105 ==3)
assert(#(maxCones F105) ==8)
assert(isSmooth F105)
assert(isPure F105)
assert(isSimplicial F105)
assert(isComplete F105)
assert(fVector F105 == {1, 6, 12, 8})
assert(isPolytopal F105)

-- Test ambDim: 3, dim: 3, nrays: 8, n_max_cones: 6
-- Checking misc tests for fan
raysF106 = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,2},{-1,-1,-1,-1,1,1,1,3}};
linealityF106 = map(QQ^3, QQ^0, 0);
raysF106 = promote(raysF106, QQ);
linealityF106 = promote(linealityF106, QQ);
maxConesF106 = {{0,2,4,6},{1,3,5,7},{0,1,4,5},{2,3,6,7},{0,1,2,3},{4,5,6,7}};
F106 = fan (raysF106 ,linealityF106 ,maxConesF106);
assert(dim F106 ==3)
assert(ambDim F106 ==3)
assert(#(maxCones F106) ==6)
assert(not isSmooth F106)
assert(isPure F106)
assert(not isSimplicial F106)
assert(isComplete F106)
assert(fVector F106 == {1, 8, 12, 6})
assert(not isPolytopal F106)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
raysF107 = matrix {{1,-1,0,1},{0,-1,1,6}};
linealityF107 = map(QQ^2, QQ^0, 0);
raysF107 = promote(raysF107, QQ);
linealityF107 = promote(linealityF107, QQ);
maxConesF107 = {{0,1},{1,2},{2,3},{0,3}};
F107 = fan (raysF107 ,linealityF107 ,maxConesF107);
assert(dim F107 ==2)
assert(ambDim F107 ==2)
assert(#(maxCones F107) ==4)
assert(not isSmooth F107)
assert(isPure F107)
assert(isSimplicial F107)
assert(isComplete F107)
assert(fVector F107 == {1, 4, 4})
assert(isPolytopal F107)

-- Test ambDim: 4, dim: 4, nrays: 6, n_max_cones: 9
-- Checking misc tests for fan
raysF108 = matrix {{1,0,0,0,1,-1},{0,1,0,0,0,-1},{0,0,1,0,-1,0},{0,0,0,1,-1,0}};
linealityF108 = map(QQ^4, QQ^0, 0);
raysF108 = promote(raysF108, QQ);
linealityF108 = promote(linealityF108, QQ);
maxConesF108 = {{1,2,3,5},{1,2,4,5},{1,3,4,5},{0,3,4,5},{0,1,3,4},{0,1,2,3},{0,2,3,5},{0,1,2,4},{0,2,4,5}};
F108 = fan (raysF108 ,linealityF108 ,maxConesF108);
assert(dim F108 ==4)
assert(ambDim F108 ==4)
assert(#(maxCones F108) ==9)
assert(isSmooth F108)
assert(isPure F108)
assert(isSimplicial F108)
assert(isComplete F108)
assert(fVector F108 == {1, 6, 15, 18, 9})
assert(isPolytopal F108)

-- Test ambDim: 4, dim: 4, nrays: 43, n_max_cones: 107
-- Checking misc tests for fan
raysF109 = matrix {{0,1,1,1,1,1,1,1,0,1,0,0,1,1,1,0,0,1,1,1,1,0,1,1,1,1,0,0,1,1,0,0,0,0,1,1,1,1,0,0,1,0,-1},{1,1,1,3,0,1,1,1/3,0,0,1,1,0,1,0,1,1,0,1,0,1/3,1,3,1,3,1/3,0,0,0,0,1,1,1,1,0,0,1/3,3,0,0,0,1,-1},{1,1,3,1,1,0,1,1/3,1,0,0,1,1,0,1,1,3,3,0,1/3,0,1/3,0,3,1,1/3,1,1,0,0,0,0,3,1/3,3,1/3,0,0,0,1,0,0,-1},{1,3,1,1,1,1,0,1/3,1,1,1,0,0,0,3,3,1,1,3,1/3,1/3,1/3,1,0,0,0,3,1/3,3,1/3,3,1/3,0,0,0,0,0,0,1,0,0,0,-1}};
linealityF109 = map(QQ^4, QQ^0, 0);
raysF109 = promote(raysF109, QQ);
linealityF109 = promote(linealityF109, QQ);
maxConesF109 = {{0,1,2,3,4,5,6,7},{0,1,2,4,8},{1,4,5,7,9},{0,1,3,5,10},{0,2,3,6,11},{2,4,6,7,12},{3,5,6,7,13},{1,4,8,14},{0,1,8,15},{0,2,8,16},{2,4,8,17},{1,4,9,14},{1,5,9,18},{4,7,9,19},{5,7,9,20},{0,1,10,15},{1,5,10,18},{0,3,10,21},{3,5,10,22},{0,2,11,16},{0,3,11,21},{2,6,11,23},{3,6,11,24},{2,4,12,17},{4,7,12,19},{2,6,12,23},{6,7,12,25},{5,7,13,20},{3,5,13,22},{3,6,13,24},{6,7,13,25},{1,8,14,15,26},{2,8,16,17,27},{1,9,14,18,28},{7,9,19,20,29},{1,10,15,18,30},{3,10,21,22,31},{2,11,16,23,32},{3,11,21,24,33},{2,12,17,23,34},{7,12,19,25,35},{7,13,20,25,36},{3,13,22,24,37},{1,14,15,18,26,28,30,38},{2,16,17,23,27,32,34,39},{7,19,20,25,29,35,36,40},{3,21,22,24,31,33,37,41},{4,8,14,42},{0,8,15,42},{0,8,16,42},{4,8,17,42},{4,9,14,42},{5,9,18,42},{4,9,19,42},{5,9,20,42},{0,10,15,42},{5,10,18,42},{0,10,21,42},{5,10,22,42},{0,11,16,42},{0,11,21,42},{6,11,23,42},{6,11,24,42},{4,12,17,42},{4,12,19,42},{6,12,23,42},{6,12,25,42},{5,13,20,42},{5,13,22,42},{6,13,24,42},{6,13,25,42},{8,15,26,42},{8,14,26,42},{8,17,27,42},{8,16,27,42},{9,14,28,42},{9,18,28,42},{9,19,29,42},{9,20,29,42},{10,15,30,42},{10,18,30,42},{10,21,31,42},{10,22,31,42},{11,16,32,42},{11,23,32,42},{11,21,33,42},{11,24,33,42},{12,17,34,42},{12,23,34,42},{12,19,35,42},{12,25,35,42},{13,20,36,42},{13,25,36,42},{13,22,37,42},{13,24,37,42},{15,26,30,38,42},{14,26,28,38,42},{18,28,30,38,42},{17,27,34,39,42},{16,27,32,39,42},{23,32,34,39,42},{19,29,35,40,42},{20,29,36,40,42},{25,35,36,40,42},{21,31,33,41,42},{22,31,37,41,42},{24,33,37,41,42}};
F109 = fan (raysF109 ,linealityF109 ,maxConesF109);
assert(dim F109 ==4)
assert(ambDim F109 ==4)
assert(#(maxCones F109) ==107)
assert(not isSmooth F109)
assert(isPure F109)
assert(not isSimplicial F109)
assert(fVector F109 == {1, 43, 170, 234, 107})

///

TEST ///
-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF110 = matrix {{-1,1,0,-1,1,0},{-1,0,1,-1,0,1},{-1,-1,-1,1,1,1}};
linealityF110 = map(QQ^3, QQ^0, 0);
raysF110 = promote(raysF110, QQ);
linealityF110 = promote(linealityF110, QQ);
maxConesF110 = {{0,1,2},{3,4,5},{0,1,4},{0,3,4},{1,2,5},{1,4,5},{0,2,3},{2,3,5}};
F110 = fan (raysF110 ,linealityF110 ,maxConesF110);
assert(dim F110 ==3)
assert(ambDim F110 ==3)
assert(#(maxCones F110) ==8)
assert(not isSmooth F110)
assert(isPure F110)
assert(isSimplicial F110)
assert(isComplete F110)
assert(fVector F110 == {1, 6, 12, 8})
assert(not isPolytopal F110)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF111 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF111 = map(QQ^3, QQ^0, 0);
raysF111 = promote(raysF111, QQ);
linealityF111 = promote(linealityF111, QQ);
maxConesF111 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F111 = fan (raysF111 ,linealityF111 ,maxConesF111);
assert(dim F111 ==3)
assert(ambDim F111 ==3)
assert(#(maxCones F111) ==8)
assert(isSmooth F111)
assert(isPure F111)
assert(isSimplicial F111)
assert(isComplete F111)
assert(fVector F111 == {1, 6, 12, 8})
assert(isPolytopal F111)

-- Test ambDim: 3, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
raysF112 = matrix {{1,0,-1,0},{0,1,0,-1},{0,0,0,0}};
linealityF112 = map(QQ^3, QQ^0, 0);
raysF112 = promote(raysF112, QQ);
linealityF112 = promote(linealityF112, QQ);
maxConesF112 = {{0,1},{1,2},{2,3},{0,3}};
F112 = fan (raysF112 ,linealityF112 ,maxConesF112);
assert(dim F112 ==2)
assert(ambDim F112 ==3)
assert(#(maxCones F112) ==4)
assert(isSmooth F112)
assert(isPure F112)
assert(isSimplicial F112)
assert(not isComplete F112)
assert(fVector F112 == {1, 4, 4})
assert(not isPolytopal F112)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
raysF113 = matrix {{0,1,0,-1},{1,0,-1,0}};
linealityF113 = map(QQ^2, QQ^0, 0);
raysF113 = promote(raysF113, QQ);
linealityF113 = promote(linealityF113, QQ);
maxConesF113 = {{0,1},{1,2},{2,3},{0,3}};
F113 = fan (raysF113 ,linealityF113 ,maxConesF113);
assert(dim F113 ==2)
assert(ambDim F113 ==2)
assert(#(maxCones F113) ==4)
assert(isSmooth F113)
assert(isPure F113)
assert(isSimplicial F113)
assert(isComplete F113)
assert(fVector F113 == {1, 4, 4})
assert(isPolytopal F113)

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF114 = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF114 = map(QQ^3, QQ^0, 0);
raysF114 = promote(raysF114, QQ);
linealityF114 = promote(linealityF114, QQ);
maxConesF114 = {{0,1},{0,2}};
F114 = fan (raysF114 ,linealityF114 ,maxConesF114);
assert(dim F114 ==2)
assert(ambDim F114 ==3)
assert(#(maxCones F114) ==2)
assert(isSmooth F114)
assert(isPure F114)
assert(isSimplicial F114)
assert(not isComplete F114)
assert(fVector F114 == {1, 3, 2})
assert(not isPolytopal F114)

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
raysF115 = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF115 = map(QQ^2, QQ^0, 0);
raysF115 = promote(raysF115, QQ);
linealityF115 = promote(linealityF115, QQ);
maxConesF115 = {{0,1},{2},{3}};
F115 = fan (raysF115 ,linealityF115 ,maxConesF115);
assert(dim F115 ==2)
assert(ambDim F115 ==2)
assert(#(maxCones F115) ==3)
assert(isSmooth F115)
assert(not isPure F115)
assert(isSimplicial F115)
assert(not isComplete F115)
assert(fVector F115 == {1, 4, 1})
assert(not isPolytopal F115)

-- Test ambDim: 3, dim: 3, nrays: 7, n_max_cones: 4
-- Checking misc tests for fan
raysF116 = matrix {{-1,0,-1,0,1,1,1},{-1,1,-2,0,1,0,-1},{-1,0,-1,1,-1,0,1}};
linealityF116 = map(QQ^3, QQ^0, 0);
raysF116 = promote(raysF116, QQ);
linealityF116 = promote(linealityF116, QQ);
maxConesF116 = {{0},{1,3,4,5},{2,6},{3,5,6}};
F116 = fan (raysF116 ,linealityF116 ,maxConesF116);
assert(dim F116 ==3)
assert(ambDim F116 ==3)
assert(#(maxCones F116) ==4)
assert(not isSmooth F116)
assert(not isPure F116)
assert(not isSimplicial F116)
assert(not isComplete F116)
assert(fVector F116 == {1, 7, 7, 2})
assert(not isPolytopal F116)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF117 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF117 = map(QQ^3, QQ^0, 0);
raysF117 = promote(raysF117, QQ);
linealityF117 = promote(linealityF117, QQ);
maxConesF117 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F117 = fan (raysF117 ,linealityF117 ,maxConesF117);
assert(dim F117 ==3)
assert(ambDim F117 ==3)
assert(#(maxCones F117) ==8)
assert(isSmooth F117)
assert(isPure F117)
assert(isSimplicial F117)
assert(isComplete F117)
assert(fVector F117 == {1, 6, 12, 8})
assert(isPolytopal F117)

-- Test ambDim: 5, dim: 3, nrays: 20, n_max_cones: 170
-- Checking misc tests for fan
raysF118 = matrix {{-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},{0,0,0,0,0,0,0,-1,0,0,0,1,0,0,0,0,0,0,0,0},{0,-1,1,-1,-1,1,1,0,-1,1,1,0,-1,1,1,-1,1,-1,-1,0},{0,-2498297281740881198678291544546863/4292233922170347411817814193674192,-7012538792226086112924358907263123/8715740054670143779591681945787888,-853168194540718366519552314380549/1633604436597579867756918855037712,-2376368850200680272846378297742423/1188965054868984553175757399859904,3959858915640440808161367802075969/1756385359147711885281230275771760,-15370518860189209722653530257365101/4032359592672019241882820197740480,0,-33005235043899437497208274745288/105868656415338140456586840412833,723365086110191345493168434272/387998850010162462078593566713487,25362665923488389096629268456676/511676852640953683525248935581739,0,284734156325347081245652018033768/249884178116974368294619025288309,247742622372680406219772156972539/651910696131929186067030561586961,-11920975463394952023387172305872/5901745461923206181439973021473,-108239975558002546890967166653714/261036258360191737714509799678319,539426970431570236277770871109463/130987853236973852203880901389345,25170421131309517652087703934203/15926414258505777594363147697561,-20303616827570155889020353601744/9102424464896945257519978842191,0},{0,3878447178730676951910694486166429/8584467844340694823635628387348384,12509787701488839738937439262537009/17431480109340287559183363891575776,3124204339441645711919396014383823/6534417746390319471027675420150848,477892342042613973646209057148781/594482527434492276587878699929952,26099595939955105780012082333606671/7025541436590847541124921103087040,983540656839097004575785510609369/1008089898168004810470705049435120,0,-330103335103499526366998142504871/423474625661352561826347361651332,-838442066190720239385936283948709/387998850010162462078593566713487,-930713632552731931854669041770003/438580159406531728735927659070062,0,-2662382051315823227432821454096327/1999073424935794946356952202306472,-853819649472531556941987776395057/1303821392263858372134061123173922,-16616654466592771089357379907323/5901745461923206181439973021473,-177602914497116110289244987475037/261036258360191737714509799678319,843580423007891323234545346409517/523951412947895408815523605557380,-20356059935560866960509607801117/63705657034023110377452590790244,-24375837395791716729326666520827/9102424464896945257519978842191,0}};
linealityF118 = map(QQ^5, QQ^0, 0);
raysF118 = promote(raysF118, QQ);
linealityF118 = promote(linealityF118, QQ);
maxConesF118 = {{0,1,3},{0,1,4},{0,1,7},{0,1,11},{0,1,15},{0,2,5},{0,2,6},{0,2,7},{0,2,11},{0,2,13},{0,3,5},{0,3,7},{0,3,11},{0,3,17},{0,4,6},{0,4,7},{0,4,11},{0,4,18},{0,5,7},{0,5,11},{0,5,16},{0,6,7},{0,6,11},{0,6,14},{0,7,8},{0,7,9},{0,7,10},{0,7,12},{0,7,13},{0,7,14},{0,7,15},{0,7,16},{0,7,17},{0,7,18},{0,8,9},{0,8,11},{0,8,12},{0,8,15},{0,9,10},{0,9,11},{0,9,14},{0,10,11},{0,10,12},{0,10,13},{0,11,12},{0,11,13},{0,11,14},{0,11,15},{0,11,16},{0,11,17},{0,11,18},{0,12,17},{0,13,16},{0,14,18},{0,15,18},{0,16,17},{1,2,3,4,5,6},{1,3,7},{1,3,8,12,15,17},{1,3,11},{1,3,19},{1,4,7},{1,4,11},{1,4,15,18},{1,4,19},{1,7,15},{1,7,19},{1,11,15},{1,11,19},{1,15,19},{2,5,7},{2,5,11},{2,5,13,16},{2,5,19},{2,6,7},{2,6,9,10,13,14},{2,6,11},{2,6,19},{2,7,13},{2,7,19},{2,11,13},{2,11,19},{2,13,19},{3,5,7},{3,5,11},{3,5,16,17},{3,5,19},{3,7,17},{3,7,19},{3,11,17},{3,11,19},{3,17,19},{4,6,7},{4,6,11},{4,6,14,18},{4,6,19},{4,7,18},{4,7,19},{4,11,18},{4,11,19},{4,18,19},{5,7,16},{5,7,19},{5,11,16},{5,11,19},{5,16,19},{6,7,14},{6,7,19},{6,11,14},{6,11,19},{6,14,19},{7,8,9},{7,8,12},{7,8,15},{7,8,19},{7,9,10},{7,9,14},{7,9,19},{7,10,12},{7,10,13},{7,10,19},{7,12,17},{7,12,19},{7,13,16},{7,13,19},{7,14,18},{7,14,19},{7,15,18},{7,15,19},{7,16,17},{7,16,19},{7,17,19},{7,18,19},{8,9,10,12},{8,9,11},{8,9,14,15,18},{8,9,19},{8,11,12},{8,11,15},{8,11,19},{8,12,19},{8,15,19},{9,10,11},{9,10,19},{9,11,14},{9,11,19},{9,14,19},{10,11,12},{10,11,13},{10,11,19},{10,12,13,16,17},{10,12,19},{10,13,19},{11,12,17},{11,12,19},{11,13,16},{11,13,19},{11,14,18},{11,14,19},{11,15,18},{11,15,19},{11,16,17},{11,16,19},{11,17,19},{11,18,19},{12,17,19},{13,16,19},{14,18,19},{15,18,19},{16,17,19}};
F118 = fan (raysF118 ,linealityF118 ,maxConesF118);
assert(dim F118 ==3)
assert(ambDim F118 ==5)
assert(#(maxCones F118) ==170)
assert(not isSmooth F118)
assert(isPure F118)
assert(not isSimplicial F118)
assert(not isComplete F118)
assert(fVector F118 == {1, 20, 92, 170})
assert(not isPolytopal F118)

-- Test ambDim: 3, dim: 3, nrays: 5, n_max_cones: 5
-- Checking misc tests for fan
raysF119 = matrix {{1,0,0,-1,-1},{0,1,0,-1,-2},{0,0,1,-1,-3}};
linealityF119 = map(QQ^3, QQ^0, 0);
raysF119 = promote(raysF119, QQ);
linealityF119 = promote(linealityF119, QQ);
maxConesF119 = {{4},{0,3},{1,3},{2,3},{0,1,2}};
F119 = fan (raysF119 ,linealityF119 ,maxConesF119);
assert(dim F119 ==3)
assert(ambDim F119 ==3)
assert(#(maxCones F119) ==5)
assert(isSmooth F119)
assert(not isPure F119)
assert(isSimplicial F119)
assert(not isComplete F119)
assert(fVector F119 == {1, 5, 6, 1})
assert(not isPolytopal F119)

///

TEST ///
-- Test ambDim: 3, dim: 3, nrays: 5, n_max_cones: 5
-- Checking misc tests for fan
raysF120 = matrix {{1,0,0,-1,-1},{0,1,0,-1,-2},{0,0,1,-1,-3}};
linealityF120 = map(QQ^3, QQ^0, 0);
raysF120 = promote(raysF120, QQ);
linealityF120 = promote(linealityF120, QQ);
maxConesF120 = {{0,1,2},{0,3},{1,3},{2,3},{4}};
F120 = fan (raysF120 ,linealityF120 ,maxConesF120);
assert(dim F120 ==3)
assert(ambDim F120 ==3)
assert(#(maxCones F120) ==5)
assert(isSmooth F120)
assert(not isPure F120)
assert(isSimplicial F120)
assert(not isComplete F120)
assert(fVector F120 == {1, 5, 6, 1})
assert(not isPolytopal F120)

-- Test ambDim: 5, dim: 4, nrays: 20, n_max_cones: 136
-- Checking misc tests for fan
raysF121 = matrix {{-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},{0,0,0,0,0,0,0,-1,0,0,0,1,0,0,0,0,0,0,0,0},{0,-1,1,-1,-1,1,1,0,-1,1,1,0,-1,1,1,-1,1,-1,-1,0},{0,-2498297281740881198678291544546863/4292233922170347411817814193674192,-7012538792226086112924358907263123/8715740054670143779591681945787888,-853168194540718366519552314380549/1633604436597579867756918855037712,-2376368850200680272846378297742423/1188965054868984553175757399859904,3959858915640440808161367802075969/1756385359147711885281230275771760,-15370518860189209722653530257365101/4032359592672019241882820197740480,0,-33005235043899437497208274745288/105868656415338140456586840412833,723365086110191345493168434272/387998850010162462078593566713487,25362665923488389096629268456676/511676852640953683525248935581739,0,284734156325347081245652018033768/249884178116974368294619025288309,247742622372680406219772156972539/651910696131929186067030561586961,-11920975463394952023387172305872/5901745461923206181439973021473,-108239975558002546890967166653714/261036258360191737714509799678319,539426970431570236277770871109463/130987853236973852203880901389345,25170421131309517652087703934203/15926414258505777594363147697561,-20303616827570155889020353601744/9102424464896945257519978842191,0},{0,3878447178730676951910694486166429/8584467844340694823635628387348384,12509787701488839738937439262537009/17431480109340287559183363891575776,3124204339441645711919396014383823/6534417746390319471027675420150848,477892342042613973646209057148781/594482527434492276587878699929952,26099595939955105780012082333606671/7025541436590847541124921103087040,983540656839097004575785510609369/1008089898168004810470705049435120,0,-330103335103499526366998142504871/423474625661352561826347361651332,-838442066190720239385936283948709/387998850010162462078593566713487,-930713632552731931854669041770003/438580159406531728735927659070062,0,-2662382051315823227432821454096327/1999073424935794946356952202306472,-853819649472531556941987776395057/1303821392263858372134061123173922,-16616654466592771089357379907323/5901745461923206181439973021473,-177602914497116110289244987475037/261036258360191737714509799678319,843580423007891323234545346409517/523951412947895408815523605557380,-20356059935560866960509607801117/63705657034023110377452590790244,-24375837395791716729326666520827/9102424464896945257519978842191,0}};
linealityF121 = map(QQ^5, QQ^0, 0);
raysF121 = promote(raysF121, QQ);
linealityF121 = promote(linealityF121, QQ);
maxConesF121 = {{0,1,2,3,4,5,6},{0,1,3,7},{0,1,3,8,12,15,17},{0,1,3,11},{0,1,4,7},{0,1,4,11},{0,1,4,15,18},{0,1,7,15},{0,1,11,15},{0,2,5,7},{0,2,5,11},{0,2,5,13,16},{0,2,6,7},{0,2,6,9,10,13,14},{0,2,6,11},{0,2,7,13},{0,2,11,13},{0,3,5,7},{0,3,5,11},{0,3,5,16,17},{0,3,7,17},{0,3,11,17},{0,4,6,7},{0,4,6,11},{0,4,6,14,18},{0,4,7,18},{0,4,11,18},{0,5,7,16},{0,5,11,16},{0,6,7,14},{0,6,11,14},{0,7,8,9},{0,7,8,12},{0,7,8,15},{0,7,9,10},{0,7,9,14},{0,7,10,12},{0,7,10,13},{0,7,12,17},{0,7,13,16},{0,7,14,18},{0,7,15,18},{0,7,16,17},{0,8,9,10,12},{0,8,9,11},{0,8,9,14,15,18},{0,8,11,12},{0,8,11,15},{0,9,10,11},{0,9,11,14},{0,10,11,12},{0,10,11,13},{0,10,12,13,16,17},{0,11,12,17},{0,11,13,16},{0,11,14,18},{0,11,15,18},{0,11,16,17},{1,2,3,4,5,6,7},{1,2,3,4,5,6,11},{1,2,3,4,5,6,19},{1,3,7,8,12,15,17},{1,3,7,19},{1,3,8,11,12,15,17},{1,3,8,12,15,17,19},{1,3,11,19},{1,4,7,15,18},{1,4,7,19},{1,4,11,15,18},{1,4,11,19},{1,4,15,18,19},{1,7,15,19},{1,11,15,19},{2,5,7,13,16},{2,5,7,19},{2,5,11,13,16},{2,5,11,19},{2,5,13,16,19},{2,6,7,9,10,13,14},{2,6,7,19},{2,6,9,10,11,13,14},{2,6,9,10,13,14,19},{2,6,11,19},{2,7,13,19},{2,11,13,19},{3,5,7,16,17},{3,5,7,19},{3,5,11,16,17},{3,5,11,19},{3,5,16,17,19},{3,7,17,19},{3,11,17,19},{4,6,7,14,18},{4,6,7,19},{4,6,11,14,18},{4,6,11,19},{4,6,14,18,19},{4,7,18,19},{4,11,18,19},{5,7,16,19},{5,11,16,19},{6,7,14,19},{6,11,14,19},{7,8,9,10,12},{7,8,9,14,15,18},{7,8,9,19},{7,8,12,19},{7,8,15,19},{7,9,10,19},{7,9,14,19},{7,10,12,13,16,17},{7,10,12,19},{7,10,13,19},{7,12,17,19},{7,13,16,19},{7,14,18,19},{7,15,18,19},{7,16,17,19},{8,9,10,11,12},{8,9,10,12,19},{8,9,11,14,15,18},{8,9,11,19},{8,9,14,15,18,19},{8,11,12,19},{8,11,15,19},{9,10,11,19},{9,11,14,19},{10,11,12,13,16,17},{10,11,12,19},{10,11,13,19},{10,12,13,16,17,19},{11,12,17,19},{11,13,16,19},{11,14,18,19},{11,15,18,19},{11,16,17,19}};
F121 = fan (raysF121 ,linealityF121 ,maxConesF121);
assert(dim F121 ==4)
assert(ambDim F121 ==5)
assert(#(maxCones F121) ==136)
assert(not isSmooth F121)
assert(isPure F121)
assert(not isSimplicial F121)
assert(not isComplete F121)
assert(fVector F121 == {1, 20, 92, 170, 136})
assert(not isPolytopal F121)

-- Test ambDim: 5, dim: 5, nrays: 20, n_max_cones: 40
-- Checking misc tests for fan
raysF122 = matrix {{-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},{0,0,0,0,0,0,0,-1,0,0,0,1,0,0,0,0,0,0,0,0},{0,-1,1,-1,-1,1,1,0,-1,1,1,0,-1,1,1,-1,1,-1,-1,0},{0,-2498297281740881198678291544546863/4292233922170347411817814193674192,-7012538792226086112924358907263123/8715740054670143779591681945787888,-853168194540718366519552314380549/1633604436597579867756918855037712,-2376368850200680272846378297742423/1188965054868984553175757399859904,3959858915640440808161367802075969/1756385359147711885281230275771760,-15370518860189209722653530257365101/4032359592672019241882820197740480,0,-33005235043899437497208274745288/105868656415338140456586840412833,723365086110191345493168434272/387998850010162462078593566713487,25362665923488389096629268456676/511676852640953683525248935581739,0,284734156325347081245652018033768/249884178116974368294619025288309,247742622372680406219772156972539/651910696131929186067030561586961,-11920975463394952023387172305872/5901745461923206181439973021473,-108239975558002546890967166653714/261036258360191737714509799678319,539426970431570236277770871109463/130987853236973852203880901389345,25170421131309517652087703934203/15926414258505777594363147697561,-20303616827570155889020353601744/9102424464896945257519978842191,0},{0,3878447178730676951910694486166429/8584467844340694823635628387348384,12509787701488839738937439262537009/17431480109340287559183363891575776,3124204339441645711919396014383823/6534417746390319471027675420150848,477892342042613973646209057148781/594482527434492276587878699929952,26099595939955105780012082333606671/7025541436590847541124921103087040,983540656839097004575785510609369/1008089898168004810470705049435120,0,-330103335103499526366998142504871/423474625661352561826347361651332,-838442066190720239385936283948709/387998850010162462078593566713487,-930713632552731931854669041770003/438580159406531728735927659070062,0,-2662382051315823227432821454096327/1999073424935794946356952202306472,-853819649472531556941987776395057/1303821392263858372134061123173922,-16616654466592771089357379907323/5901745461923206181439973021473,-177602914497116110289244987475037/261036258360191737714509799678319,843580423007891323234545346409517/523951412947895408815523605557380,-20356059935560866960509607801117/63705657034023110377452590790244,-24375837395791716729326666520827/9102424464896945257519978842191,0}};
linealityF122 = map(QQ^5, QQ^0, 0);
raysF122 = promote(raysF122, QQ);
linealityF122 = promote(linealityF122, QQ);
maxConesF122 = {{1,4,11,15,18,19},{8,9,11,14,15,18,19},{10,11,12,13,16,17,19},{4,6,11,14,18,19},{1,3,8,11,12,15,17,19},{8,9,10,11,12,19},{1,2,3,4,5,6,11,19},{2,6,9,10,11,13,14,19},{2,5,11,13,16,19},{3,5,11,16,17,19},{1,4,7,15,18,19},{7,8,9,14,15,18,19},{7,10,12,13,16,17,19},{4,6,7,14,18,19},{1,3,7,8,12,15,17,19},{7,8,9,10,12,19},{1,2,3,4,5,6,7,19},{2,6,7,9,10,13,14,19},{2,5,7,13,16,19},{3,5,7,16,17,19},{0,1,4,11,15,18},{0,8,9,11,14,15,18},{0,10,11,12,13,16,17},{0,4,6,11,14,18},{0,1,3,8,11,12,15,17},{0,8,9,10,11,12},{0,1,2,3,4,5,6,11},{0,2,6,9,10,11,13,14},{0,2,5,11,13,16},{0,3,5,11,16,17},{0,1,4,7,15,18},{0,7,8,9,14,15,18},{0,7,10,12,13,16,17},{0,4,6,7,14,18},{0,1,3,7,8,12,15,17},{0,7,8,9,10,12},{0,1,2,3,4,5,6,7},{0,2,6,7,9,10,13,14},{0,2,5,7,13,16},{0,3,5,7,16,17}};
F122 = fan (raysF122 ,linealityF122 ,maxConesF122);
assert(dim F122 ==5)
assert(ambDim F122 ==5)
assert(#(maxCones F122) ==40)
assert(not isSmooth F122)
assert(isPure F122)
assert(not isSimplicial F122)
assert(isComplete F122)
assert(fVector F122 == {1, 20, 92, 170, 136, 40})

-- Test ambDim: 3, dim: 3, nrays: 5, n_max_cones: 5
-- Checking misc tests for fan
raysF123 = matrix {{1,0,0,-1,-1},{0,1,0,-1,-2},{0,0,1,-1,-3}};
linealityF123 = map(QQ^3, QQ^0, 0);
raysF123 = promote(raysF123, QQ);
linealityF123 = promote(linealityF123, QQ);
maxConesF123 = {{4},{0,3},{1,3},{2,3},{0,1,2}};
F123 = fan (raysF123 ,linealityF123 ,maxConesF123);
assert(dim F123 ==3)
assert(ambDim F123 ==3)
assert(#(maxCones F123) ==5)
assert(isSmooth F123)
assert(not isPure F123)
assert(isSimplicial F123)
assert(not isComplete F123)
assert(fVector F123 == {1, 5, 6, 1})
assert(not isPolytopal F123)

-- Test ambDim: 2, dim: 1, nrays: 3, n_max_cones: 3
-- Checking misc tests for fan
raysF124 = matrix {{1,0,-1},{0,1,-1}};
linealityF124 = map(QQ^2, QQ^0, 0);
raysF124 = promote(raysF124, QQ);
linealityF124 = promote(linealityF124, QQ);
maxConesF124 = {{0},{1},{2}};
F124 = fan (raysF124 ,linealityF124 ,maxConesF124);
assert(dim F124 ==1)
assert(ambDim F124 ==2)
assert(#(maxCones F124) ==3)
assert(isSmooth F124)
assert(isPure F124)
assert(isSimplicial F124)
assert(not isComplete F124)
assert(fVector F124 == {1, 3})
assert(not isPolytopal F124)

-- Test ambDim: 2, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF125 = matrix {{1,0,-1},{0,1,-1}};
linealityF125 = map(QQ^2, QQ^0, 0);
raysF125 = promote(raysF125, QQ);
linealityF125 = promote(linealityF125, QQ);
maxConesF125 = {{0,1},{2}};
F125 = fan (raysF125 ,linealityF125 ,maxConesF125);
assert(dim F125 ==2)
assert(ambDim F125 ==2)
assert(#(maxCones F125) ==2)
assert(isSmooth F125)
assert(not isPure F125)
assert(isSimplicial F125)
assert(not isComplete F125)
assert(fVector F125 == {1, 3, 1})
assert(not isPolytopal F125)

-- Test ambDim: 2, dim: 1, nrays: 3, n_max_cones: 3
-- Checking misc tests for fan
raysF126 = matrix {{1,0,-1},{0,1,-1}};
linealityF126 = map(QQ^2, QQ^0, 0);
raysF126 = promote(raysF126, QQ);
linealityF126 = promote(linealityF126, QQ);
maxConesF126 = {{0},{1},{2}};
F126 = fan (raysF126 ,linealityF126 ,maxConesF126);
assert(dim F126 ==1)
assert(ambDim F126 ==2)
assert(#(maxCones F126) ==3)
assert(isSmooth F126)
assert(isPure F126)
assert(isSimplicial F126)
assert(not isComplete F126)
assert(fVector F126 == {1, 3})
assert(not isPolytopal F126)

-- Test ambDim: 3, dim: 2, nrays: 6, n_max_cones: 12
-- Checking misc tests for fan
raysF127 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF127 = map(QQ^3, QQ^0, 0);
raysF127 = promote(raysF127, QQ);
linealityF127 = promote(linealityF127, QQ);
maxConesF127 = {{0,2},{0,3},{0,4},{0,5},{1,2},{1,3},{1,4},{1,5},{2,4},{2,5},{3,4},{3,5}};
F127 = fan (raysF127 ,linealityF127 ,maxConesF127);
assert(dim F127 ==2)
assert(ambDim F127 ==3)
assert(#(maxCones F127) ==12)
assert(isSmooth F127)
assert(isPure F127)
assert(isSimplicial F127)
assert(not isComplete F127)
assert(fVector F127 == {1, 6, 12})
assert(not isPolytopal F127)

-- Test ambDim: 5, dim: 1, nrays: 20, n_max_cones: 20
-- Checking misc tests for fan
raysF128 = matrix {{-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},{0,0,0,0,0,0,0,-1,0,0,0,1,0,0,0,0,0,0,0,0},{0,-1,1,-1,-1,1,1,0,-1,1,1,0,-1,1,1,-1,1,-1,-1,0},{0,-2498297281740881198678291544546863/4292233922170347411817814193674192,-7012538792226086112924358907263123/8715740054670143779591681945787888,-853168194540718366519552314380549/1633604436597579867756918855037712,-2376368850200680272846378297742423/1188965054868984553175757399859904,3959858915640440808161367802075969/1756385359147711885281230275771760,-15370518860189209722653530257365101/4032359592672019241882820197740480,0,-33005235043899437497208274745288/105868656415338140456586840412833,723365086110191345493168434272/387998850010162462078593566713487,25362665923488389096629268456676/511676852640953683525248935581739,0,284734156325347081245652018033768/249884178116974368294619025288309,247742622372680406219772156972539/651910696131929186067030561586961,-11920975463394952023387172305872/5901745461923206181439973021473,-108239975558002546890967166653714/261036258360191737714509799678319,539426970431570236277770871109463/130987853236973852203880901389345,25170421131309517652087703934203/15926414258505777594363147697561,-20303616827570155889020353601744/9102424464896945257519978842191,0},{0,3878447178730676951910694486166429/8584467844340694823635628387348384,12509787701488839738937439262537009/17431480109340287559183363891575776,3124204339441645711919396014383823/6534417746390319471027675420150848,477892342042613973646209057148781/594482527434492276587878699929952,26099595939955105780012082333606671/7025541436590847541124921103087040,983540656839097004575785510609369/1008089898168004810470705049435120,0,-330103335103499526366998142504871/423474625661352561826347361651332,-838442066190720239385936283948709/387998850010162462078593566713487,-930713632552731931854669041770003/438580159406531728735927659070062,0,-2662382051315823227432821454096327/1999073424935794946356952202306472,-853819649472531556941987776395057/1303821392263858372134061123173922,-16616654466592771089357379907323/5901745461923206181439973021473,-177602914497116110289244987475037/261036258360191737714509799678319,843580423007891323234545346409517/523951412947895408815523605557380,-20356059935560866960509607801117/63705657034023110377452590790244,-24375837395791716729326666520827/9102424464896945257519978842191,0}};
linealityF128 = map(QQ^5, QQ^0, 0);
raysF128 = promote(raysF128, QQ);
linealityF128 = promote(linealityF128, QQ);
maxConesF128 = {{0},{1},{2},{3},{4},{5},{6},{7},{8},{9},{10},{11},{12},{13},{14},{15},{16},{17},{18},{19}};
F128 = fan (raysF128 ,linealityF128 ,maxConesF128);
assert(dim F128 ==1)
assert(ambDim F128 ==5)
assert(#(maxCones F128) ==20)
assert(isSmooth F128)
assert(isPure F128)
assert(isSimplicial F128)
assert(not isComplete F128)
assert(fVector F128 == {1, 20})
assert(not isPolytopal F128)

-- Test ambDim: 3, dim: 2, nrays: 6, n_max_cones: 12
-- Checking misc tests for fan
raysF129 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF129 = map(QQ^3, QQ^0, 0);
raysF129 = promote(raysF129, QQ);
linealityF129 = promote(linealityF129, QQ);
maxConesF129 = {{0,2},{0,3},{0,4},{0,5},{1,2},{1,3},{1,4},{1,5},{2,4},{2,5},{3,4},{3,5}};
F129 = fan (raysF129 ,linealityF129 ,maxConesF129);
assert(dim F129 ==2)
assert(ambDim F129 ==3)
assert(#(maxCones F129) ==12)
assert(isSmooth F129)
assert(isPure F129)
assert(isSimplicial F129)
assert(not isComplete F129)
assert(fVector F129 == {1, 6, 12})
assert(not isPolytopal F129)

///

TEST ///
-- Test ambDim: 3, dim: 1, nrays: 5, n_max_cones: 5
-- Checking misc tests for fan
raysF130 = matrix {{1,0,0,-1,-1},{0,1,0,-1,-2},{0,0,1,-1,-3}};
linealityF130 = map(QQ^3, QQ^0, 0);
raysF130 = promote(raysF130, QQ);
linealityF130 = promote(linealityF130, QQ);
maxConesF130 = {{0},{1},{2},{3},{4}};
F130 = fan (raysF130 ,linealityF130 ,maxConesF130);
assert(dim F130 ==1)
assert(ambDim F130 ==3)
assert(#(maxCones F130) ==5)
assert(isSmooth F130)
assert(isPure F130)
assert(isSimplicial F130)
assert(not isComplete F130)
assert(fVector F130 == {1, 5})
assert(not isPolytopal F130)

-- Test ambDim: 3, dim: 2, nrays: 5, n_max_cones: 7
-- Checking misc tests for fan
raysF131 = matrix {{1,0,0,-1,-1},{0,1,0,-1,-2},{0,0,1,-1,-3}};
linealityF131 = map(QQ^3, QQ^0, 0);
raysF131 = promote(raysF131, QQ);
linealityF131 = promote(linealityF131, QQ);
maxConesF131 = {{4},{0,1},{0,2},{0,3},{1,2},{1,3},{2,3}};
F131 = fan (raysF131 ,linealityF131 ,maxConesF131);
assert(dim F131 ==2)
assert(ambDim F131 ==3)
assert(#(maxCones F131) ==7)
assert(isSmooth F131)
assert(not isPure F131)
assert(isSimplicial F131)
assert(not isComplete F131)
assert(fVector F131 == {1, 5, 6})
assert(not isPolytopal F131)

-- Test ambDim: 2, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF132 = matrix {{1,0,-1},{0,1,-1}};
linealityF132 = map(QQ^2, QQ^0, 0);
raysF132 = promote(raysF132, QQ);
linealityF132 = promote(linealityF132, QQ);
maxConesF132 = {{2},{0,1}};
F132 = fan (raysF132 ,linealityF132 ,maxConesF132);
assert(dim F132 ==2)
assert(ambDim F132 ==2)
assert(#(maxCones F132) ==2)
assert(isSmooth F132)
assert(not isPure F132)
assert(isSimplicial F132)
assert(not isComplete F132)
assert(fVector F132 == {1, 3, 1})
assert(not isPolytopal F132)

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
raysF133 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF133 = map(QQ^3, QQ^0, 0);
raysF133 = promote(raysF133, QQ);
linealityF133 = promote(linealityF133, QQ);
maxConesF133 = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F133 = fan (raysF133 ,linealityF133 ,maxConesF133);
assert(dim F133 ==3)
assert(ambDim F133 ==3)
assert(#(maxCones F133) ==8)
assert(isSmooth F133)
assert(isPure F133)
assert(isSimplicial F133)
assert(isComplete F133)
assert(fVector F133 == {1, 6, 12, 8})
assert(isPolytopal F133)

-- Test ambDim: 2, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
raysF134 = matrix {{1,0,-1},{0,1,-1}};
linealityF134 = map(QQ^2, QQ^0, 0);
raysF134 = promote(raysF134, QQ);
linealityF134 = promote(linealityF134, QQ);
maxConesF134 = {{2},{0,1}};
F134 = fan (raysF134 ,linealityF134 ,maxConesF134);
assert(dim F134 ==2)
assert(ambDim F134 ==2)
assert(#(maxCones F134) ==2)
assert(isSmooth F134)
assert(not isPure F134)
assert(isSimplicial F134)
assert(not isComplete F134)
assert(fVector F134 == {1, 3, 1})
assert(not isPolytopal F134)

-- Test ambDim: 3, dim: 2, nrays: 5, n_max_cones: 7
-- Checking misc tests for fan
raysF135 = matrix {{1,0,0,-1,-1},{0,1,0,-1,-2},{0,0,1,-1,-3}};
linealityF135 = map(QQ^3, QQ^0, 0);
raysF135 = promote(raysF135, QQ);
linealityF135 = promote(linealityF135, QQ);
maxConesF135 = {{4},{0,1},{0,2},{0,3},{1,2},{1,3},{2,3}};
F135 = fan (raysF135 ,linealityF135 ,maxConesF135);
assert(dim F135 ==2)
assert(ambDim F135 ==3)
assert(#(maxCones F135) ==7)
assert(isSmooth F135)
assert(not isPure F135)
assert(isSimplicial F135)
assert(not isComplete F135)
assert(fVector F135 == {1, 5, 6})
assert(not isPolytopal F135)

-- Test ambDim: 3, dim: 3, nrays: 5, n_max_cones: 5
-- Checking misc tests for fan
raysF136 = matrix {{1,0,0,-1,-1},{0,1,0,-1,-2},{0,0,1,-1,-3}};
linealityF136 = map(QQ^3, QQ^0, 0);
raysF136 = promote(raysF136, QQ);
linealityF136 = promote(linealityF136, QQ);
maxConesF136 = {{4},{0,3},{1,3},{2,3},{0,1,2}};
F136 = fan (raysF136 ,linealityF136 ,maxConesF136);
assert(dim F136 ==3)
assert(ambDim F136 ==3)
assert(#(maxCones F136) ==5)
assert(isSmooth F136)
assert(not isPure F136)
assert(isSimplicial F136)
assert(not isComplete F136)
assert(fVector F136 == {1, 5, 6, 1})
assert(not isPolytopal F136)

-- Test ambDim: 3, dim: 1, nrays: 5, n_max_cones: 5
-- Checking misc tests for fan
raysF137 = matrix {{1,0,0,-1,-1},{0,1,0,-1,-2},{0,0,1,-1,-3}};
linealityF137 = map(QQ^3, QQ^0, 0);
raysF137 = promote(raysF137, QQ);
linealityF137 = promote(linealityF137, QQ);
maxConesF137 = {{0},{1},{2},{3},{4}};
F137 = fan (raysF137 ,linealityF137 ,maxConesF137);
assert(dim F137 ==1)
assert(ambDim F137 ==3)
assert(#(maxCones F137) ==5)
assert(isSmooth F137)
assert(isPure F137)
assert(isSimplicial F137)
assert(not isComplete F137)
assert(fVector F137 == {1, 5})
assert(not isPolytopal F137)

///
