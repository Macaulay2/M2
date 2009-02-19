-- This is test code for the new binomial GB code...

-- Basic code for binomial GB's

newBinomialGB = (R,wts,ishomog) -> (
     if ishomog then ishomog = 3 else ishomog = 0;
     sendgg(ggPush R, ggPush wts, ggPush 1, ggPush ishomog, ggbinomialGB);
     newHandle())

gensBinomialGB = (g, m) -> sendgg(ggPush g, ggPush m, ggbinomialGBaddgens)

extendBinomialGB = (g, newR, wts) -> 
     sendgg(ggPush g, ggPush newR, ggPush wts, ggbinomialGBenlarge)

calcBinomialGB = (g, deg) -> (
     sendgg(ggPush g, ggPush deg, ggPush {}, ggcalc);
     eePopInt())

getBinomialGB = (g,R) -> (
     sendgg(ggPush g, gggetgb);
     getMatrix R)
          
subringBinomialGB = (g,R) -> (
     sendgg(ggPush g, gggetsubring);
     getMatrix R)

-- First: making a new computation
binomialGB = (m,wts) -> (
     if not m.?binomialGB then (
         sendgg(ggPush ring m, ggPush wts, ggPush 1, ggPush 0, ggbinomialGB);
         m.binomialGB = newHandle();
         sendgg(ggPush m.binomialGB, ggPush m, ggbinomialGBaddgens);
	 );
     m.binomialGB
     )

calc = (m) -> (
     sendgg(ggPush m.binomialGB, ggPush {}, ggPush {}, ggcalc);
     m.binomialGBstatus = eePopInt())

addgens = (m) -> (
     if not m.?binomialGB then 
         m.binomialGB = binomialGB(m,ring m);
     sendgg(ggPush m.binomialGB, ggPush m, ggbinomialGBaddgens))

extendring = (m,newR,wts) -> (
     newm = substitute(m,newR);
     newm.binomialGB = m.binomialGB;
     remove(m, symbol binomialGB);
     sendgg(ggPush m.binomialGB, ggPush newR, ggPush wts))

getbinomialgb = (m) -> (
     sendgg(ggPush m.binomialGB, gggetgb);
     getMatrix ring m)

sortColumns := (m) -> (
     sendgg(ggPush m, ggPush 1, ggPush 1, ggsortcolumns);
     m _ (eePopIntarray()))

--Some examples:
bernd = (n) -> (
     -- ideal is x_i - t^i * y_i, 1 <= i <= n
     -- order is: eliminate t.
     t = symbol t;
     x = symbol x;
     y = symbol y;
     R = ZZ/101[t, x_1 .. x_n, y_1 .. y_n, Degrees=>{1,2..n+1,n:1},MonomialOrder=>Eliminate 1,
	  MonomialSize=>16];
     m = map(R^1, n, (i,j) -> x_(j+1) - t^(j+1) * y_(j+1));
     m)

--integer matrix to binomials
intmat2matrix = (m,R) -> (
     map(R^1, numgens source m, (r,j) -> (
	  a := 1_R;
	  b := 1_R;
	  i := 0;
	  n := numgens target m;
	  while i < n do (
	       val := m_(i,j);
	       if val > 0 then a = a * (R_i)^val
	       else if val < 0 then b = b * (R_i)^(-val);
	       i = i+1);
	  a-b)))

matrix2intmat = (m) -> (
     R := ring m;
     n := numgens R;
     ones := map(ZZ,R,elements(n:1));
     ones jacobian m)

///
-- Test of intmat2matrix:
m1 = transpose map(ZZ^4, ZZ^2, {{0,1},{1,1},{3,1},{4,1}})
m = syz m1
R = ZZ/101[a..d]
time intmat2matrix (m,R)
///


methodA = (n) -> (
     m = bernd n;
     time gb m;
     sortColumns gens gb m
     )

methodB = (n) -> (
     m = bernd n;
     sendgg(ggPush R, ggPush splice {1,(2*n):0}, ggPush 1, ggPush 0, ggbinomialGB);
     m.binomialGB = newHandle();
     sendgg(ggPush m.binomialGB, ggPush m, ggbinomialGBaddgens);
     time calc m;
     sortColumns getbinomialgb m)

methodC = (n) -> (
     m = bernd n;
     sendgg(ggPush R, ggPush splice {1,(2*n):0}, ggPush 1, ggPush 3, ggbinomialGB);
     m.binomialGB = newHandle();
     sendgg(ggPush m.binomialGB, ggPush m, ggbinomialGBaddgens);
     time calc m;
     sortColumns getbinomialgb m)

///
load "binomials.m2"
R = ZZ/101[a..d]
m = matrix{{a*d-b*c, a^2-d^2}}
m = matrix{{a*d-b*c, b^2-d^2}}
binomialGB(m,ring m)
calc m
getbinomialgb m
sendgg(ggPush m.binomialGB, ggstats)
time gens gb m


gbTrace = 5
load "binomials.m2"
R = ZZ/101[s,t,a..d,Degrees=>{1,1,4,4,4,4},MonomialOrder=>Eliminate 2]
m = matrix{{a-s^4, b-s^3*t, c-s*t^3, d-t^4}}
sendgg(ggPush R, ggPush {1,1,0,0,0,0}, ggPush 1, ggPush 4, ggbinomialGB)
m.binomialGB = newHandle()
sendgg(ggPush m.binomialGB, ggPush m, ggbinomialGBaddgens)
time calc m
sendgg(ggPush m.binomialGB, ggstats)
sendgg(ggPush m.binomialGB, gggetsubring); getMatrix R



load "binomials.m2"
R = ZZ/101[s,t,a..d,MonomialOrder=>Eliminate 2]
m = matrix{{a-s^4, b-s^3*t, c-s*t^3, d-t^4}}
sendgg(ggPush R, ggPush {1,1,0,0,0,0}, ggPush 1, ggPush 4, ggbinomialGB)
m.binomialGB = newHandle()
sendgg(ggPush m.binomialGB, ggPush m, ggbinomialGBaddgens)
calc m
sendgg(ggPush m.binomialGB, gggetsubring); getMatrix R

R = ZZ/101[s,t,a..d,MonomialOrder=>Eliminate 2]
m = matrix{{a-s^4, b-s^3*t, c-s*t^3, d-t^4}}
binomialGB(m,R)
calc m
getbinomialgb m

R = ZZ/101[a..d]
m = matrix{{a*b-a, a*c-1, b*c-d}}
binomialGB(m,R)
calc m
getbinomialgb m

restart
load "binomials.m2"
methodA 6     -- 1.93 seconds   [201 elements in GB]
methodB 6     --  .45 seconds
methodC 6     --  .34 seconds

methodA 7     -- 12.86 seconds  [499 elements in GB]
methodB 7     --  4.25 seconds
methodC 7     --  2.9  seconds

methodA 8     -- 59.35 seconds  [1032 elements in GB]
methodB 8     -- 21.71 seconds
methodC 8     -- 15.12 seconds

methodA 9     -- 301.82 seconds [using MonomialSize=>16]
methodB 9     -- 158.68 seconds [2193 elements in GB]
methodC 9     -- 114.4  seconds
///

///
m = bernd 5
bg = newBinomialGB(ring m,{1,0,0,0,0,0,0,0,0,0,0},true)
gensBinomialGB(bg, m)
calcBinomialGB(bg, {6})
getBinomialGB(bg, R)
subringBinomialGB(bg, R)  -- only get these once!

N = 6
setup = (N) -> (
     m = bernd N;
     bg = newBinomialGB(ring m, elements splice(1,2*N:0), true);
     gensBinomialGB(bg, m);
     bg)

nextstep = (bg, N) -> (
    m1 = bernd N;
    newgens = m1_{N-1};
    extendBinomialGB(bg, ring m1, elements splice(1,2*N:0));
    gensBinomialGB(bg, newgens);
    calcBinomialGB(bg, {N+1});
    subringBinomialGB(bg, ring m1))

load "binomials.tst"
bg = setup 5
nextstep(bg,5)  -- core dumps at the moment...
///

///
load "/home/mike/src/M2/Macaulay2/mike/binomials.m2"
-- A test for adding generators/extending the ring
kk = ZZ/101
rings = {"kk[x_0,x_1,x_2,x_3,MonomialOrder => ProductOrder{3, 1}]","kk[x_0,x_1,x_2,x_3,x_4,Degrees => {{1}, {1}, {1}, {1}, {2}},MonomialOrder => ProductOrder{3, 2}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}},MonomialOrder => ProductOrder{3, 4}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}},MonomialOrder => ProductOrder{3, 5}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}},MonomialOrder => ProductOrder{3, 6}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}},MonomialOrder => ProductOrder{3, 7}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}},MonomialOrder => ProductOrder{3, 8}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}},MonomialOrder => ProductOrder{3, 9}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}},MonomialOrder => ProductOrder{3, 10}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}},MonomialOrder => ProductOrder{3, 11}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}},MonomialOrder => ProductOrder{3, 12}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}, {12}},MonomialOrder => ProductOrder{3, 13}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,x_16,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}, {12}, {13}},MonomialOrder => ProductOrder{3, 14}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,x_16,x_17,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}, {12}, {13}, {14}},MonomialOrder => ProductOrder{3, 15}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,x_16,x_17,x_18,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}, {12}, {13}, {14}, {15}},MonomialOrder => ProductOrder{3, 16}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,x_16,x_17,x_18,x_19,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}, {12}, {13}, {14}, {15}, {16}},MonomialOrder => ProductOrder{3, 17}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,x_16,x_17,x_18,x_19,x_20,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}, {12}, {13}, {14}, {15}, {16}, {17}},MonomialOrder => ProductOrder{3, 18}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,x_16,x_17,x_18,x_19,x_20,x_21,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}, {12}, {13}, {14}, {15}, {16}, {17}, {18}},MonomialOrder => ProductOrder{3, 19}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,x_16,x_17,x_18,x_19,x_20,x_21,x_22,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}, {12}, {13}, {14}, {15}, {16}, {17}, {18}, {19}},MonomialOrder => ProductOrder{3, 20}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,x_16,x_17,x_18,x_19,x_20,x_21,x_22,x_23,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}, {12}, {13}, {14}, {15}, {16}, {17}, {18}, {19}, {20}},MonomialOrder => ProductOrder{3, 21}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,x_16,x_17,x_18,x_19,x_20,x_21,x_22,x_23,x_24,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}, {12}, {13}, {14}, {15}, {16}, {17}, {18}, {19}, {20}, {21}},MonomialOrder => ProductOrder{3, 22}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,x_16,x_17,x_18,x_19,x_20,x_21,x_22,x_23,x_24,x_25,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}, {12}, {13}, {14}, {15}, {16}, {17}, {18}, {19}, {20}, {21}, {22}},MonomialOrder => ProductOrder{3, 23}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,x_16,x_17,x_18,x_19,x_20,x_21,x_22,x_23,x_24,x_25,x_26,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}, {12}, {13}, {14}, {15}, {16}, {17}, {18}, {19}, {20}, {21}, {22}, {23}},MonomialOrder => ProductOrder{3, 24}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,x_16,x_17,x_18,x_19,x_20,x_21,x_22,x_23,x_24,x_25,x_26,x_27,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}, {12}, {13}, {14}, {15}, {16}, {17}, {18}, {19}, {20}, {21}, {22}, {23}, {24}},MonomialOrder => ProductOrder{3, 25}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,x_16,x_17,x_18,x_19,x_20,x_21,x_22,x_23,x_24,x_25,x_26,x_27,x_28,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}, {12}, {13}, {14}, {15}, {16}, {17}, {18}, {19}, {20}, {21}, {22}, {23}, {24}, {25}},MonomialOrder => ProductOrder{3, 26}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,x_16,x_17,x_18,x_19,x_20,x_21,x_22,x_23,x_24,x_25,x_26,x_27,x_28,x_29,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}, {12}, {13}, {14}, {15}, {16}, {17}, {18}, {19}, {20}, {21}, {22}, {23}, {24}, {25}, {26}},MonomialOrder => ProductOrder{3, 27}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,x_16,x_17,x_18,x_19,x_20,x_21,x_22,x_23,x_24,x_25,x_26,x_27,x_28,x_29,x_30,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}, {12}, {13}, {14}, {15}, {16}, {17}, {18}, {19}, {20}, {21}, {22}, {23}, {24}, {25}, {26}, {27}},MonomialOrder => ProductOrder{3, 28}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,x_16,x_17,x_18,x_19,x_20,x_21,x_22,x_23,x_24,x_25,x_26,x_27,x_28,x_29,x_30,x_31,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}, {12}, {13}, {14}, {15}, {16}, {17}, {18}, {19}, {20}, {21}, {22}, {23}, {24}, {25}, {26}, {27}, {28}},MonomialOrder => ProductOrder{3, 29}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,x_16,x_17,x_18,x_19,x_20,x_21,x_22,x_23,x_24,x_25,x_26,x_27,x_28,x_29,x_30,x_31,x_32,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}, {12}, {13}, {14}, {15}, {16}, {17}, {18}, {19}, {20}, {21}, {22}, {23}, {24}, {25}, {26}, {27}, {28}, {29}},MonomialOrder => ProductOrder{3, 30}]","kk[x_0,x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,x_16,x_17,x_18,x_19,x_20,x_21,x_22,x_23,x_24,x_25,x_26,x_27,x_28,x_29,x_30,x_31,x_32,x_33,Degrees => {{1}, {1}, {1}, {1}, {2}, {3}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}, {11}, {12}, {13}, {14}, {15}, {16}, {17}, {18}, {19}, {20}, {21}, {22}, {23}, {24}, {25}, {26}, {27}, {28}, {29}, {30}},MonomialOrder => ProductOrder{3, 31}]"}
mats  = {"matrix{{-x_0+x_3}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14,-x_0*x_1^11+x_15}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14,-x_0*x_1^11+x_15,-x_0*x_1^12+x_16}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14,-x_0*x_1^11+x_15,-x_0*x_1^12+x_16,-x_0*x_1^13+x_17}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14,-x_0*x_1^11+x_15,-x_0*x_1^12+x_16,-x_0*x_1^13+x_17,-x_0*x_1^14+x_18}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14,-x_0*x_1^11+x_15,-x_0*x_1^12+x_16,-x_0*x_1^13+x_17,-x_0*x_1^14+x_18,-x_0*x_1^15+x_19}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14,-x_0*x_1^11+x_15,-x_0*x_1^12+x_16,-x_0*x_1^13+x_17,-x_0*x_1^14+x_18,-x_0*x_1^15+x_19,-x_0*x_1^16+x_20}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14,-x_0*x_1^11+x_15,-x_0*x_1^12+x_16,-x_0*x_1^13+x_17,-x_0*x_1^14+x_18,-x_0*x_1^15+x_19,-x_0*x_1^16+x_20,-x_0*x_1^17+x_21}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14,-x_0*x_1^11+x_15,-x_0*x_1^12+x_16,-x_0*x_1^13+x_17,-x_0*x_1^14+x_18,-x_0*x_1^15+x_19,-x_0*x_1^16+x_20,-x_0*x_1^17+x_21,-x_0*x_1^18+x_22}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14,-x_0*x_1^11+x_15,-x_0*x_1^12+x_16,-x_0*x_1^13+x_17,-x_0*x_1^14+x_18,-x_0*x_1^15+x_19,-x_0*x_1^16+x_20,-x_0*x_1^17+x_21,-x_0*x_1^18+x_22,-x_0*x_1^19+x_23}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14,-x_0*x_1^11+x_15,-x_0*x_1^12+x_16,-x_0*x_1^13+x_17,-x_0*x_1^14+x_18,-x_0*x_1^15+x_19,-x_0*x_1^16+x_20,-x_0*x_1^17+x_21,-x_0*x_1^18+x_22,-x_0*x_1^19+x_23,-x_0*x_1^20+x_24}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14,-x_0*x_1^11+x_15,-x_0*x_1^12+x_16,-x_0*x_1^13+x_17,-x_0*x_1^14+x_18,-x_0*x_1^15+x_19,-x_0*x_1^16+x_20,-x_0*x_1^17+x_21,-x_0*x_1^18+x_22,-x_0*x_1^19+x_23,-x_0*x_1^20+x_24,-x_0*x_1^21+x_25}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14,-x_0*x_1^11+x_15,-x_0*x_1^12+x_16,-x_0*x_1^13+x_17,-x_0*x_1^14+x_18,-x_0*x_1^15+x_19,-x_0*x_1^16+x_20,-x_0*x_1^17+x_21,-x_0*x_1^18+x_22,-x_0*x_1^19+x_23,-x_0*x_1^20+x_24,-x_0*x_1^21+x_25,-x_0*x_1^22+x_26}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14,-x_0*x_1^11+x_15,-x_0*x_1^12+x_16,-x_0*x_1^13+x_17,-x_0*x_1^14+x_18,-x_0*x_1^15+x_19,-x_0*x_1^16+x_20,-x_0*x_1^17+x_21,-x_0*x_1^18+x_22,-x_0*x_1^19+x_23,-x_0*x_1^20+x_24,-x_0*x_1^21+x_25,-x_0*x_1^22+x_26,-x_0*x_1^23+x_27}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14,-x_0*x_1^11+x_15,-x_0*x_1^12+x_16,-x_0*x_1^13+x_17,-x_0*x_1^14+x_18,-x_0*x_1^15+x_19,-x_0*x_1^16+x_20,-x_0*x_1^17+x_21,-x_0*x_1^18+x_22,-x_0*x_1^19+x_23,-x_0*x_1^20+x_24,-x_0*x_1^21+x_25,-x_0*x_1^22+x_26,-x_0*x_1^23+x_27,-x_0*x_1^24+x_28}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14,-x_0*x_1^11+x_15,-x_0*x_1^12+x_16,-x_0*x_1^13+x_17,-x_0*x_1^14+x_18,-x_0*x_1^15+x_19,-x_0*x_1^16+x_20,-x_0*x_1^17+x_21,-x_0*x_1^18+x_22,-x_0*x_1^19+x_23,-x_0*x_1^20+x_24,-x_0*x_1^21+x_25,-x_0*x_1^22+x_26,-x_0*x_1^23+x_27,-x_0*x_1^24+x_28,-x_0*x_1^25+x_29}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14,-x_0*x_1^11+x_15,-x_0*x_1^12+x_16,-x_0*x_1^13+x_17,-x_0*x_1^14+x_18,-x_0*x_1^15+x_19,-x_0*x_1^16+x_20,-x_0*x_1^17+x_21,-x_0*x_1^18+x_22,-x_0*x_1^19+x_23,-x_0*x_1^20+x_24,-x_0*x_1^21+x_25,-x_0*x_1^22+x_26,-x_0*x_1^23+x_27,-x_0*x_1^24+x_28,-x_0*x_1^25+x_29,-x_0*x_1^26+x_30}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14,-x_0*x_1^11+x_15,-x_0*x_1^12+x_16,-x_0*x_1^13+x_17,-x_0*x_1^14+x_18,-x_0*x_1^15+x_19,-x_0*x_1^16+x_20,-x_0*x_1^17+x_21,-x_0*x_1^18+x_22,-x_0*x_1^19+x_23,-x_0*x_1^20+x_24,-x_0*x_1^21+x_25,-x_0*x_1^22+x_26,-x_0*x_1^23+x_27,-x_0*x_1^24+x_28,-x_0*x_1^25+x_29,-x_0*x_1^26+x_30,-x_0*x_1^27+x_31}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14,-x_0*x_1^11+x_15,-x_0*x_1^12+x_16,-x_0*x_1^13+x_17,-x_0*x_1^14+x_18,-x_0*x_1^15+x_19,-x_0*x_1^16+x_20,-x_0*x_1^17+x_21,-x_0*x_1^18+x_22,-x_0*x_1^19+x_23,-x_0*x_1^20+x_24,-x_0*x_1^21+x_25,-x_0*x_1^22+x_26,-x_0*x_1^23+x_27,-x_0*x_1^24+x_28,-x_0*x_1^25+x_29,-x_0*x_1^26+x_30,-x_0*x_1^27+x_31,-x_0*x_1^28+x_32}}", "matrix{{-x_0+x_3,-x_0*x_1+x_4,-x_0*x_1*x_2+x_5,-x_0*x_1^2+x_6,-x_0*x_1^3+x_7,-x_0*x_1^4+x_8,-x_0*x_1^5+x_9,-x_0*x_1^6+x_10,-x_0*x_1^7+x_11,-x_0*x_1^8+x_12,-x_0*x_1^9+x_13,-x_0*x_1^10+x_14,-x_0*x_1^11+x_15,-x_0*x_1^12+x_16,-x_0*x_1^13+x_17,-x_0*x_1^14+x_18,-x_0*x_1^15+x_19,-x_0*x_1^16+x_20,-x_0*x_1^17+x_21,-x_0*x_1^18+x_22,-x_0*x_1^19+x_23,-x_0*x_1^20+x_24,-x_0*x_1^21+x_25,-x_0*x_1^22+x_26,-x_0*x_1^23+x_27,-x_0*x_1^24+x_28,-x_0*x_1^25+x_29,-x_0*x_1^26+x_30,-x_0*x_1^27+x_31,-x_0*x_1^28+x_32,-x_0*x_1^29+x_33}}"}

dobin = (m, wts, deg) -> (
     bg := newBinomialGB(ring m, wts, true);
     gensBinomialGB(bg, m);
     calcBinomialGB(bg, deg);
     getBinomialGB(bg, ring m))

dobini = (m, wts, deg) -> (
     bg := newBinomialGB(ring m, wts, false);
     gensBinomialGB(bg, m);
     calcBinomialGB(bg, deg);
     getBinomialGB(bg, ring m))

startit = () -> (
     R = evaluate rings_0;
     m = evaluate mats_0;
     r = numgens source m;
     bg = newBinomialGB(ring m, elements splice(1,1,1,0), true);
     gensBinomialGB(bg, m);
     calcBinomialGB(bg, {1});
     s = subringBinomialGB(bg, R))


doit = (i) -> (
     oldr = numgens source m;
     R = evaluate rings_i;
     m = evaluate mats_i;
     extendBinomialGB(bg, R, elements splice(1,1,1,(numgens R - 3):0));
     --sendgg(ggPush bg, ggstats);
     r = numgens source m;
     gensBinomialGB(bg, m_{oldr..r-1});
     calcBinomialGB(bg, {i+1});
     -- g = getBinomialGB(bg,R);
     s = subringBinomialGB(bg, R))

startit()
newR = evaluate rings_1
extendBinomialGB(bg, newR, {1,1,1,0,0})
calcBinomialGB(bg,{2})
getBinomialGB(bg,newR)

startit()
doit 1 
doit 2
doit 3 -- core dumps

R = evaluate rings_29;
m = evaluate mats_29;
time dobin(m, elements splice(1,1,1,(numgens R - 3):0), {})
///
