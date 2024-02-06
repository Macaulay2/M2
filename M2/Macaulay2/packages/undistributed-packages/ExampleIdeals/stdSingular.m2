--------------------------------------------------------
--alex1, Singular standard bases#1.
R = kk{t,x,y,z}
ideal"5t3x2z+2t2y3x5,7y+4x2y+y2x+2zt,3tz+3yz2+2yz4"
-* -- Singular code
  ring R=32003,(t,x,y,z),ds;
  ideal i = 5t3x2z+2t2x5y3,7y+2tz+4x2y+xy2,3tz+3yz2+2yz4;
  timer=1;
  option(prot);
  std(i);
*-
--------------------------------------------------------
--std2
R = kk{t,x,y,z,u,v, MonomialSize=>8}
ideal"xy+xz+2xu+yu+zu+2u2+yv+zv+2uv-3txz-y2z-yz2-3tzu-2yzu-3tzv+3tyz2,
      y2+z2+2u2-3u2v,
      tx+z2+2xv-txy,
      -yz-z2-xu-u2+tv-v2,
      -1569xy3-753t2zu,
      -587xz+15625yv"
-* -- Singular code
  ring R=32003,(t,x,y,z,u,v),ds;
  ideal i = xy+xz+2xu+yu+zu+2u2+yv+zv+2uv-3txz-y2z-yz2-3tzu-2yzu-3tzv+3tyz2,
             y2+z2+2u2-3u2v,
             tx+z2+2xv-txy,
             -yz-z2-xu-u2+tv-v2,
            -1569xy3-753t2zu,
            -587xz+15625yv;
  timer=1;
  option(prot);
  std(i);
  mres(i,0);
*-
--------------------------------------------------------
--std3
R = kk{x,y,z}
F = singF(13,12,3,1)
ideal(diff(x,F),diff(y,F),diff(z,F))
-*
  ring R=32003,(x,y,z),ds;
  ideal i = 3x2y3+5x4y2+4xy5+2xy2z3+y7+13x12,
           3x3y2+2x5y+10x2y4+2x2yz3+7xy6+12y11,
	   3x2y2z2+9z8;
  timer=1;
  std(i); // much better than M2
*-
--------------------------------------------------------
--std4
R = kk{t,x,y,z,u, MonomialSize=>8}
ideal"2yz+2xu+t2x,
    2tx+z2+2yu+x2y,
    x2+2ty+yz+2zu,
    2xy+2tz+u2+z3,
    y2+2xz+2tu+u5"
-* -- Singular code
  ring R=32003,(t,x,y,z,u),ds;
  ideal i = 2yz+2xu+t2x,
            2tx+z2+2yu+x2y,
            x2+2ty+yz+2zu,
            2xy+2tz+u2+z3,
           y2+2xz+2tu+u5;
  timer=1;
  option(prot);
  std(i);
*-
--------------------------------------------------------
--std5
R = kk{x,y,z}
F = singH 8
ideal(diff(x,F),diff(y,F),diff(z,F))
-*
  ring R=32003,(x,y,z),ds;
  ideal i = 3x2+6xy+3y2+6xz+6yz+3z2+3x2yz+4xy2z+y3z+4xyz2+2y2z2+yz3+8x7,
              3x2+6xy+3y2+6xz+6yz+3z2+x3z+4x2yz+3xy2z+2x2z2+4xyz2+xz3+8y7,
	      3x2+6xy+3y2+6xz+6yz+3z2+x3y+2x2y2+xy3+4x2yz+4xy2z+3xyz2+8z7;
  timer=1;
  std(i); // much better than M2
*-
--------------------------------------------------------
--std6
R = kk{x,y,z, MonomialSize=>8}
F = singG(6,8,10,5,5,0)
ideal(F,diff(x,F),diff(y,F),diff(z,F))
-*
  ring R=32003,(x,y,z),ds;
  ideal i = x5+x3y2+x2yz2+x6+xy5+y8+z10,
     5x4+3x2y2+2xyz2+6x5+y5,
     2x3y+x2z2+5xy4+8y7,
     2x2yz+10z9;
  timer=1;
  std(i); // much better than M2
*-
--------------------------------------------------------
--std7
R = kk{t,x,y,z, MonomialSize=>8}
J = ideal"17y6+49y5-9y4+12y3+33y2+11y+73x2z,
  21x2y+17x2z+63y3+11xz+77y2+91y+xz2,
  26y2+44xz+12y+9zxy"
J^2 + x^2*z^2*J
-* -- Singular code
  ring R=32003,(t,x,y,z),ds;
  ideal i = 121y2+726y3+1353y4+1606x2yz+594y5+4818x2y2z+628y6+1752x2y3z+5329x4z2+3392y7-1314x2y4z+2379y8+7154x2y5z-474y9+2482x2y6z+2095y10+1666y11+289y12,
     1001y2+3850y3+121xyz+231x2y2+4326y4+6830x2yz+363xy2z+11xyz2+693x2y3+2184y5+6182x2y2z+132xy3z+803x3z2+33xy2z2+252x2y4+4522y6+1533x4yz+4803x2y3z-99xy4z+1241x4z2+12xy3z2+73x3z3-189x2y5+4753y7-153x2y4z+539xy5z-9xy4z2+1029x2y6+4396y8+833x2y5z+187xy6z+49xy5z2+357x2y7+1071y9+289x2y6z+17xy6z2,
     132y2+682y3+484xyz+1002y4+876x2yz+1551xy2z+204y5+1898x2y2z+825xy3z+3212x3z2+354y6-288xy4z+657x3yz2+1478y7+2075xy5z+442y8+1189xy6z+153xy7z,
     8281y2+14014y3+2002xyz+3822x2y2-14608y4+3094x2yz+1694xy2z+121x2z2+182xyz2+3234x2y3+9702y5+462x3yz+2618x2y2z+1386xy3z+374x3z2+154xy2z2+22x2z3+441x4y2+2646x2y4+3969y6+714x4yz+2142x2y3z+289x4z2+42x3yz2+126xy3z2+34x3z3+x2z4,
     1092y2+3290y3+4136xyz+252x2y2+2758y4+204x2yz+4493xy2z+484x2z2+12xyz2+546x2y3+1638y5+924x3yz+442x2y2z+3465xy3z+748x3z2+99x2yz2+26xy2z2+44x2z3+189x3y2z+567xy4z+153x3yz2+9x2yz3,
     144y2+624y3+1056xyz+676y4+2504xy2z+1936x2z2+468xy3z+792x2yz2+81x2y2z2,
     11x2yz2+33x2y2z2+12x2y3z2+73x4z3-9x2y4z2+49x2y5z2+17x2y6z2,
     91x2yz2+77x2y2z2+11x3z3+21x4yz2+63x2y3z2+17x4z3+x3z4,
     12x2yz2+26x2y2z2+44x3z3+9x3yz3;
  timer=1;
  option(prot);
  std(i);
*-
--------------------------------------------------------
--std8
R = kk{t,x,y,z,u,v,w,a,b,c,d, MonomialSize=>8}
ideal"bcd-u5,
  y2b2c2-2yzu4bc+z2u8,
  t2a2b2-2tyzu3ab+y2z2u6,
  txc-y3zu,
  zwa-y4u,
  xuw-y5"
-* -- Singular code
  ring R=32003,(t,x,y,z,u,v,w,a,b,c,d),ds;
  ideal i = bcd-u5,
     y2b2c2-2yzu4bc+z2u8,
     t2a2b2-2tyzu3ab+y2z2u6,
     txc-y3zu,
     zwa-y4u,
     xuw-y5;
  timer=1;
  option(prot);
  std(i);
*-
--------------------------------------------------------
--std9
R = kk{x,y,z}
F = singF(13,12,3,1)
ideal(F,diff(x,F),diff(y,F),diff(z,F))
-* -- Singular code
  ring R=32003,(x,y,z),ds;
  ideal i = x3y3+x5y2+2x2y5+x2y2z3+xy7+z9+y12+x13,
     3x2y3+5x4y2+4xy5+2xy2z3+y7+13x12,
     3x3y2+2x5y+10x2y4+2x2yz3+7xy6+12y11,
     3x2y2z2+9z8;
  timer=1;
  option(prot);
  std(i);
*-
--------------------------------------------------------
--std10
R = kk{t,x,y,z}
(ideal"-x28yz+t31-t30x,
     -xy25z+t28+t27y,
     -xyz24+t27+t26z")^2
-* -- Singular code
  ring R=32003,(t,x,y,z),ds;
  ideal i = x56y2z2-2t31x28yz+2t30x29yz+t62-2t61x+t60x2,
     x29y26z2-t28x28yz-t27x28y2z-t31xy25z+t30x2y25z+t59-t58x+t58y-t57xy,
     x29y2z25-t27x28yz-t26x28yz2-t31xyz24+t30x2yz24+t58-t57x+t57z-t56xz,
     x2y50z2-2t28xy25z-2t27xy26z+t56+2t55y+t54y2,
     x2y26z25-t27xy25z-t26xy25z2-t28xyz24-t27xy2z24+t55+t54y+t54z+t53yz,
     x2y2z48-2t27xyz24-2t26xyz25+t54+2t53z+t52z2;
  timer=1;
  option(prot);
  std(i);
*-
--------------------------------------------------------
--std11
R = kk{x,y,z}
(ideal"x2+y2+z2-2yz2+y4+xy2z,
     -2yz2+y4+xy2z+x3yz-y2z3,
     y2z+x3y-3y3z-2xyz2"
     )^2
-* -- Singular code
  ring R=32003,(x,y,z),ds;
  ideal i = x4+2x2y2+y4+2x2z2+2y2z2+z4-4x2yz2-4y3z2-4yz4+2x2y4+2y6+2x3y2z+2xy4z+2y4z2+2xy2z3+4y2z4-4y5z2-4xy3z3+y8+2xy6z+x2y4z2,
     -2x2yz2-2y3z2-2yz4+x2y4+y6+x3y2z+xy4z+y4z2+xy2z3+4y2z4+x5yz+x3y3z-4y5z2+x3yz3-x2y2z3-4xy3z3-y4z3-y2z5+y8+2xy6z+x2y4z2-2x3y2z3+2y3z5+x3y5z+x4y3z2-y6z3-xy4z4,
     x2y2z+y4z+y2z3+x5y+x3y3-3x2y3z-3y5z-x3yz2-2xy3z2-5y3z3-2xyz4+y6z-2x3y2z2+xy4z2+6y4z3+4xy2z4+x3y5+x4y3z-3y7z-5xy5z2-2x2y3z3,
     4y2z4-4y5z2-4xy3z3+y8+2xy6z+x2y4z2-4x3y2z3+4y3z5+2x3y5z+2x4y3z2-2y6z3-2xy4z4+x6y2z2-2x3y3z4+y4z6,
     -2y3z3+y6z-2x3y2z2+xy4z2+6y4z3+4xy2z4+x3y5+x4y3z-3y7z+x3y3z2-5xy5z2-2x2y3z3-y4z4+x6y2z-3x3y4z2-2x4y2z3-x3y3z3+3y5z4+2xy3z5,
     y4z2+2x3y3z-6y5z2-4xy3z3+x6y2-6x3y4z-4x4y2z2+9y6z2+12xy4z3+4x2y2z4;
  timer=1;
  option(prot);
  std(i);
*-
--------------------------------------------------------
--std12
R = kk{x,y,z,w}
ideal"x2-z10-z20,xy3-z10-z30,y6-xy3w40"
-* -- Singular code
  ring R=32003,(x,y,z,w),ds;
  ideal i = x2-z10-z20,
     xy3-z10-z30,
     y6-xy3w40;
  timer=1;
  option(prot);
  std(i);
*-
--------------------------------------------------------
--std13
R = kk{t,x,y,z,u,v}
ideal"txz+xyz+xy2z+xyz2,
  t2z+xz2+yz2+x2yz+xy2z+xyz2,
  t2x+t2z+xz2+x2yz+xy2z+xyz2,
  txv+xuv+xu2v+xuv2,
  t2v+xv2+uv2+x2uv+xu2v+xuv2,
  t2x+t2v+xv2+x2uv+xu2v+xuv2"
-* -- Singular code
  ring R=32003,(t,x,y,z,u,v),ds;
  ideal i = txz+xyz+xy2z+xyz2,
     t2z+xz2+yz2+x2yz+xy2z+xyz2,
     t2x+t2z+xz2+x2yz+xy2z+xyz2,
     txv+xuv+xu2v+xuv2,
     t2v+xv2+uv2+x2uv+xu2v+xuv2,
     t2x+t2v+xv2+x2uv+xu2v+xuv2;
  timer=1;
  option(prot);
  std(i);
*-
--------------------------------------------------------
--std14
R = kk{t,x,y,z, MonomialSize=>8}
R = kk[t,x,y,z,MonomialOrder=>{Weights=>{-32,-32,-24,-117}},Global=>false]
	   -- Does this order help out??
J = ideal"-3x2-t3+x3-y4,ty+5x3-3t4,-20t2+x2-y2"
J^3 + z*J^2 + z^2*J
-* -- Singular code
  ring R=32003,(t,x,y,z),ds;
  ideal i = -27x6-27t3x4+27x7-9t6x2+18t3x5-9x8-27x4y4-t9+3t6x3-3t3x6+x9-18t3x2y4+18x5y4-3t6y4+6t3x3y4-3x6y4-9x2y8-3t3y8+3x3y8-y12,
     9tx4y+45x7+6t4x2y-6tx5y-27t4x4+30t3x5-30x8+t7y-2t4x3y+tx6y+6tx2y5-18t7x2+5t6x3+18t4x5-10t3x6+5x9+30x5y4+2t4y5-2tx3y5-3t10+6t7x3-3t4x6-18t4x2y4+10t3x3y4-10x6y4+ty9-6t7y4+6t4x3y4+5x3y8-3t4y8,
     -180t2x4+9x6-9x4y2-120t5x2+6t3x4+120t2x5-6x7-6t3x2y2+6x5y2-20t8+t6x2+40t5x3-2t3x5-20t2x6+x8-t6y2+2t3x3y2-x6y2-120t2x2y4+6x4y4-6x2y6-40t5y4+2t3x2y4+40t2x3y4-2x5y4-2t3y6+2x3y6-20t2y8+x2y8-y10,
     -3t2x2y2-30tx5y-t5y2+t2x3y2-75x8+18t5x2y-10t4x3y+10tx6y-t2y6+90t4x5-25t3x6+25x9+6t8y-6t5x3y-10tx3y5-27t8x2+30t7x3-30t4x6-25x6y4+6t5y5-9t11+9t8x3+30t4x3y4-9t8y4,
     60t3x2y-3tx4y+3tx2y3+300t2x5-15x7+20t6y-t4x2y-20t3x3y+tx5y+15x5y2+t4y3-tx3y3-180t6x2+100t5x3+9t4x4-5t3x5-100t2x6+5x8-9t4x2y2+5t3x3y2-5x6y2+20t3y5-tx2y5+ty7-60t9+3t7x2+60t6x3-3t4x5-3t7y2+3t4x3y2+100t2x3y4-5x5y4+5x3y6-60t6y4+3t4x2y4-3t4y6,
     -1200t4x2+120t2x4-3x6-120t2x2y2+6x4y2-3x2y4-400t7+40t5x2+400t4x3-t3x4-40t2x5+x7-40t5y2+2t3x2y2+40t2x3y2-2x5y2-t3y4+x3y4-400t4y4+40t2x2y4-x4y4-40t2y6+2x2y6-y8,
     t3y3+15t2x3y2+75tx6y-9t6y2+125x9-90t5x3y-225t4x6+27t9y+135t8x3-27t12,
     -20t4y2+t2x2y2-t2y4-200t3x3y+10tx5y-10tx3y3-500t2x6+25x8+120t7y-6t5x2y-25x6y2+6t5y3+600t6x3-30t4x5+30t4x3y2-180t10+9t8x2-9t8y2,
     400t5y-40t3x2y+tx4y+40t3y3-2tx2y3+ty5+2000t4x3-200t2x5+5x7+200t2x3y2-10x5y2+5x3y4-1200t8+120t6x2-3t4x4-120t6y2+6t4x2y2-3t4y4,
     -8000t6+1200t4x2-60t2x4+x6-1200t4y2+120t2x2y2-3x4y2-60t2y4+3x2y4-y6,
     9x4z+6t3x2z-6x5z+t6z-2t3x3z+x6z+6x2y4z+2t3y4z-2x3y4z+y8z,
     -3tx2yz-15x5z-t4yz+tx3yz+9t4x2z-5t3x3z+5x6z-ty5z+3t7z-3t4x3z-5x3y4z+3t4y4z,
     60t2x2z-3x4z+3x2y2z+20t5z-t3x2z-20t2x3z+x5z+t3y2z-x3y2z+20t2y4z-x2y4z+y6z,
     t2y2z+10tx3yz+25x6z-6t5yz-30t4x3z+9t8z,
     -20t3yz+tx2yz-ty3z-100t2x3z+5x5z-5x3y2z+60t6z-3t4x2z+3t4y2z,
     400t4z-40t2x2z+x4z+40t2y2z-2x2y2z+y4z,
     -3x2z2-t3z2+x3z2-y4z2,
     tyz2+5x3z2-3t4z2,
     -20t2z2+x2z2-y2z2;
  timer=1;
  option(prot);
  std(i);
*-
--------------------------------------------------------
--std15
R = kk{x,y,z}
ideal"47x7y8z3+91x7y4z7+28x3y6z8+63x2y,
    21x3y2z10+57xy7z+15x3yz5+51xy3z3,
    32x7y4z8+53x6y6z2+17x3y7z2+74xy5z,
    32x10y9z6+23x5y8z8+21x2y3z7+27y5z,
    81x10y10z+19x3y5z5+79x5z7+36xy2z3"
R = kk{x,y,z}
ideal"x2y,
    21x3y2z10+57xy7z+15x3yz5+51xy3z3,
    32x7y4z8+53x6y6z2+17x3y7z2+74xy5z,
    32x10y9z6+23x5y8z8+21x2y3z7+27y5z,
    81x10y10z+19x3y5z5+79x5z7+36xy2z3"

-* -- Singular code
  ring R=32003,(x,y,z),ds;
  ideal i = 63x2y+28x3y6z8+47x7y8z3+91x7y4z7,
     51xy3z3+57xy7z+15x3yz5+21x3y2z10,
     74xy5z+17x3y7z2+53x6y6z2+32x7y4z8,
     27y5z+21x2y3z7+23x5y8z8+32x10y9z6,
     36xy2z3+79x5z7+19x3y5z5+81x10y10z;
  timer=1;
  option(prot);
  std(i);
*-
--------------------------------------------------------
--std16
R = kk{t,x,y,z}
(ideal"t18x2y-t19z-t18z2,
  t26xy-t27z-t25z3,
  t38y2-t37xy")^2
-* -- Singular code
  ring R=32003,(t,x,y,z),ds;
  ideal i = t38z2+2t37z3+t36z4-2t37x2yz-2t36x2yz2+t36x4y2,
     -t45xyz+t46z2-t44xyz2+t45z3+t44z4+t43z5+t44x3y2-t45x2yz-t43x2yz3,
     t56xyz+t55xyz2-t55x3y2-t57y2z-t56y2z2+t56x2y3,
     t52x2y2-2t53xyz+t54z2-2t51xyz3+2t52z4+t50z6,
     -t63x2y2+t64xyz+t62xyz3+t64xy3-t65y2z-t63y2z3,
     t74x2y2-2t75xy3+t76y4;
  timer=1;
  option(prot);
  std(i);
*-
--------------------------------------------------------
--std17
R = kk{x,y,z}
I = ideal"x3+y4+2xz3+z5-3x4y2+2z6+3z7,
       xz3-2x4y2+z6+2z7,
       9x3z2+18x2z5-5z7+12x4y2z2+42x2z6+40x3y2z4+7z9+24x3y2z5,
       -4y3z3-12x6y+32x3y5-2x4yz3,
       12xy3z2+6x5yz2+24y3z5+20x4yz4+56y3z6+12x4yz5"
-* -- Singular code
  ring R=32003,(x,y,z),ds;
  ideal i = x3+y4+2xz3+z5-3x4y2+2z6+3z7,
     xz3-2x4y2+z6+2z7,
     9x3z2+18x2z5-5z7+12x4y2z2+42x2z6+40x3y2z4+7z9+24x3y2z5,
     -4y3z3-12x6y+32x3y5-2x4yz3,
     12xy3z2+6x5yz2+24y3z5+20x4yz4+56y3z6+12x4yz5;
  timer=1;
  option(prot);
  std(i);
*-
--------------------------------------------------------
--std18
R = kk{t,x,y,z}
ideal"4t2z+6z3t+3z3+tz,
  5t2z7y3x+5x2z4t3y+3t7,
  6zt2y+2x8+6z2y2t+2y5"
-* -- Singular code
  ring R=32003,(t,x,y,z),ds;
  ideal i = tz+4t2z+3z3+6tz3,
     3t7+5t3x2yz4+5t2xy3z7,
     6t2yz+2y5+6ty2z2+2x8;
  timer=1;
  option(prot);
  std(i);
*-
--------------------------------------------------------
--std19
R = kk{t,x,y,z,u}
f1 = poly"2t2+x2+2z2+2u2+2y3"
f2 = poly"2tx+yz+2tu+2zu-t3"
f3 = poly"t2+2tz+2xu+2yu-u3"
f4 = poly"2ty+2xz+2tu-z3"
f5 = poly"x+2y+2z+2u"
ideal(f1*f2,f2*f3,f3*f4,f4*f5,f5*f1)
-* -- Singular code
  ring R=32003,(t,x,y,z,u),ds;
  ideal i = 4t3x+2tx3+2t2yz+x2yz+4txz2+2yz3+4t3u+2tx2u+4t2zu+2x2zu+4tz2u+4z3u+4txu2+2yzu2+4tu3+4zu3-2t5-t3x2+4txy3+2y4z-2t3z2+4ty3u+4y3zu-2t3u2-2t3y3,
     2t3x+4t2xz+t2yz+2tyz2+2t3u+4tx2u+4txyu+6t2zu+2xyzu+2y2zu+4tz2u+4txu2+4tyu2+4xzu2+4yzu2-t5-2t4z-2t3xu-2t3yu-2txu3-yzu3-2tu4-2zu4+t3u3,
     2t3y+2t2xz+4t2yz+4txz2+2t3u+4txyu+4ty2u+4t2zu+4x2zu+4xyzu+4txu2+4tyu2-t2z3-2tz4-2xz3u-2yz3u-2tyu3-2xzu3-2tu4+z3u3,
     2txy+4ty2+2x2z+4tyz+4xyz+4xz2+2txu+8tyu+4tzu+4xzu+4tu2-xz3-2yz3-2z4-2z3u,
     2t2x+x3+4t2y+2x2y+4t2z+2x2z+2xz2+4yz2+4z3+4t2u+2x2u+4z2u+2xu2+4yu2+4zu2+4u3+2xy3+4y4+4y3z+4y3u;
  timer=1;
  option(prot);
  std(i);
*-
--------------------------------------------------------
--std20
R = kk{x,y,z}
F = singF(24,23,6,1)
ideal(diff(x,F),diff(y,F),diff(z,F))
-* -- Singular code
  ring R=32003,(x,y,z),ds;
  ideal i = 6x5y6+8x7y5+10x4y8+5x4y5z3+4x3y10+24x23,
     6x6y5+5x8y4+16x5y7+5x5y4z3+10x4y9+23y22,
     3x5y5z2+18z17;
  timer=1;
  option(prot);
  std(i);
*-
