--------------------------------------------------------
--alex1, Singular standard bases#1.
R = kk{t,x,y,z}
ideal"5t3x2z+2t2y3x5,7y+4x2y+y2x+2zt,3tz+3yz2+2yz4"
{* -- Singular code
  ring R=32003,(t,x,y,z),ds;
  ideal i = 5t3x2z+2t2x5y3,7y+2tz+4x2y+xy2,3tz+3yz2+2yz4;
  timer=1;
  option(prot);
  std(i);
*}
--------------------------------------------------------
--std2
R = kk{t,x,y,z,u,v}
ideal"xy+xz+2xu+yu+zu+2u2+yv+zv+2uv-3txz-y2z-yz2-3tzu-2yzu3tzv+3tyz2,
      y2+z2+2u2-3u2v,
      tx+z2+2xv-txy,
      -yz-z2-xu-u2+tv-v2,
      -1569xy3-753t2zu,
      -587xz+15625yv"
--------------------------------------------------------
--std3
R = kk{x,y,z}
F = singF(13,12,3,1)
ideal(diff(x,F),diff(y,F),diff(z,F))
{*
  ring R=32003,(x,y,z),ds;
  ideal i = 3x2y3+5x4y2+4xy5+2xy2z3+y7+13x12,
           3x3y2+2x5y+10x2y4+2x2yz3+7xy6+12y11,
	   3x2y2z2+9z8;
  timer=1;
  std(i); // much better than M2
*}
--------------------------------------------------------
--std4
R = kk{t,x,y,z,u}
ideal"2yz+2xu+t2x,
    2tx+z2+2yu+x2y,
    x2+2ty+yz+2zu,
    2xy+2tz+u2+z3,
    y2+2xz+2tu+u5"
--------------------------------------------------------
--std5
R = kk{x,y,z}
F = singH 8
ideal(diff(x,F),diff(y,F),diff(z,F))
{*
  ring R=32003,(x,y,z),ds;
  ideal i = 3x2+6xy+3y2+6xz+6yz+3z2+3x2yz+4xy2z+y3z+4xyz2+2y2z2+yz3+8x7,
              3x2+6xy+3y2+6xz+6yz+3z2+x3z+4x2yz+3xy2z+2x2z2+4xyz2+xz3+8y7,
	      3x2+6xy+3y2+6xz+6yz+3z2+x3y+2x2y2+xy3+4x2yz+4xy2z+3xyz2+8z7;
  timer=1;
  std(i); // much better than M2
*}
--------------------------------------------------------
--std6
R = kk{x,y,z}
F = singG(6,8,10,5,5,0)
ideal(F,diff(x,F),diff(y,F),diff(z,F))
{*
  ring R=32003,(x,y,z),ds;
  ideal i = x5+x3y2+x2yz2+x6+xy5+y8+z10,
     5x4+3x2y2+2xyz2+6x5+y5,
     2x3y+x2z2+5xy4+8y7,
     2x2yz+10z9;
  timer=1;
  std(i); // much better than M2
*}
--------------------------------------------------------
--std7
R = kk{t,x,y,z}
J = ideal"17y6+49y5-9y4+12y3+33y2+11y+73x2z,
  21x2y+17x2z+63y3+11xz+77y2+91y+xz2,
  26y2+44xz+12y+9zxy"
J^2 + x^2*z^2*J
--------------------------------------------------------
--std8
R = kk{t,x,y,z,u,v,w,a,b,c,d}
ideal"bcd-u5,
  y2b2c2-2yzu4bc+z2u8,
  t2a2b2-2tyzu3ab+y2z2u6,
  txc-y3zu,
  zwa-y4u,
  xuw-y5"
--------------------------------------------------------
--std9
R = kk{x,y,z}
F = singF(13,12,3,1)
ideal(F,diff(x,F),diff(y,F),diff(z,F))
--------------------------------------------------------
--std10
R = kk{t,x,y,z}
(ideal"-x28yz+t31-t30x,
     -xy25z+t28+t27y,
     -xyz24+t27+t26z")^2
--------------------------------------------------------
--std11
R = kk{x,y,z}
(ideal"x2+y2+z2-2yz2+y4+xy2z,
     -2yz2+y4+xy2z+x3yz-y2z3,
     y2z+x3y-3y3z-2xyz2"
     )^2
--------------------------------------------------------
--std12
R = kk{x,y,z,w}
ideal"x2-z10-z20,xy3-z10-z30,y6-xy3w40"
--------------------------------------------------------
--std13
R = kk{t,x,y,z,u,v}
ideal"txz+xyz+xy2z+xyz2,
  t2z+xz2+yz2+x2yz+xy2z+xyz2,
  t2x+t2z+xz2+x2yz+xy2z+xyz2,
  txv+xuv+xu2v+xuv2,
  t2v+xv2+uv2+x2uv+xu2v+xuv2,
  t2x+t2v+xv2+x2uv+xu2v+xuv2"
--------------------------------------------------------
--std14
R = kk{t,x,y,z}
J = ideal"-3x2-t3+x3-y4,ty+5x3-3t4,-20t2+x2-y2"
J^3 + z*J^2 + z^2*J
--------------------------------------------------------
--std15
R = kk{x,y,z}
ideal"47x7y8z3+91x7y4z7+28x3y6z8+63x2y,
    21x3y2z10+57xy7z+15x3yz5+51xy3z3,
    32x7y4z8+53x6y6z2+17x3y7z2+74xy5z,
    32x10y9z6+23x5y8z8+21x2y3z7+27y5z,
    81x10y10z+19x3y5z5+79x5z7+36xy2z3"
--------------------------------------------------------
--std16
R = kk{t,x,y,z}
(ideal"t18x2y-t19z-t18z2,
  t26xy-t27z-t25z3,
  t38y2-t37xy")^2
{*
  ring R=32003,(t,x,y,z),ds;
  ideal i = t38z2+2t37z3+t36z4-2t37x2yz-2t36x2yz2+t36x4y2,
   -t45xyz+t46z2-t44xyz2+t45z3+t44z4+t43z5+t44x3y2-t45x2yz-t43x2yz3,
   t56xyz+t55xyz2-t55x3y2-t57y2z-t56y2z2+t56x2y3,
   t52x2y2-2t53xyz+t54z2-2t51xyz3+2t52z4+t50z6,
   -t63x2y2+t64xyz+t62xyz3+t64xy3-t65y2z-t63y2z3,
   t74x2y2-2t75xy3+t76y4;
*}
--------------------------------------------------------
--std17
R = kk{x,y,z}
I = ideal"x3+y4+2xz3+z5-3x4y2+2z6+3z7,
       xz3-2x4y2+z6+2z7,
       9x3z2+18x2z5-5z7+12x4y2z2+42x2z6+40x3y2z4+7z9+24x3y2z5,
       -4y3z3-12x6y+32x3y5-2x4y3,
       12xy3z2+6x5yz2+24y3z5+20x4yz4+56y3z6+12x4yz5"
--------------------------------------------------------
--std18
R = kk{t,x,y,z}
ideal"4t2z+6z3t+3z3+tz,
  5t2z7y3x+5x2z4t3y+3t7,
  6zt2y+2x8+6z2y2t+2y5"
--------------------------------------------------------
--std19
R = kk{t,x,y,z,u}
f1 = poly"2t2+x2+2z2+2u2+2y3"
f2 = poly"2tx+yz+2tu+2zu-t3"
f3 = poly"t2+2tz+2xu+2yu-u3"
f4 = poly"2ty+2xz+2tu-z3"
f5 = poly"x+2y+2z+2u"
ideal(f1*f2,f2*f3,f3*f4,f4*f5,f5*f1)
--------------------------------------------------------
--std20
R = kk{x,y,z}
F = singF(24,23,6,1)
ideal(diff(x,F),diff(y,F),diff(z,F))
