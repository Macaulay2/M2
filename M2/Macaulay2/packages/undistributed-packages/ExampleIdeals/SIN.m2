-- Singular examples: local GB's

singF = (a,b,c,t) -> (
     x^a+y^b+z^(3*c)+x^(c+2)*y^(c-1)+x^(c-1)*y^(c-1)*z^3+x^(c-2)*y^c*(y^2+t*x)^2
     )
singH = (a) -> x^a+y^a+z^a+x*y*z*(x+y+z)^2+(x+y+z)^4
singG = (a,b,c,d,e,t) -> (
     x^a+y^b+z^c++x^d*y^(e-5)+x^(d-2)*y^(e-3)+x^(d-3)*y^(e-4)*z^2+x^(d-4)*y^(e-4)*(y^2+t*x)^2)

sin3a = () -> (
     R = ZZ/32003[x,y,z, MonomialOrder=>Weights=>{-1,-1,-1},Global=>false];
     f := singF(13,12,3,1);
     ideal jacobian matrix{{f}}
     )

--------------------------------------------------------
--singular1 5 vars
R = kk[t,x,y,z,w];
ideal"5t3x2zw4 + 2t2y3x5, 
     7yw2 + 4x2y + y2x + 2ztw, 
     3tzw3+ 3yz2w2 + 2yz4"
--------------------------------------------------------
--singular2
R = kk[symbol w, symbol t, symbol x, symbol y, symbol z];
ideal"5t3x2zw4 + 2t2y3x5, 
     7yw2 + 4x2y + y2x + 2ztw, 
     3tzw3+ 3yz2w2 + 2yz4"
--------------------------------------------------------
--singular3
R = kk[symbol w, symbol x, symbol y, symbol z];
singF := (a,b,c,t) -> x^a + y^b + z^(3*c) + x^(c+2) * y^(c-1) + 
   x^(c-1) * y^(c-1) * z^3 + x^(c-2) * y^c * (y^2 + t * x)^2
f1 := singF(11,10,3,1);
ideal(diff(x,f1), diff(y,f1), diff(z,f1))
--------------------------------------------------------
--singular4
R = kk[symbol w, symbol x, symbol y, symbol z];
singH := (a) -> x^a + y^a + z^a + x * y * z * (x + y + z)^2 + (x+y+z)^3
f1 := singH 7;
ideal(diff(x,f1), diff(y,f1), diff(z,f1))
--------------------------------------------------------
--singular5
R = kk[symbol t, symbol w, symbol x, symbol y, symbol z];
ideal(x^2 * t^18 - z^10 * t^10 - z^20,
     x * y^3 * t^26 - z^10 * t^20 - z^30,
     y^6 * t^38 - x * y^3 * w^40)
--------------------------------------------------------
--singular6 -- I don't know what this example is
null
--------------------------------------------------------
--singular7
R = kk[vars(0..5)];
(ideal(b * (a^8 + c^2 * d^2 * e^2 * f^2),
	  c * (a^8 + b^2 * d^2 * e^2 * f^2),
	  d * (a^8 + b^2 * c^2 * e^2 * f^2),
	  e * (a^8 + b^2 * c^2 * d^2 * f^2),
	  f * (a^8 + b^2 * c^2 * d^2 * e^2)))^2
--------------------------------------------------------
--singular8
R = kk[symbol t, symbol x, symbol y, symbol z, symbol w];
ideal(t^18 * x^2 - t^19 * z - t^18 * z^2, 
     t^26 * x * y - t^27 * z - t^25 * z^3,
     t^38 * y^2 - t^37 * x * y * w)
--------------------------------------------------------
--singular9
R = kk[vars(0..4)];
ideal random(R^1, R^{-3,-3,-3,-3,-3})
--------------------------------------------------------
--singular10 homog cyclic 5 roots
R = kk[vars(0..4),symbol h];
ideal"a+b+c+d+e,
     ab + bc + cd + de + ea,
     abc + bcd + cde + dea + eab,
     abcd + bcde + cdea + deab + eabc,
     abcde - h5"
--------------------------------------------------------
--singular11
R = kk[vars(0..4)];
ideal"a4 - b4, 
     b4 - c4, 
     c4 - d4, 
     d4 - e4, 
     a3b + b3c + c3d + d3e + e3a"
--------------------------------------------------------
--singular12
R = kk[symbol u, symbol v, symbol w, symbol x, symbol y, symbol z];
ideal"
     xy + y2 + uz - wz - xz + yz,
     x2 - y2 + uz - wz - xz + z2,
     wy,
     wx - uz + yz,
     w2 - y2 + uz - z2,
     vz - yz - z2,
     vy - y2 - wz + xz + yz + z2,
     vx - y2 + uz + yz - z2,
     vw - y2 + uz + yz - z2,
     v2 + y2 + uz - xz - yz - z2,
     uy + y2 + yz,
     ux - y2 + wz - xz - z2,
     uw - y2 + uz + yz - z2,
     uv - y2 - xz + yz,
     u2 + yz"
--------------------------------------------------------
--singular13
R = kk[vars(0..4)];
ideal random(R^1, R^{-5,-5,-5})
--------------------------------------------------------
--singular14 -- 5 variables over ZZ/31991
R = ZZ/31991[vars(0..4)];
ideal"
     ab + 14766ac - 12713bc + 8997 ad + 1878ae,
     ac + 9210 bc + 9399 ad + 13903bd - 9583ae,
     bc - 13988ad - 4712 bd + 6771 ae - 7341be,
     ad - 2340 bd - 7515 cd + 1575 ae - 4211be,
     bd + 5023 cd - 874  ae + 4773 be + 14016ce,
     cd + 1617 ae - 14718be - 1384 ce + 12060de,
     a3 - 10731b3 + 5818ae2 + 13936be2 + 11725ce2 + 6401de2,
     b3 - 4862 c3 - 2334ae2 + 9991be2 + 14579ce2 + 10173de2,
     c3 - 6087 d3 + 1135ae2 + 4570be2 + 5250ce2 + 1393de2,
     d3 + 11392ae2 + 7125be2 - 15188ce2 - 12706de2 - 10957e3"
--------------------------------------------------------
--singular15 -- nonexistent
null
--------------------------------------------------------
--singular16 -- this one doesn't seem to be the same as in paper
R = kk[symbol w, symbol x, symbol y, symbol z, symbol t];
ideal"
     -2w2x - w2z + 2xyt + y2z,
     2w4 + 4w2x2 + 4w2xz - 10w2y2 - 10w2yt - x3z + 4x2yt 
     + 4xy2z + 2y3t,
     - w2x - 2w2z + xt2 + 2yzt,
     2w4 + 4w2xz - 10w2yt + 4w2z2 - 10w2t2 
     - xz3 + 4yz2t + 2yt3"
--------------------------------------------------------
--singular17
PellikaanJaworski(kk,9)
--------------------------------------------------------
--singular18
PellikaanJaworski(kk,10)
--------------------------------------------------------
--singular19
R = kk[vars(0..3)];
ideal random(R^1, R^{-2,-3,-4,-5,-6,-7,-8,-9,-10})
--------------------------------------------------------
--singular20
R = kk[vars(0..3)];
ideal random(R^1, R^{-5,-6,-7,-8,-9,-10})
