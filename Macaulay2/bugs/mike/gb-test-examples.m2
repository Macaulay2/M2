-----------------------------------------------
-- simple1
R = ZZ/101[x,y,z]
answer'gb = matrix"z,y,x"
I = ideal"x,y,z";
-----------------------------------------------
-- simple2
R = ZZ/101[x,y,z]
answer'gb = matrix"y,x,z2";
I = ideal"x,y,z2,xz+z2";
-----------------------------------------------
-- simple3
R = ZZ/101[x,y,z]
answer'gb = matrix"xy-y2, x2, y3"
I = ideal"xy-y2, x2"
-----------------------------------------------
-- simple4
R = ZZ/101[x,y,z]
answer'gb = matrix"xy-y2, x2, y3"
answer'mingens = matrix"xy-y2,x2"
I = ideal"xy-y2, x2"
-----------------------------------------------
-- simple5
R = ZZ/101[x,y,z]
answer'gb = matrix {{
  (42*x*y+13*y^2-84*x*z-10*y*z-32*z^2)//(42_R),
  (21*x^2-2*y^2+42*x*z+8*y*z+13*z^2)//(21_R),
  y^3-6*y^2*z+12*y*z^2-8*z^3
  }}
I = ideal((3*x+y+z)^2, (7*x+2*y+3*z)^2)
-----------------------------------------------
-- simple6
R = ZZ/101[a..f]
answer'gb = matrix"c, b, a, 0, 0, 0; f, e, d, ce-bf, cd-af, bd-ae"
I = image matrix"a,b,c;d,e,f"
-----------------------------------------------
-- simple7
-- semi-random 
R = ZZ/101[a..d]
test'code = "assert(degrees source gens gb I == {{10},{10},{10},{11},{12},{13},{14},{15},{15},{16},{16},{16},{16},{17},{17},{17},{17},{17},{17},{18},{18},{18},{18},{18},{18},{18},{18},{19},{19},{19},{19},{19},{19},{19},{19},{19},{19},{20},{20},{20},{20},{20},{20},{20},{20},{20},{21},{21},{21},{21},{21},{21},{21},{21},{22},{22},{22},{22},{22},{22},{22},{23},{23},{23},{23},{23},{23},{24},{24},{24},{24},{24},{25},{25},{25},{25},{26},{26},{26},{27},{27},{28}})"
I = ideal((5*a+b+3*c)^10, (3*a+17*b+4*d)^10, (9*b+13*c+12*d)^10)
-----------------------------------------------
-- simple8
-- inhomog, finite field
R = ZZ/101[x,y,z]
answer'gb = matrix"z, y-1, x-1"
I = ideal"xy-1, x2-x, x3-z-1"
-----------------------------------------------
-- simple9
-- inhomog, finite field
R = ZZ/101[x,y,z, MonomialOrder=>Lex]
answer'gb = matrix"z, y-1, x-1"
I = ideal"xy-1, x2-x, x3-z-1"
-----------------------------------------------
-- simple10
-- inhomog, finite field
R = ZZ/101[x,y,z, MonomialOrder=>Lex]
answer'gb = matrix"y7-2y5+y4+y3-2y2-y+1, x-y6+y4-y3+y+1"
I = ideal"x2y-y3-1, xy2-x-1"
-----------------------------------------------
-- simple11
-- inhomog, finite field
R = ZZ/101[x,y, MonomialOrder=>Lex]
answer'gb = matrix"y7+33y5+37y4+45y3+9y2-24y+49, x+25y6-42y4+16y3-39y+6"
I = ideal"x2y-17y3-23, 3xy2-x-6"
-----------------------------------------------
-- simple12
-- inhomog, finite field
R = ZZ/101[x,y,z, MonomialOrder=>Lex]
answer'gb = matrix{{1_R}}
I = ideal"x+y+z, xy+yz+zx, xyz-1, (x+y+z)5+x(xyz-1) + 13"
-----------------------------------------------
-- simple13
-- inhomog, finite field
R = ZZ/101[x,y,z, MonomialOrder=>Lex]
answer'gb = matrix {{z^420-4*z^400-28, y+25*z^20, x-29*z^400}}
I = ideal"3x-y20, 4y-z20, xy-x-1"
-----------------------------------------------
-- simple14
-- inhomog, finite field
R = ZZ/101[x,y,z,w,t, MonomialOrder=>Lex]
answer'gb = matrix"w+20t+39, z+50t7+49, y-t3-2, x-1"
I = ideal"x-1,y-t3-2,2z-t7-3,5w-t-7"
-----------------------------------------------
-- simple15
-- inhomog, finite field
R = ZZ/101[t,a..d, MonomialOrder=>{1,4}]
answer'gb = matrix"ad3-45bc2, b4-13ac, b3d3+21c3, ta-1, tbc2-9d3, tc5-14b2d6"
I = ideal"b4-13ac, 12bc2-7ad3, ta-1"
-----------------------------------------------
-- simple16
-- inhomog, finite field
R = ZZ/101[u..z, MonomialOrder=>Lex]
answer'gb = matrix"x6-3x4y2+23x4z3+6x4z2-3x4z+3x2y4-42x2y2z3+6x2y2z-7x2z6+13x2z5-36x2z4-16x2z3-y6+23y4z3-6y4z2-3y4z+7y2z6+13y2z5+36y2z4-16y2z3+39z9+45z7+46z5, 
  vy3z2+30vyz5-33vyz4+2vyz3+2vyz2+42x4z+25x4-33x2y2z-50x2y2-34x2z4+38x2z3-9x2z2+26x2z-9y4z+25y4+32y2z4+29y2z3-44y2z2-26y2z+49z7+9z6-40z5-22z4+2z3, 
  vx2z+3vx2+5vy2z-3vy2+19vz4+26vz3+8vz2+3x2y-3y3+22yz3-12yz, 
  vx2y+2vy3z-vy3-41vyz4-25vyz3+4vyz2-17x4+35x2y2+33x2z3-23x2z2-50x2z-18y4-37y2z3-11y2z2+46y2z-3z6+27z5+41z4+35z3, 
  vx4+32vx2+4vy4z-vy4+19vy2z4-6vy2z3-39vy2z2-37vy2z-32vy2+43vz6-34vz5+49vz4+2vz3+18vz2-34x4y-31x2y3-35x2yz3+8x2yz2+23x2yz+32x2y-36y5+27y3z3+23y3z2-31y3z-32y3-6yz6-14yz5+40yz4-50yz3+9yz2-27yz, 
  v2z2+3v2z-3vyz+49x2-49y2-28z3-z2-46z, 
  v2yz-50vx2+50vy2-41vz3+35vz2-17yz2+50yz, 
  v2x2-v2y2-39v2z+22vyz2+35vyz-22x2z-31x2+23y2z+31y2+27z4-21z3-22z2-8z, 
  v3-50vz-49v+50y, 
  uy2-19uz3-31uz2-35v2xz-vxy+44xz2, 
  ux-33v2z-vy+45z2-z, 
  uvz-26uy+26vx, 
  uvy+11uz2+2uz-v2x+33xz, 
  uv2-17uz-49u+50x, 
  u2-v2-34z"
I = ideal"x - 3u-3uv2+u3, y-3v-3u2v+v3, z-3u2+3v2"
-----------------------------------------------
-- simple17
-- inhomog, finite field
R = ZZ/32003[a..d]
answer'gb = matrix"b4-13ac, abd3+9142bc2+4572, a2cd3+9142ac3+7737b3"
I = ideal"b4-13ac, 12bc2-7abd3-1"
-----------------------------------------------
-- simple18
R = QQ[a]
answer'gb = matrix {{7*a^2+a+1}}
I = ideal((7*a^2+a+1)*(a^20+a^17+4*a^13+1)^7,
          (7*a^2+a+1)*(6*a^20-123*a^17+4*a^13+1)^6)
-----------------------------------------------
-- simple19
R = ZZ/101[x,y,z,w]
test'code = "
  RT = degreesRing R
  hf = 1-3*T^4+3*T^8-T^12
  J = ideal I_*
  assert(gens gb I == gens gb(J, Hilbert=>hf))
  "
I = ideal((3*x+y+z+w)^4, (7*x+2*y+3*z)^4 + x*w^3, (x+y+z)^4)
-----------------------------------------------
-- simple20
R = ZZ/101[x,y,z,w, MonomialOrder=>Lex]
test'code = "
  RT = degreesRing R
  hf = 1-3*T^4+3*T^8-T^12
  J = ideal I_*
  assert(gens gb I == gens gb(J, Hilbert=>hf))
  "
I = ideal((3*x+y+z+w)^4, (7*x+2*y+3*z)^4 + x*w^3, (x+y+z)^4)
-----------------------------------------------
-- simple21-ZZ
R = ZZ[x,y]
answer'gb = matrix"11, y-5, x+3"
M = matrix"y2+y-(x3-x2)"
I = ideal diff(matrix"x,y", M) + ideal M
-----------------------------------------------
-- simple22-ZZ
R = ZZ[x,y]
answer'gb = matrix"53, y-26, x-17"
I = ideal"2y+1, 3x2+2x, y2 + y - (x3 - x2)"
-----------------------------------------------
-- simple23-ZZ
R = ZZ[x,y]
answer'gb = matrix"24336, 2y, 4x-7800, y2+2x-3900, x2+2x-3900"
I = ideal"-3x2+50x-156,  2y, -x3+25x2+y2-156x"
-----------------------------------------------
-- simple24-ZZ
R = ZZ[x,y, MonomialOrder=>Lex]
answer'gb = matrix {{24336, 2*y, y^3, 2*x+y^2-3900, x*y^2+12168, x^2+y^2}}
I = ideal"-3x2+50x-156,  2y, -x3+25x2+y2-156x"
-----------------------------------------------
-- simple25-ZZ
R = ZZ[y,x, MonomialOrder=>Lex]
answer'gb = matrix {{24336, 4*x-7800, x^2+2*x-3900, 2*y, y^2+2*x-3900}}
I = ideal"-3x2+50x-156,  2y, -x3+25x2+y2-156x"

