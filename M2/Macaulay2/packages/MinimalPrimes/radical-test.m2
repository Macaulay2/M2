-- These are some tests of computing radicals
--  in small characteristic

exampleE2 = (kk) -> (
     R = kk[symbol x..symbol z];
     ideal"x+3xy3+y4+yz2,-x2z+2y3z+z2+2yz2+3xyz2,3x3+xy2+yz2-2xz3"
     )

exampleE3 = (kk) -> (
     R = kk[symbol x..symbol z];
     ideal"x2+y4+x3z+yz-2xz3,-x2y2-y3z-z3-3yz3,y4-x2z+2y2z-2xyz2"
     )

exampleL = (kk) -> (     
     R = kk[vars(0..9)];
     ideal"2ahi+bh2+2cdj-cei-cgh-deh,ai2+2bhi+2cfj-cgi+d2j-dei-dgh-efh,
       bi2+2dfj-dgi-efi-fgh,f(fj-gi)"
     )

exampleM = (kk) -> (
     R = kk[vars(0..3)];
     ideal"-a3d+b4,
      -a3c+ab3,
      -ac3d+ad4+bc4-bcd3,
      -bc3d2+bd5+c6-c3d3,
      ac5-ac2d3-b2c3d+b2d4,
      -a3d3+a2c4-a2cd3+b3d3,
      -a3d3+b3c3,
      -a3cd2+ab2c3-ab2d3+b3cd2,
      -a3c2d+a2bc3-a2bd3+b3c2d,
      -a3bd2+a3c3,
      a4c2-a3b2d"
      )

example8'3 = (kk) -> (
     R = kk[symbol c, symbol e, symbol f, symbol g, symbol h, 
	    symbol C, symbol E, symbol F, symbol G, symbol H];
     ideal"C+cE-eC-E,F-C,E-G,eF+fH+hE-fE-hF-eH,fG-gF,gH+G-hG-H,cH-hC"
     )

exampleC = (kk) -> (
     R = kk[(symbol a)_1..(symbol a)_2, (symbol b)_1..(symbol b)_2,
     	  (symbol c)_1..(symbol c)_2,
    	  (symbol x)_1..(symbol x)_2,
     	  (symbol y)_1..(symbol y)_2,
     	  (symbol z)_1..(symbol z)_2,
     	  (symbol o)_1..(symbol o)_2];
     ideal"a[1]b[2]+b[1]x[2]+x[1]a[2]-a[2]b[1]-b[2]x[1]-x[2]a[1],
          b[1]c[2]+c[1]y[2]+y[1]b[2]-b[2]c[1]-c[2]y[1]-y[2]b[1],
	  a[1]c[2]+c[1]z[2]+z[1]a[2]-a[2]c[1]-c[2]z[1]-z[2]a[1],
	  c[1]o[2]+o[1]x[2]+x[1]c[2]-c[2]o[1]-o[2]x[1]-x[2]c[1],
	  a[1]o[2]+o[1]y[2]+y[1]a[2]-a[2]o[1]-o[2]y[1]-y[2]a[1],
	  b[1]o[2]+o[1]z[2]+z[1]b[2]-b[2]o[1]-o[2]z[1]-z[2]b[1]
	  a[1],
	  a[2],
	  b[1]-1,
	  b[2]"
      )

end
restart
load "radical-test.m2"
I = exampleE2(ZZ/32003)
time Irad = intersect decompose I

I = exampleE3(ZZ/32003)
time Irad = intersect decompose I
time Irad2 = radical I

Irad2 = radical I
Irad == Irad2
codim I
eliminate({x,y},I)
factor oo_0
radical o5
intersect decompose o5
