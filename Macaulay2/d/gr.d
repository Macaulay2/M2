--		Copyright 1994 by Daniel R. Grayson
-- TEST pc graphics

use C;
use stdio;
use strings;
use grx;

graphics_mode();
-- ;(define screen    (GrSaveContext (null)))
-- ;(define offscreen (GrCreateContext xsize ysize (null) (null)))
-- ;(GrSetContext offscreen)
background:=GrBlack();
white := GrWhite();
gray := GrAllocColor(128, 128, 128);
yellow := GrAllocColor(255, 255, 0);
cyan := GrAllocColor(0, 255, 255);

magenta := GrAllocColor( 255,   0, 255);
red := GrAllocColor( 255,   0,   0);
blue := GrAllocColor(   0,   0, 255);
dodeccolor := red;
icoscolor := blue;
ptcolor := gray;
point := {x:double, y:double, z:double,
		X:int, Y:int,
		lastX:int, lastY:int,
		color:int,
		label:(null or string)};
points := array(point);
dist(p:point,q:point):double := sqrt(sqr(p.x)+sqr(p.y)+sqr(p.z));
pt(x:double,y:double,z:double):point :=
	point( x, y, z, 0, 0, 0, 0, ptcolor, null());
(p:point) + (q:point):point := pt(p.x+q.x,p.y+q.y,p.z+q.z);
(p:point) - (q:point):point := pt(p.x-q.x,p.y-q.y,p.z-q.z);
(a:double) * (q:point):point := pt(q.x*a,q.y*a,q.z*a);
(q:point) / (a:double):point := pt(q.x/a,q.y/a,q.z/a);
ctr(p:point,q:point,r:point):point := (p+q+r)/3.;
line := {p:point, q:point, color:int};
lines := array(line);
ln (p:point, q:point):line := line(p,q,icoscolor);
scale := 3 * ysize;
offset := 12;
project(p:point):void := (
	p.X = int(p.x / (p.z + offset) * scale) + xsize/2 ;
	p.Y = int(p.y / (p.z + offset) * scale) + ysize/2 );
update(p:point):void := (
	p.lastX = p.X;
	p.lastY = p.Y; );
update(pp:points):void := foreach p in pp do update(p);
draw(p:point,color:int):void := (
	GrFilledCircle (p.lastX, p.lastY, 2, background);
	GrFilledCircle (p.    X, p.    Y, 2, color);
	when p.label
	is s:string do (
	     grtext(p.lastX + 12, p.lastY + 4, s, background, background);
	     grtext(p.    X + 12, p.    Y + 4, s, color     , background);
	     )
	else nothing);
draw(p:point):void := draw(p,p.color);
draw(l:line):void := (
	p := l.p;
	q := l.q;
	GrLine(p.lastX, p.lastY, q.lastX, q.lastY, background);
   	GrLine(p.    X, p.    Y, q.    X, q.    Y, l.color));
draw(c:points):void := foreach p in c do draw(p);
draw(c:lines):void := foreach l in c do draw(l);

ca := pt( 1.,  1.,  1.);
cb := pt( 1.,  1., -1.);
cc := pt( 1., -1.,  1.);
cd := pt( 1., -1., -1.);
ce := pt(-1.,  1.,  1.);
cf := pt(-1.,  1., -1.);
cg := pt(-1., -1.,  1.);
ch := pt(-1., -1., -1.);
cube_points := points(ca, cb, cc, cd, ce, cf, cg, ch);
cube := lines(
    ln(ca,cb), ln(ca,cc), ln(ca,ce), ln(cg,ch),
    ln(cb,cd), ln(cb,cf), ln(cc,cd), ln(cc,cg),
    ln(cd,ch), ln(ce,cf), ln(ce,cg), ln(cf,ch));
t := (1 + sqrt(5.))/2;
a := pt( 0.,  1.,  t);
b := pt( 0.,  1., -t);
c := pt( 0., -1.,  t);
d := pt( 0., -1., -t);
e := pt( t,   0.,  1.);
f := pt( t,   0., -1.);
g := pt(-t,   0.,  1.);
h := pt(-t,   0., -1.);
i := pt( 1.,  t,   0.);
j := pt( 1., -t,   0.);
k := pt(-1.,  t,   0.);
l := pt(-1., -t,   0.);
A := ctr(a,e,i); Q := ctr(a,g,k);
B := ctr(b,f,i); R := ctr(b,h,k);
CC := ctr(c,e,j); S := ctr(c,g,l);
D := ctr(d,h,l); T := ctr(d,f,j);
E := ctr(a,c,e); F := ctr(a,c,g); 
G := ctr(b,d,f); H := ctr(b,d,h);
I := ctr(e,f,i); J := ctr(e,f,j);
K := ctr(g,h,k); L := ctr(g,h,l);
M := ctr(a,i,k); N := ctr(b,i,k);
O := ctr(c,j,l); P := ctr(d,j,l);
icosahedron_points := points( a,b,c,d,e,f,g,h,i,j,k,l);
dodecahedron_points := points(A,B,CC,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T);
a.label = "a"; b.label = "b"; c.label = "c";
d.label = "d"; e.label = "e"; f.label = "f";
g.label = "g"; h.label = "h"; i.label = "i";
j.label = "j"; k.label = "k"; l.label = "l";
A.label = "A"; B.label = "B"; CC.label = "CC";
D.label = "D"; E.label = "E"; F.label = "F";
G.label = "G"; H.label = "H"; I.label = "I";
J.label = "J"; K.label = "K"; L.label = "L";
M.label = "M"; N.label = "N"; O.label = "O";
P.label = "P"; Q.label = "Q"; R.label = "R";
S.label = "S"; T.label = "T";
icosahedron := lines(
    line(a,c,icoscolor), 	line(E,F,magenta),
    line(a,e,icoscolor), 	line(A,E,magenta),
    line(a,g,icoscolor), 	line(F,Q,magenta),
    line(a,i,icoscolor), 	line(A,M,magenta),
    line(a,k,icoscolor), 	line(Q,M,magenta),
    line(b,d,icoscolor), 	line(G,H,dodeccolor),
    line(b,i,icoscolor), 	line(B,N,dodeccolor),
    line(b,k,icoscolor), 	line(R,N,dodeccolor),
    line(b,f,icoscolor), 	line(B,G,dodeccolor),
    line(b,h,icoscolor), 	line(H,R,dodeccolor),
    line(c,j,icoscolor), 	line(CC,O,dodeccolor),
    line(c,l,icoscolor), 	line(S,O,dodeccolor),
    line(c,e,yellow), 		line(CC,E,dodeccolor),
    line(c,g,yellow), 		line(F,S,dodeccolor),
    line(d,j,icoscolor), 	line(P,T,dodeccolor),
    line(d,l,icoscolor), 	line(P,D,dodeccolor),
    line(d,f,icoscolor), 	line(T,G,dodeccolor),
    line(d,h,icoscolor), 	line(D,H,dodeccolor),
    line(e,f,icoscolor), 	line(I,J,dodeccolor),
    line(e,i,yellow), 		line(A,I,dodeccolor),
    line(e,j,icoscolor), 	line(CC,J,dodeccolor),
    line(f,i,icoscolor), 	line(B,I,dodeccolor),
    line(f,j,icoscolor), 	line(J,T,dodeccolor),
    line(g,h,icoscolor), 	line(K,L,dodeccolor),
    line(g,k,yellow),	 	line(K,Q,dodeccolor),
    line(g,l,icoscolor), 	line(L,S,dodeccolor),
    line(h,k,icoscolor), 	line(K,R,dodeccolor),
    line(h,l,icoscolor), 	line(D,L,dodeccolor),
    line(i,k,yellow), 		line(M,N,dodeccolor),
    line(j,l,icoscolor), 	line(O,P,dodeccolor));

complex := {re:double, im:double};
expi(theta:double):complex := complex(cos(theta),sin(theta));

x_rotate (p:point,z:complex):void := (
	Y := z.re * p.y - z.im * p.z;
	Z := z.im * p.y + z.re * p.z;
	p.y = Y;
	p.z = Z;
	project(p));
z_rotate(p:point,z:complex):void := (
	X := z.re * p.x - z.im * p.y ;
	Y := z.im * p.x + z.re * p.y ;
	p.x = X;
	p.y = Y;
	project(p));

x_rotate(l:points,z:complex):void := foreach p in l do x_rotate(p, z);
z_rotate(l:points,z:complex):void := foreach p in l do z_rotate(p, z);
pause := 40000;
delay():void := for pause do nothing;
speed := 0.025;
x_angle := speed;
z_angle := speed;
single_step := false;
while true do (
     if single_step then (
	key := getkey();
	if key == char(3) then break;
	if key == 'q' then break;
	if key == 's' then single_step = false;
	);
     if kbhit() then (
	key := getkey();
	if key == char(3) then break;
	if key == 'q' then break;
	if key == '+' then speed = speed + 0.005;
	if key == '-' then speed = speed - 0.005;
	if key == 'p' then (getkey(););
	if key == 's' then single_step = true;
	);
     z := expi(x_angle);
     x_rotate(icosahedron_points, z);
     x_rotate(dodecahedron_points, z);
     x_angle = speed + (x_angle - speed) * drand(0.95,1.02);
     z = expi(z_angle);
     z_rotate(icosahedron_points, z);
     z_rotate(dodecahedron_points, z);
     z_angle = speed + (z_angle - speed) * drand(0.95,1.02);
     draw(icosahedron);
     draw(dodecahedron_points);
     draw(icosahedron_points);
     update(dodecahedron_points);
     update(icosahedron_points);
     --GrBitBlt( screen, 0, 0, offscreen, 0, 0, xsize, ysize, GrWRITE);
     delay();
     );
text_mode();
-- MouseUnInit();
stdout << "xsize = " << xsize << "  ysize = " << ysize << endl
       << "adapter = " << adapter_type() << endl
       << "number of colors = " << GrNumColors() << endl;
