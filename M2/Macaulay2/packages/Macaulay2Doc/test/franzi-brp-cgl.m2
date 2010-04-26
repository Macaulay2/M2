-- Example: 16 node Conway game of life, on a torus
--  load "~/2010March/Macaulay2/packages/Macaulay2Doc/test/franzi-brp-cgl.m2"
R = ZZ/2[vars(0..15), MonomialOrder=>Lex]
l = apply(gens R, x-> x^2+x);
QR = R/l;

L = {a*b*d*e*f*h*m*n + a*b*d*e*f*h*m*p + a*b*d*e*f*h*m + a*b*d*e*f*h*n*p +
a*b*d*e*f*h*n + a*b*d*e*f*h*p + a*b*d*e*f*m*n*p + a*b*d*e*f*m*n +
a*b*d*e*f*m*p + a*b*d*e*f*n*p + a*b*d*e*h*m*n*p + a*b*d*e*h*m*n +
a*b*d*e*h*m*p + a*b*d*e*h*n*p + a*b*d*e*m*n*p + a*b*d*e + a*b*d*f*h*m*n*p +
a*b*d*f*h*m*n + a*b*d*f*h*m*p + a*b*d*f*h*n*p + a*b*d*f*m*n*p + a*b*d*f +
a*b*d*h*m*n*p + a*b*d*h + a*b*d*m + a*b*d*n + a*b*d*p + a*b*d +
a*b*e*f*h*m*n*p + a*b*e*f*h*m*n + a*b*e*f*h*m*p + a*b*e*f*h*n*p +
a*b*e*f*m*n*p + a*b*e*f + a*b*e*h*m*n*p + a*b*e*h + a*b*e*m + a*b*e*n +
a*b*e*p + a*b*e + a*b*f*h*m*n*p + a*b*f*h + a*b*f*m + a*b*f*n + a*b*f*p +
a*b*f + a*b*h*m + a*b*h*n + a*b*h*p + a*b*h + a*b*m*n + a*b*m*p + a*b*m +
a*b*n*p + a*b*n + a*b*p + a*d*e*f*h*m*n*p + a*d*e*f*h*m*n + a*d*e*f*h*m*p +
a*d*e*f*h*n*p + a*d*e*f*m*n*p + a*d*e*f + a*d*e*h*m*n*p + a*d*e*h + a*d*e*m +
a*d*e*n + a*d*e*p + a*d*e + a*d*f*h*m*n*p + a*d*f*h + a*d*f*m + a*d*f*n +
a*d*f*p + a*d*f + a*d*h*m + a*d*h*n + a*d*h*p + a*d*h + a*d*m*n + a*d*m*p +
a*d*m + a*d*n*p + a*d*n + a*d*p + a*e*f*h*m*n*p + a*e*f*h + a*e*f*m + a*e*f*n
+ a*e*f*p + a*e*f + a*e*h*m + a*e*h*n + a*e*h*p + a*e*h + a*e*m*n + a*e*m*p +
a*e*m + a*e*n*p + a*e*n + a*e*p + a*f*h*m + a*f*h*n + a*f*h*p + a*f*h +
a*f*m*n + a*f*m*p + a*f*m + a*f*n*p + a*f*n + a*f*p + a*h*m*n + a*h*m*p +
a*h*m + a*h*n*p + a*h*n + a*h*p + a*m*n*p + a*m*n + a*m*p + a*n*p +
b*d*e*f*h*m*n + b*d*e*f*h*m*p + b*d*e*f*h*n*p + b*d*e*f*m*n*p + b*d*e*h*m*n*p
+ b*d*e + b*d*f*h*m*n*p + b*d*f + b*d*h + b*d*m + b*d*n + b*d*p +
b*e*f*h*m*n*p + b*e*f + b*e*h + b*e*m + b*e*n + b*e*p + b*f*h + b*f*m + b*f*n
+ b*f*p + b*h*m + b*h*n + b*h*p + b*m*n + b*m*p + b*n*p + d*e*f*h*m*n*p +
d*e*f + d*e*h + d*e*m + d*e*n + d*e*p + d*f*h + d*f*m + d*f*n + d*f*p + d*h*m
+ d*h*n + d*h*p + d*m*n + d*m*p + d*n*p + e*f*h + e*f*m + e*f*n + e*f*p +
e*h*m + e*h*n + e*h*p + e*m*n + e*m*p + e*n*p + f*h*m + f*h*n + f*h*p + f*m*n
+ f*m*p + f*n*p + h*m*n + h*m*p + h*n*p + m*n*p, a*b*c*e*f*g*m*n +
a*b*c*e*f*g*m*o + a*b*c*e*f*g*m + a*b*c*e*f*g*n*o + a*b*c*e*f*g*n +
a*b*c*e*f*g*o + a*b*c*e*f*m*n*o + a*b*c*e*f*m*n + a*b*c*e*f*m*o +
a*b*c*e*f*n*o + a*b*c*e*g*m*n*o + a*b*c*e*g*m*n + a*b*c*e*g*m*o +
a*b*c*e*g*n*o + a*b*c*e*m*n*o + a*b*c*e + a*b*c*f*g*m*n*o + a*b*c*f*g*m*n +
a*b*c*f*g*m*o + a*b*c*f*g*n*o + a*b*c*f*m*n*o + a*b*c*f + a*b*c*g*m*n*o +
a*b*c*g + a*b*c*m + a*b*c*n + a*b*c*o + a*b*c + a*b*e*f*g*m*n*o +
a*b*e*f*g*m*n + a*b*e*f*g*m*o + a*b*e*f*g*n*o + a*b*e*f*m*n*o + a*b*e*f +
a*b*e*g*m*n*o + a*b*e*g + a*b*e*m + a*b*e*n + a*b*e*o + a*b*e + a*b*f*g*m*n*o
+ a*b*f*g + a*b*f*m + a*b*f*n + a*b*f*o + a*b*f + a*b*g*m + a*b*g*n + a*b*g*o
+ a*b*g + a*b*m*n + a*b*m*o + a*b*m + a*b*n*o + a*b*n + a*b*o + a*c*e*f*g*m*n
+ a*c*e*f*g*m*o + a*c*e*f*g*n*o + a*c*e*f*m*n*o + a*c*e*g*m*n*o + a*c*e +
a*c*f*g*m*n*o + a*c*f + a*c*g + a*c*m + a*c*n + a*c*o + a*e*f*g*m*n*o + a*e*f
+ a*e*g + a*e*m + a*e*n + a*e*o + a*f*g + a*f*m + a*f*n + a*f*o + a*g*m +
a*g*n + a*g*o + a*m*n + a*m*o + a*n*o + b*c*e*f*g*m*n*o + b*c*e*f*g*m*n +
b*c*e*f*g*m*o + b*c*e*f*g*n*o + b*c*e*f*m*n*o + b*c*e*f + b*c*e*g*m*n*o +
b*c*e*g + b*c*e*m + b*c*e*n + b*c*e*o + b*c*e + b*c*f*g*m*n*o + b*c*f*g +
b*c*f*m + b*c*f*n + b*c*f*o + b*c*f + b*c*g*m + b*c*g*n + b*c*g*o + b*c*g +
b*c*m*n + b*c*m*o + b*c*m + b*c*n*o + b*c*n + b*c*o + b*e*f*g*m*n*o + b*e*f*g
+ b*e*f*m + b*e*f*n + b*e*f*o + b*e*f + b*e*g*m + b*e*g*n + b*e*g*o + b*e*g +
b*e*m*n + b*e*m*o + b*e*m + b*e*n*o + b*e*n + b*e*o + b*f*g*m + b*f*g*n +
b*f*g*o + b*f*g + b*f*m*n + b*f*m*o + b*f*m + b*f*n*o + b*f*n + b*f*o +
b*g*m*n + b*g*m*o + b*g*m + b*g*n*o + b*g*n + b*g*o + b*m*n*o + b*m*n + b*m*o
+ b*n*o + c*e*f*g*m*n*o + c*e*f + c*e*g + c*e*m + c*e*n + c*e*o + c*f*g +
c*f*m + c*f*n + c*f*o + c*g*m + c*g*n + c*g*o + c*m*n + c*m*o + c*n*o + e*f*g
+ e*f*m + e*f*n + e*f*o + e*g*m + e*g*n + e*g*o + e*m*n + e*m*o + e*n*o +
f*g*m + f*g*n + f*g*o + f*m*n + f*m*o + f*n*o + g*m*n + g*m*o + g*n*o + m*n*o,
b*c*d*f*g*h*n*o + b*c*d*f*g*h*n*p + b*c*d*f*g*h*n + b*c*d*f*g*h*o*p +
b*c*d*f*g*h*o + b*c*d*f*g*h*p + b*c*d*f*g*n*o*p + b*c*d*f*g*n*o +
b*c*d*f*g*n*p + b*c*d*f*g*o*p + b*c*d*f*h*n*o*p + b*c*d*f*h*n*o +
b*c*d*f*h*n*p + b*c*d*f*h*o*p + b*c*d*f*n*o*p + b*c*d*f + b*c*d*g*h*n*o*p +
b*c*d*g*h*n*o + b*c*d*g*h*n*p + b*c*d*g*h*o*p + b*c*d*g*n*o*p + b*c*d*g +
b*c*d*h*n*o*p + b*c*d*h + b*c*d*n + b*c*d*o + b*c*d*p + b*c*d +
b*c*f*g*h*n*o*p + b*c*f*g*h*n*o + b*c*f*g*h*n*p + b*c*f*g*h*o*p +
b*c*f*g*n*o*p + b*c*f*g + b*c*f*h*n*o*p + b*c*f*h + b*c*f*n + b*c*f*o +
b*c*f*p + b*c*f + b*c*g*h*n*o*p + b*c*g*h + b*c*g*n + b*c*g*o + b*c*g*p +
b*c*g + b*c*h*n + b*c*h*o + b*c*h*p + b*c*h + b*c*n*o + b*c*n*p + b*c*n +
b*c*o*p + b*c*o + b*c*p + b*d*f*g*h*n*o + b*d*f*g*h*n*p + b*d*f*g*h*o*p +
b*d*f*g*n*o*p + b*d*f*h*n*o*p + b*d*f + b*d*g*h*n*o*p + b*d*g + b*d*h + b*d*n
+ b*d*o + b*d*p + b*f*g*h*n*o*p + b*f*g + b*f*h + b*f*n + b*f*o + b*f*p +
b*g*h + b*g*n + b*g*o + b*g*p + b*h*n + b*h*o + b*h*p + b*n*o + b*n*p + b*o*p
+ c*d*f*g*h*n*o*p + c*d*f*g*h*n*o + c*d*f*g*h*n*p + c*d*f*g*h*o*p +
c*d*f*g*n*o*p + c*d*f*g + c*d*f*h*n*o*p + c*d*f*h + c*d*f*n + c*d*f*o +
c*d*f*p + c*d*f + c*d*g*h*n*o*p + c*d*g*h + c*d*g*n + c*d*g*o + c*d*g*p +
c*d*g + c*d*h*n + c*d*h*o + c*d*h*p + c*d*h + c*d*n*o + c*d*n*p + c*d*n +
c*d*o*p + c*d*o + c*d*p + c*f*g*h*n*o*p + c*f*g*h + c*f*g*n + c*f*g*o +
c*f*g*p + c*f*g + c*f*h*n + c*f*h*o + c*f*h*p + c*f*h + c*f*n*o + c*f*n*p +
c*f*n + c*f*o*p + c*f*o + c*f*p + c*g*h*n + c*g*h*o + c*g*h*p + c*g*h +
c*g*n*o + c*g*n*p + c*g*n + c*g*o*p + c*g*o + c*g*p + c*h*n*o + c*h*n*p +
c*h*n + c*h*o*p + c*h*o + c*h*p + c*n*o*p + c*n*o + c*n*p + c*o*p +
d*f*g*h*n*o*p + d*f*g + d*f*h + d*f*n + d*f*o + d*f*p + d*g*h + d*g*n + d*g*o
+ d*g*p + d*h*n + d*h*o + d*h*p + d*n*o + d*n*p + d*o*p + f*g*h + f*g*n +
f*g*o + f*g*p + f*h*n + f*h*o + f*h*p + f*n*o + f*n*p + f*o*p + g*h*n + g*h*o
+ g*h*p + g*n*o + g*n*p + g*o*p + h*n*o + h*n*p + h*o*p + n*o*p,
a*c*d*e*g*h*m*o + a*c*d*e*g*h*m*p + a*c*d*e*g*h*m + a*c*d*e*g*h*o*p +
a*c*d*e*g*h*o + a*c*d*e*g*h*p + a*c*d*e*g*m*o*p + a*c*d*e*g*m*o +
a*c*d*e*g*m*p + a*c*d*e*g*o*p + a*c*d*e*h*m*o*p + a*c*d*e*h*m*o +
a*c*d*e*h*m*p + a*c*d*e*h*o*p + a*c*d*e*m*o*p + a*c*d*e + a*c*d*g*h*m*o*p +
a*c*d*g*h*m*o + a*c*d*g*h*m*p + a*c*d*g*h*o*p + a*c*d*g*m*o*p + a*c*d*g +
a*c*d*h*m*o*p + a*c*d*h + a*c*d*m + a*c*d*o + a*c*d*p + a*c*d + a*c*e*g*h*m*o
+ a*c*e*g*h*m*p + a*c*e*g*h*o*p + a*c*e*g*m*o*p + a*c*e*h*m*o*p + a*c*e +
a*c*g*h*m*o*p + a*c*g + a*c*h + a*c*m + a*c*o + a*c*p + a*d*e*g*h*m*o*p +
a*d*e*g*h*m*o + a*d*e*g*h*m*p + a*d*e*g*h*o*p + a*d*e*g*m*o*p + a*d*e*g +
a*d*e*h*m*o*p + a*d*e*h + a*d*e*m + a*d*e*o + a*d*e*p + a*d*e + a*d*g*h*m*o*p
+ a*d*g*h + a*d*g*m + a*d*g*o + a*d*g*p + a*d*g + a*d*h*m + a*d*h*o + a*d*h*p
+ a*d*h + a*d*m*o + a*d*m*p + a*d*m + a*d*o*p + a*d*o + a*d*p + a*e*g*h*m*o*p
+ a*e*g + a*e*h + a*e*m + a*e*o + a*e*p + a*g*h + a*g*m + a*g*o + a*g*p +
a*h*m + a*h*o + a*h*p + a*m*o + a*m*p + a*o*p + c*d*e*g*h*m*o*p +
c*d*e*g*h*m*o + c*d*e*g*h*m*p + c*d*e*g*h*o*p + c*d*e*g*m*o*p + c*d*e*g +
c*d*e*h*m*o*p + c*d*e*h + c*d*e*m + c*d*e*o + c*d*e*p + c*d*e + c*d*g*h*m*o*p
+ c*d*g*h + c*d*g*m + c*d*g*o + c*d*g*p + c*d*g + c*d*h*m + c*d*h*o + c*d*h*p
+ c*d*h + c*d*m*o + c*d*m*p + c*d*m + c*d*o*p + c*d*o + c*d*p + c*e*g*h*m*o*p
+ c*e*g + c*e*h + c*e*m + c*e*o + c*e*p + c*g*h + c*g*m + c*g*o + c*g*p +
c*h*m + c*h*o + c*h*p + c*m*o + c*m*p + c*o*p + d*e*g*h*m*o*p + d*e*g*h +
d*e*g*m + d*e*g*o + d*e*g*p + d*e*g + d*e*h*m + d*e*h*o + d*e*h*p + d*e*h +
d*e*m*o + d*e*m*p + d*e*m + d*e*o*p + d*e*o + d*e*p + d*g*h*m + d*g*h*o +
d*g*h*p + d*g*h + d*g*m*o + d*g*m*p + d*g*m + d*g*o*p + d*g*o + d*g*p +
d*h*m*o + d*h*m*p + d*h*m + d*h*o*p + d*h*o + d*h*p + d*m*o*p + d*m*o + d*m*p
+ d*o*p + e*g*h + e*g*m + e*g*o + e*g*p + e*h*m + e*h*o + e*h*p + e*m*o +
e*m*p + e*o*p + g*h*m + g*h*o + g*h*p + g*m*o + g*m*p + g*o*p + h*m*o + h*m*p
+ h*o*p + m*o*p, a*b*d*e*f*h*i*j + a*b*d*e*f*h*i*l + a*b*d*e*f*h*i +
a*b*d*e*f*h*j*l + a*b*d*e*f*h*j + a*b*d*e*f*h*l + a*b*d*e*f*i*j*l +
a*b*d*e*f*i*j + a*b*d*e*f*i*l + a*b*d*e*f*j*l + a*b*d*e*h*i*j*l +
a*b*d*e*h*i*j + a*b*d*e*h*i*l + a*b*d*e*h*j*l + a*b*d*e*i*j*l + a*b*d*e +
a*b*d*f*h*i*j + a*b*d*f*h*i*l + a*b*d*f*h*j*l + a*b*d*f*i*j*l + a*b*d*h*i*j*l
+ a*b*d + a*b*e*f*h*i*j*l + a*b*e*f*h*i*j + a*b*e*f*h*i*l + a*b*e*f*h*j*l +
a*b*e*f*i*j*l + a*b*e*f + a*b*e*h*i*j*l + a*b*e*h + a*b*e*i + a*b*e*j +
a*b*e*l + a*b*e + a*b*f*h*i*j*l + a*b*f + a*b*h + a*b*i + a*b*j + a*b*l +
a*d*e*f*h*i*j*l + a*d*e*f*h*i*j + a*d*e*f*h*i*l + a*d*e*f*h*j*l +
a*d*e*f*i*j*l + a*d*e*f + a*d*e*h*i*j*l + a*d*e*h + a*d*e*i + a*d*e*j +
a*d*e*l + a*d*e + a*d*f*h*i*j*l + a*d*f + a*d*h + a*d*i + a*d*j + a*d*l +
a*e*f*h*i*j*l + a*e*f*h + a*e*f*i + a*e*f*j + a*e*f*l + a*e*f + a*e*h*i +
a*e*h*j + a*e*h*l + a*e*h + a*e*i*j + a*e*i*l + a*e*i + a*e*j*l + a*e*j +
a*e*l + a*f*h + a*f*i + a*f*j + a*f*l + a*h*i + a*h*j + a*h*l + a*i*j + a*i*l
+ a*j*l + b*d*e*f*h*i*j*l + b*d*e*f*h*i*j + b*d*e*f*h*i*l + b*d*e*f*h*j*l +
b*d*e*f*i*j*l + b*d*e*f + b*d*e*h*i*j*l + b*d*e*h + b*d*e*i + b*d*e*j +
b*d*e*l + b*d*e + b*d*f*h*i*j*l + b*d*f + b*d*h + b*d*i + b*d*j + b*d*l +
b*e*f*h*i*j*l + b*e*f*h + b*e*f*i + b*e*f*j + b*e*f*l + b*e*f + b*e*h*i +
b*e*h*j + b*e*h*l + b*e*h + b*e*i*j + b*e*i*l + b*e*i + b*e*j*l + b*e*j +
b*e*l + b*f*h + b*f*i + b*f*j + b*f*l + b*h*i + b*h*j + b*h*l + b*i*j + b*i*l
+ b*j*l + d*e*f*h*i*j*l + d*e*f*h + d*e*f*i + d*e*f*j + d*e*f*l + d*e*f +
d*e*h*i + d*e*h*j + d*e*h*l + d*e*h + d*e*i*j + d*e*i*l + d*e*i + d*e*j*l +
d*e*j + d*e*l + d*f*h + d*f*i + d*f*j + d*f*l + d*h*i + d*h*j + d*h*l + d*i*j
+ d*i*l + d*j*l + e*f*h*i + e*f*h*j + e*f*h*l + e*f*h + e*f*i*j + e*f*i*l +
e*f*i + e*f*j*l + e*f*j + e*f*l + e*h*i*j + e*h*i*l + e*h*i + e*h*j*l + e*h*j
+ e*h*l + e*i*j*l + e*i*j + e*i*l + e*j*l + f*h*i + f*h*j + f*h*l + f*i*j +
f*i*l + f*j*l + h*i*j + h*i*l + h*j*l + i*j*l, a*b*c*e*f*g*i*j +
a*b*c*e*f*g*i*k + a*b*c*e*f*g*i + a*b*c*e*f*g*j*k + a*b*c*e*f*g*j +
a*b*c*e*f*g*k + a*b*c*e*f*i*j*k + a*b*c*e*f*i*j + a*b*c*e*f*i*k +
a*b*c*e*f*j*k + a*b*c*e*g*i*j + a*b*c*e*g*i*k + a*b*c*e*g*j*k + a*b*c*e*i*j*k
+ a*b*c*f*g*i*j*k + a*b*c*f*g*i*j + a*b*c*f*g*i*k + a*b*c*f*g*j*k +
a*b*c*f*i*j*k + a*b*c*f + a*b*c*g*i*j*k + a*b*c + a*b*e*f*g*i*j*k +
a*b*e*f*g*i*j + a*b*e*f*g*i*k + a*b*e*f*g*j*k + a*b*e*f*i*j*k + a*b*e*f +
a*b*e*g*i*j*k + a*b*e + a*b*f*g*i*j*k + a*b*f*g + a*b*f*i + a*b*f*j + a*b*f*k
+ a*b*f + a*b*g + a*b*i + a*b*j + a*b*k + a*c*e*f*g*i*j*k + a*c*e*f*g*i*j +
a*c*e*f*g*i*k + a*c*e*f*g*j*k + a*c*e*f*i*j*k + a*c*e*f + a*c*e*g*i*j*k +
a*c*e + a*c*f*g*i*j*k + a*c*f*g + a*c*f*i + a*c*f*j + a*c*f*k + a*c*f + a*c*g
+ a*c*i + a*c*j + a*c*k + a*e*f*g*i*j*k + a*e*f*g + a*e*f*i + a*e*f*j +
a*e*f*k + a*e*f + a*e*g + a*e*i + a*e*j + a*e*k + a*f*g*i + a*f*g*j + a*f*g*k
+ a*f*g + a*f*i*j + a*f*i*k + a*f*i + a*f*j*k + a*f*j + a*f*k + a*g*i + a*g*j
+ a*g*k + a*i*j + a*i*k + a*j*k + b*c*e*f*g*i*j*k + b*c*e*f*g*i*j +
b*c*e*f*g*i*k + b*c*e*f*g*j*k + b*c*e*f*i*j*k + b*c*e*f + b*c*e*g*i*j*k +
b*c*e + b*c*f*g*i*j*k + b*c*f*g + b*c*f*i + b*c*f*j + b*c*f*k + b*c*f + b*c*g
+ b*c*i + b*c*j + b*c*k + b*e*f*g*i*j*k + b*e*f*g + b*e*f*i + b*e*f*j +
b*e*f*k + b*e*f + b*e*g + b*e*i + b*e*j + b*e*k + b*f*g*i + b*f*g*j + b*f*g*k
+ b*f*g + b*f*i*j + b*f*i*k + b*f*i + b*f*j*k + b*f*j + b*f*k + b*g*i + b*g*j
+ b*g*k + b*i*j + b*i*k + b*j*k + c*e*f*g*i*j*k + c*e*f*g + c*e*f*i + c*e*f*j
+ c*e*f*k + c*e*f + c*e*g + c*e*i + c*e*j + c*e*k + c*f*g*i + c*f*g*j +
c*f*g*k + c*f*g + c*f*i*j + c*f*i*k + c*f*i + c*f*j*k + c*f*j + c*f*k + c*g*i
+ c*g*j + c*g*k + c*i*j + c*i*k + c*j*k + e*f*g*i + e*f*g*j + e*f*g*k + e*f*g
+ e*f*i*j + e*f*i*k + e*f*i + e*f*j*k + e*f*j + e*f*k + e*g*i + e*g*j + e*g*k
+ e*i*j + e*i*k + e*j*k + f*g*i*j + f*g*i*k + f*g*i + f*g*j*k + f*g*j + f*g*k
+ f*i*j*k + f*i*j + f*i*k + f*j*k + g*i*j + g*i*k + g*j*k + i*j*k,
b*c*d*f*g*h*j*k + b*c*d*f*g*h*j*l + b*c*d*f*g*h*j + b*c*d*f*g*h*k*l +
b*c*d*f*g*h*k + b*c*d*f*g*h*l + b*c*d*f*g*j*k*l + b*c*d*f*g*j*k +
b*c*d*f*g*j*l + b*c*d*f*g*k*l + b*c*d*f*h*j*k + b*c*d*f*h*j*l + b*c*d*f*h*k*l
+ b*c*d*f*j*k*l + b*c*d*g*h*j*k*l + b*c*d*g*h*j*k + b*c*d*g*h*j*l +
b*c*d*g*h*k*l + b*c*d*g*j*k*l + b*c*d*g + b*c*d*h*j*k*l + b*c*d +
b*c*f*g*h*j*k*l + b*c*f*g*h*j*k + b*c*f*g*h*j*l + b*c*f*g*h*k*l +
b*c*f*g*j*k*l + b*c*f*g + b*c*f*h*j*k*l + b*c*f + b*c*g*h*j*k*l + b*c*g*h +
b*c*g*j + b*c*g*k + b*c*g*l + b*c*g + b*c*h + b*c*j + b*c*k + b*c*l +
b*d*f*g*h*j*k*l + b*d*f*g*h*j*k + b*d*f*g*h*j*l + b*d*f*g*h*k*l +
b*d*f*g*j*k*l + b*d*f*g + b*d*f*h*j*k*l + b*d*f + b*d*g*h*j*k*l + b*d*g*h +
b*d*g*j + b*d*g*k + b*d*g*l + b*d*g + b*d*h + b*d*j + b*d*k + b*d*l +
b*f*g*h*j*k*l + b*f*g*h + b*f*g*j + b*f*g*k + b*f*g*l + b*f*g + b*f*h + b*f*j
+ b*f*k + b*f*l + b*g*h*j + b*g*h*k + b*g*h*l + b*g*h + b*g*j*k + b*g*j*l +
b*g*j + b*g*k*l + b*g*k + b*g*l + b*h*j + b*h*k + b*h*l + b*j*k + b*j*l +
b*k*l + c*d*f*g*h*j*k*l + c*d*f*g*h*j*k + c*d*f*g*h*j*l + c*d*f*g*h*k*l +
c*d*f*g*j*k*l + c*d*f*g + c*d*f*h*j*k*l + c*d*f + c*d*g*h*j*k*l + c*d*g*h +
c*d*g*j + c*d*g*k + c*d*g*l + c*d*g + c*d*h + c*d*j + c*d*k + c*d*l +
c*f*g*h*j*k*l + c*f*g*h + c*f*g*j + c*f*g*k + c*f*g*l + c*f*g + c*f*h + c*f*j
+ c*f*k + c*f*l + c*g*h*j + c*g*h*k + c*g*h*l + c*g*h + c*g*j*k + c*g*j*l +
c*g*j + c*g*k*l + c*g*k + c*g*l + c*h*j + c*h*k + c*h*l + c*j*k + c*j*l +
c*k*l + d*f*g*h*j*k*l + d*f*g*h + d*f*g*j + d*f*g*k + d*f*g*l + d*f*g + d*f*h
+ d*f*j + d*f*k + d*f*l + d*g*h*j + d*g*h*k + d*g*h*l + d*g*h + d*g*j*k +
d*g*j*l + d*g*j + d*g*k*l + d*g*k + d*g*l + d*h*j + d*h*k + d*h*l + d*j*k +
d*j*l + d*k*l + f*g*h*j + f*g*h*k + f*g*h*l + f*g*h + f*g*j*k + f*g*j*l +
f*g*j + f*g*k*l + f*g*k + f*g*l + f*h*j + f*h*k + f*h*l + f*j*k + f*j*l +
f*k*l + g*h*j*k + g*h*j*l + g*h*j + g*h*k*l + g*h*k + g*h*l + g*j*k*l + g*j*k
+ g*j*l + g*k*l + h*j*k + h*j*l + h*k*l + j*k*l + 1, a*c*d*e*g*h*i*k +
a*c*d*e*g*h*i*l + a*c*d*e*g*h*i + a*c*d*e*g*h*k*l + a*c*d*e*g*h*k +
a*c*d*e*g*h*l + a*c*d*e*g*i*k + a*c*d*e*g*i*l + a*c*d*e*g*k*l +
a*c*d*e*h*i*k*l + a*c*d*e*h*i*k + a*c*d*e*h*i*l + a*c*d*e*h*k*l +
a*c*d*e*i*k*l + a*c*d*g*h*i*k*l + a*c*d*g*h*i*k + a*c*d*g*h*i*l +
a*c*d*g*h*k*l + a*c*d*g*i*k*l + a*c*d*h*i*k*l + a*c*d*h + a*c*d +
a*c*e*g*h*i*k*l + a*c*e*g*h*i*k + a*c*e*g*h*i*l + a*c*e*g*h*k*l +
a*c*e*g*i*k*l + a*c*e*h*i*k*l + a*c*e*h + a*c*e + a*c*g*h*i*k*l + a*c*g*h +
a*c*g + a*c*h*i + a*c*h*k + a*c*h*l + a*c*h + a*c*i + a*c*k + a*c*l +
a*d*e*g*h*i*k*l + a*d*e*g*h*i*k + a*d*e*g*h*i*l + a*d*e*g*h*k*l +
a*d*e*g*i*k*l + a*d*e*h*i*k*l + a*d*e*h + a*d*e + a*d*g*h*i*k*l + a*d*g*h +
a*d*g + a*d*h*i + a*d*h*k + a*d*h*l + a*d*h + a*d*i + a*d*k + a*d*l +
a*e*g*h*i*k*l + a*e*g*h + a*e*g + a*e*h*i + a*e*h*k + a*e*h*l + a*e*h + a*e*i
+ a*e*k + a*e*l + a*g*h*i + a*g*h*k + a*g*h*l + a*g*h + a*g*i + a*g*k + a*g*l
+ a*h*i*k + a*h*i*l + a*h*i + a*h*k*l + a*h*k + a*h*l + a*i*k + a*i*l + a*k*l
+ c*d*e*g*h*i*k*l + c*d*e*g*h*i*k + c*d*e*g*h*i*l + c*d*e*g*h*k*l +
c*d*e*g*i*k*l + c*d*e*h*i*k*l + c*d*e*h + c*d*e + c*d*g*h*i*k*l + c*d*g*h +
c*d*g + c*d*h*i + c*d*h*k + c*d*h*l + c*d*h + c*d*i + c*d*k + c*d*l +
c*e*g*h*i*k*l + c*e*g*h + c*e*g + c*e*h*i + c*e*h*k + c*e*h*l + c*e*h + c*e*i
+ c*e*k + c*e*l + c*g*h*i + c*g*h*k + c*g*h*l + c*g*h + c*g*i + c*g*k + c*g*l
+ c*h*i*k + c*h*i*l + c*h*i + c*h*k*l + c*h*k + c*h*l + c*i*k + c*i*l + c*k*l
+ d*e*g*h*i*k*l + d*e*g*h + d*e*g + d*e*h*i + d*e*h*k + d*e*h*l + d*e*h +
d*e*i + d*e*k + d*e*l + d*g*h*i + d*g*h*k + d*g*h*l + d*g*h + d*g*i + d*g*k +
d*g*l + d*h*i*k + d*h*i*l + d*h*i + d*h*k*l + d*h*k + d*h*l + d*i*k + d*i*l +
d*k*l + e*g*h*i + e*g*h*k + e*g*h*l + e*g*h + e*g*i + e*g*k + e*g*l + e*h*i*k
+ e*h*i*l + e*h*i + e*h*k*l + e*h*k + e*h*l + e*i*k + e*i*l + e*k*l + g*h*i*k
+ g*h*i*l + g*h*i + g*h*k*l + g*h*k + g*h*l + g*i*k + g*i*l + g*k*l + h*i*k*l
+ h*i*k + h*i*l + h*k*l + i*k*l + 1, f*g*h*j*k*l*n*o + f*g*h*j*k*l*n*p +
f*g*h*j*k*l*n + f*g*h*j*k*l*o*p + f*g*h*j*k*l*o + f*g*h*j*k*l*p +
f*g*h*j*k*n*o*p + f*g*h*j*k*n*o + f*g*h*j*k*n*p + f*g*h*j*k*o*p +
f*g*h*j*l*n*o + f*g*h*j*l*n*p + f*g*h*j*l*o*p + f*g*h*j*n*o*p +
f*g*h*k*l*n*o*p + f*g*h*k*l*n*o + f*g*h*k*l*n*p + f*g*h*k*l*o*p +
f*g*h*k*n*o*p + f*g*h*k + f*g*h*l*n*o*p + f*g*h + f*g*j*k*l*n*o*p +
f*g*j*k*l*n*o + f*g*j*k*l*n*p + f*g*j*k*l*o*p + f*g*j*k*n*o*p + f*g*j*k +
f*g*j*l*n*o*p + f*g*j + f*g*k*l*n*o*p + f*g*k*l + f*g*k*n + f*g*k*o + f*g*k*p
+ f*g*k + f*g*l + f*g*n + f*g*o + f*g*p + f*h*j*k*l*n*o*p + f*h*j*k*l*n*o +
f*h*j*k*l*n*p + f*h*j*k*l*o*p + f*h*j*k*n*o*p + f*h*j*k + f*h*j*l*n*o*p +
f*h*j + f*h*k*l*n*o*p + f*h*k*l + f*h*k*n + f*h*k*o + f*h*k*p + f*h*k + f*h*l
+ f*h*n + f*h*o + f*h*p + f*j*k*l*n*o*p + f*j*k*l + f*j*k*n + f*j*k*o +
f*j*k*p + f*j*k + f*j*l + f*j*n + f*j*o + f*j*p + f*k*l*n + f*k*l*o + f*k*l*p
+ f*k*l + f*k*n*o + f*k*n*p + f*k*n + f*k*o*p + f*k*o + f*k*p + f*l*n + f*l*o
+ f*l*p + f*n*o + f*n*p + f*o*p + g*h*j*k*l*n*o*p + g*h*j*k*l*n*o +
g*h*j*k*l*n*p + g*h*j*k*l*o*p + g*h*j*k*n*o*p + g*h*j*k + g*h*j*l*n*o*p +
g*h*j + g*h*k*l*n*o*p + g*h*k*l + g*h*k*n + g*h*k*o + g*h*k*p + g*h*k + g*h*l
+ g*h*n + g*h*o + g*h*p + g*j*k*l*n*o*p + g*j*k*l + g*j*k*n + g*j*k*o +
g*j*k*p + g*j*k + g*j*l + g*j*n + g*j*o + g*j*p + g*k*l*n + g*k*l*o + g*k*l*p
+ g*k*l + g*k*n*o + g*k*n*p + g*k*n + g*k*o*p + g*k*o + g*k*p + g*l*n + g*l*o
+ g*l*p + g*n*o + g*n*p + g*o*p + h*j*k*l*n*o*p + h*j*k*l + h*j*k*n + h*j*k*o
+ h*j*k*p + h*j*k + h*j*l + h*j*n + h*j*o + h*j*p + h*k*l*n + h*k*l*o +
h*k*l*p + h*k*l + h*k*n*o + h*k*n*p + h*k*n + h*k*o*p + h*k*o + h*k*p + h*l*n
+ h*l*o + h*l*p + h*n*o + h*n*p + h*o*p + j*k*l*n + j*k*l*o + j*k*l*p + j*k*l
+ j*k*n*o + j*k*n*p + j*k*n + j*k*o*p + j*k*o + j*k*p + j*l*n + j*l*o + j*l*p
+ j*n*o + j*n*p + j*o*p + k*l*n*o + k*l*n*p + k*l*n + k*l*o*p + k*l*o + k*l*p
+ k*n*o*p + k*n*o + k*n*p + k*o*p + l*n*o + l*n*p + l*o*p + n*o*p + 1,
e*g*h*i*k*l*m*o + e*g*h*i*k*l*m*p + e*g*h*i*k*l*m + e*g*h*i*k*l*o*p +
e*g*h*i*k*l*o + e*g*h*i*k*l*p + e*g*h*i*k*m*o + e*g*h*i*k*m*p + e*g*h*i*k*o*p
+ e*g*h*i*l*m*o*p + e*g*h*i*l*m*o + e*g*h*i*l*m*p + e*g*h*i*l*o*p +
e*g*h*i*m*o*p + e*g*h*k*l*m*o*p + e*g*h*k*l*m*o + e*g*h*k*l*m*p +
e*g*h*k*l*o*p + e*g*h*k*m*o*p + e*g*h*l*m*o*p + e*g*h*l + e*g*h +
e*g*i*k*l*m*o*p + e*g*i*k*l*m*o + e*g*i*k*l*m*p + e*g*i*k*l*o*p +
e*g*i*k*m*o*p + e*g*i*l*m*o*p + e*g*i*l + e*g*i + e*g*k*l*m*o*p + e*g*k*l +
e*g*k + e*g*l*m + e*g*l*o + e*g*l*p + e*g*l + e*g*m + e*g*o + e*g*p +
e*h*i*k*l*m*o*p + e*h*i*k*l*m*o + e*h*i*k*l*m*p + e*h*i*k*l*o*p +
e*h*i*k*m*o*p + e*h*i*l*m*o*p + e*h*i*l + e*h*i + e*h*k*l*m*o*p + e*h*k*l +
e*h*k + e*h*l*m + e*h*l*o + e*h*l*p + e*h*l + e*h*m + e*h*o + e*h*p +
e*i*k*l*m*o*p + e*i*k*l + e*i*k + e*i*l*m + e*i*l*o + e*i*l*p + e*i*l + e*i*m
+ e*i*o + e*i*p + e*k*l*m + e*k*l*o + e*k*l*p + e*k*l + e*k*m + e*k*o + e*k*p
+ e*l*m*o + e*l*m*p + e*l*m + e*l*o*p + e*l*o + e*l*p + e*m*o + e*m*p + e*o*p
+ g*h*i*k*l*m*o*p + g*h*i*k*l*m*o + g*h*i*k*l*m*p + g*h*i*k*l*o*p +
g*h*i*k*m*o*p + g*h*i*l*m*o*p + g*h*i*l + g*h*i + g*h*k*l*m*o*p + g*h*k*l +
g*h*k + g*h*l*m + g*h*l*o + g*h*l*p + g*h*l + g*h*m + g*h*o + g*h*p +
g*i*k*l*m*o*p + g*i*k*l + g*i*k + g*i*l*m + g*i*l*o + g*i*l*p + g*i*l + g*i*m
+ g*i*o + g*i*p + g*k*l*m + g*k*l*o + g*k*l*p + g*k*l + g*k*m + g*k*o + g*k*p
+ g*l*m*o + g*l*m*p + g*l*m + g*l*o*p + g*l*o + g*l*p + g*m*o + g*m*p + g*o*p
+ h*i*k*l*m*o*p + h*i*k*l + h*i*k + h*i*l*m + h*i*l*o + h*i*l*p + h*i*l +
h*i*m + h*i*o + h*i*p + h*k*l*m + h*k*l*o + h*k*l*p + h*k*l + h*k*m + h*k*o +
h*k*p + h*l*m*o + h*l*m*p + h*l*m + h*l*o*p + h*l*o + h*l*p + h*m*o + h*m*p +
h*o*p + i*k*l*m + i*k*l*o + i*k*l*p + i*k*l + i*k*m + i*k*o + i*k*p + i*l*m*o
+ i*l*m*p + i*l*m + i*l*o*p + i*l*o + i*l*p + i*m*o + i*m*p + i*o*p + k*l*m*o
+ k*l*m*p + k*l*m + k*l*o*p + k*l*o + k*l*p + k*m*o + k*m*p + k*o*p + l*m*o*p
+ l*m*o + l*m*p + l*o*p + m*o*p + 1, e*f*h*i*j*l*m*n + e*f*h*i*j*l*m*p +
e*f*h*i*j*l*m + e*f*h*i*j*l*n*p + e*f*h*i*j*l*n + e*f*h*i*j*l*p +
e*f*h*i*j*m*n*p + e*f*h*i*j*m*n + e*f*h*i*j*m*p + e*f*h*i*j*n*p +
e*f*h*i*l*m*n*p + e*f*h*i*l*m*n + e*f*h*i*l*m*p + e*f*h*i*l*n*p +
e*f*h*i*m*n*p + e*f*h*i + e*f*h*j*l*m*n + e*f*h*j*l*m*p + e*f*h*j*l*n*p +
e*f*h*j*m*n*p + e*f*h*l*m*n*p + e*f*h + e*f*i*j*l*m*n*p + e*f*i*j*l*m*n +
e*f*i*j*l*m*p + e*f*i*j*l*n*p + e*f*i*j*m*n*p + e*f*i*j + e*f*i*l*m*n*p +
e*f*i*l + e*f*i*m + e*f*i*n + e*f*i*p + e*f*i + e*f*j*l*m*n*p + e*f*j + e*f*l
+ e*f*m + e*f*n + e*f*p + e*h*i*j*l*m*n*p + e*h*i*j*l*m*n + e*h*i*j*l*m*p +
e*h*i*j*l*n*p + e*h*i*j*m*n*p + e*h*i*j + e*h*i*l*m*n*p + e*h*i*l + e*h*i*m +
e*h*i*n + e*h*i*p + e*h*i + e*h*j*l*m*n*p + e*h*j + e*h*l + e*h*m + e*h*n +
e*h*p + e*i*j*l*m*n*p + e*i*j*l + e*i*j*m + e*i*j*n + e*i*j*p + e*i*j +
e*i*l*m + e*i*l*n + e*i*l*p + e*i*l + e*i*m*n + e*i*m*p + e*i*m + e*i*n*p +
e*i*n + e*i*p + e*j*l + e*j*m + e*j*n + e*j*p + e*l*m + e*l*n + e*l*p + e*m*n
+ e*m*p + e*n*p + f*h*i*j*l*m*n*p + f*h*i*j*l*m*n + f*h*i*j*l*m*p +
f*h*i*j*l*n*p + f*h*i*j*m*n*p + f*h*i*j + f*h*i*l*m*n*p + f*h*i*l + f*h*i*m +
f*h*i*n + f*h*i*p + f*h*i + f*h*j*l*m*n*p + f*h*j + f*h*l + f*h*m + f*h*n +
f*h*p + f*i*j*l*m*n*p + f*i*j*l + f*i*j*m + f*i*j*n + f*i*j*p + f*i*j +
f*i*l*m + f*i*l*n + f*i*l*p + f*i*l + f*i*m*n + f*i*m*p + f*i*m + f*i*n*p +
f*i*n + f*i*p + f*j*l + f*j*m + f*j*n + f*j*p + f*l*m + f*l*n + f*l*p + f*m*n
+ f*m*p + f*n*p + h*i*j*l*m*n*p + h*i*j*l + h*i*j*m + h*i*j*n + h*i*j*p +
h*i*j + h*i*l*m + h*i*l*n + h*i*l*p + h*i*l + h*i*m*n + h*i*m*p + h*i*m +
h*i*n*p + h*i*n + h*i*p + h*j*l + h*j*m + h*j*n + h*j*p + h*l*m + h*l*n +
h*l*p + h*m*n + h*m*p + h*n*p + i*j*l*m + i*j*l*n + i*j*l*p + i*j*l + i*j*m*n
+ i*j*m*p + i*j*m + i*j*n*p + i*j*n + i*j*p + i*l*m*n + i*l*m*p + i*l*m +
i*l*n*p + i*l*n + i*l*p + i*m*n*p + i*m*n + i*m*p + i*n*p + j*l*m + j*l*n +
j*l*p + j*m*n + j*m*p + j*n*p + l*m*n + l*m*p + l*n*p + m*n*p, e*f*g*i*j*k*m*n
+ e*f*g*i*j*k*m*o + e*f*g*i*j*k*m + e*f*g*i*j*k*n*o + e*f*g*i*j*k*n +
e*f*g*i*j*k*o + e*f*g*i*j*m*n*o + e*f*g*i*j*m*n + e*f*g*i*j*m*o +
e*f*g*i*j*n*o + e*f*g*i*k*m*n + e*f*g*i*k*m*o + e*f*g*i*k*n*o + e*f*g*i*m*n*o
+ e*f*g*j*k*m*n*o + e*f*g*j*k*m*n + e*f*g*j*k*m*o + e*f*g*j*k*n*o +
e*f*g*j*m*n*o + e*f*g*j + e*f*g*k*m*n*o + e*f*g + e*f*i*j*k*m*n*o +
e*f*i*j*k*m*n + e*f*i*j*k*m*o + e*f*i*j*k*n*o + e*f*i*j*m*n*o + e*f*i*j +
e*f*i*k*m*n*o + e*f*i + e*f*j*k*m*n*o + e*f*j*k + e*f*j*m + e*f*j*n + e*f*j*o
+ e*f*j + e*f*k + e*f*m + e*f*n + e*f*o + e*g*i*j*k*m*n*o + e*g*i*j*k*m*n +
e*g*i*j*k*m*o + e*g*i*j*k*n*o + e*g*i*j*m*n*o + e*g*i*j + e*g*i*k*m*n*o +
e*g*i + e*g*j*k*m*n*o + e*g*j*k + e*g*j*m + e*g*j*n + e*g*j*o + e*g*j + e*g*k
+ e*g*m + e*g*n + e*g*o + e*i*j*k*m*n*o + e*i*j*k + e*i*j*m + e*i*j*n +
e*i*j*o + e*i*j + e*i*k + e*i*m + e*i*n + e*i*o + e*j*k*m + e*j*k*n + e*j*k*o
+ e*j*k + e*j*m*n + e*j*m*o + e*j*m + e*j*n*o + e*j*n + e*j*o + e*k*m + e*k*n
+ e*k*o + e*m*n + e*m*o + e*n*o + f*g*i*j*k*m*n*o + f*g*i*j*k*m*n +
f*g*i*j*k*m*o + f*g*i*j*k*n*o + f*g*i*j*m*n*o + f*g*i*j + f*g*i*k*m*n*o +
f*g*i + f*g*j*k*m*n*o + f*g*j*k + f*g*j*m + f*g*j*n + f*g*j*o + f*g*j + f*g*k
+ f*g*m + f*g*n + f*g*o + f*i*j*k*m*n*o + f*i*j*k + f*i*j*m + f*i*j*n +
f*i*j*o + f*i*j + f*i*k + f*i*m + f*i*n + f*i*o + f*j*k*m + f*j*k*n + f*j*k*o
+ f*j*k + f*j*m*n + f*j*m*o + f*j*m + f*j*n*o + f*j*n + f*j*o + f*k*m + f*k*n
+ f*k*o + f*m*n + f*m*o + f*n*o + g*i*j*k*m*n*o + g*i*j*k + g*i*j*m + g*i*j*n
+ g*i*j*o + g*i*j + g*i*k + g*i*m + g*i*n + g*i*o + g*j*k*m + g*j*k*n +
g*j*k*o + g*j*k + g*j*m*n + g*j*m*o + g*j*m + g*j*n*o + g*j*n + g*j*o + g*k*m
+ g*k*n + g*k*o + g*m*n + g*m*o + g*n*o + i*j*k*m + i*j*k*n + i*j*k*o + i*j*k
+ i*j*m*n + i*j*m*o + i*j*m + i*j*n*o + i*j*n + i*j*o + i*k*m + i*k*n + i*k*o
+ i*m*n + i*m*o + i*n*o + j*k*m*n + j*k*m*o + j*k*m + j*k*n*o + j*k*n + j*k*o
+ j*m*n*o + j*m*n + j*m*o + j*n*o + k*m*n + k*m*o + k*n*o + m*n*o,
a*b*d*i*j*l*m*n + a*b*d*i*j*l*m*p + a*b*d*i*j*l*m + a*b*d*i*j*l*n +
a*b*d*i*j*l*p + a*b*d*i*j*m*n*p + a*b*d*i*j*m*n + a*b*d*i*j*m*p +
a*b*d*i*j*n*p + a*b*d*i*l*m*n*p + a*b*d*i*l*m*n + a*b*d*i*l*m*p +
a*b*d*i*l*n*p + a*b*d*i*m*n*p + a*b*d*j*l*m*n*p + a*b*d*j*l*m*n +
a*b*d*j*l*m*p + a*b*d*j*l*n*p + a*b*d*j*m*n*p + a*b*d*l*m*n*p + a*b*d*m +
a*b*d + a*b*i*j*l*m*n*p + a*b*i*j*l*m*n + a*b*i*j*l*m*p + a*b*i*j*l*n*p +
a*b*i*j*m*n*p + a*b*i*l*m*n*p + a*b*i*m + a*b*i + a*b*j*l*m*n*p + a*b*j*m +
a*b*j + a*b*l*m + a*b*l + a*b*m*n + a*b*m*p + a*b*m + a*b*n + a*b*p +
a*d*i*j*l*m*n*p + a*d*i*j*l*m*n + a*d*i*j*l*m*p + a*d*i*j*l*n*p +
a*d*i*j*m*n*p + a*d*i*l*m*n*p + a*d*i*m + a*d*i + a*d*j*l*m*n*p + a*d*j*m +
a*d*j + a*d*l*m + a*d*l + a*d*m*n + a*d*m*p + a*d*m + a*d*n + a*d*p +
a*i*j*l*m*n*p + a*i*j*m + a*i*j + a*i*l*m + a*i*l + a*i*m*n + a*i*m*p + a*i*m
+ a*i*n + a*i*p + a*j*l*m + a*j*l + a*j*m*n + a*j*m*p + a*j*m + a*j*n + a*j*p
+ a*l*m*n + a*l*m*p + a*l*m + a*l*n + a*l*p + a*m*n*p + a*m*n + a*m*p + a*n*p
+ b*d*i*j*l*m*n*p + b*d*i*j*l*m*n + b*d*i*j*l*m*p + b*d*i*j*l*n*p +
b*d*i*j*m*n*p + b*d*i*l*m*n*p + b*d*i*m + b*d*i + b*d*j*l*m*n*p + b*d*j*m +
b*d*j + b*d*l*m + b*d*l + b*d*m*n + b*d*m*p + b*d*m + b*d*n + b*d*p +
b*i*j*l*m*n*p + b*i*j*m + b*i*j + b*i*l*m + b*i*l + b*i*m*n + b*i*m*p + b*i*m
+ b*i*n + b*i*p + b*j*l*m + b*j*l + b*j*m*n + b*j*m*p + b*j*m + b*j*n + b*j*p
+ b*l*m*n + b*l*m*p + b*l*m + b*l*n + b*l*p + b*m*n*p + b*m*n + b*m*p + b*n*p
+ d*i*j*l*m*n*p + d*i*j*m + d*i*j + d*i*l*m + d*i*l + d*i*m*n + d*i*m*p +
d*i*m + d*i*n + d*i*p + d*j*l*m + d*j*l + d*j*m*n + d*j*m*p + d*j*m + d*j*n +
d*j*p + d*l*m*n + d*l*m*p + d*l*m + d*l*n + d*l*p + d*m*n*p + d*m*n + d*m*p +
d*n*p + i*j*l*m + i*j*l + i*j*m*n + i*j*m*p + i*j*m + i*j*n + i*j*p + i*l*m*n
+ i*l*m*p + i*l*m + i*l*n + i*l*p + i*m*n*p + i*m*n + i*m*p + i*n*p + j*l*m*n
+ j*l*m*p + j*l*m + j*l*n + j*l*p + j*m*n*p + j*m*n + j*m*p + j*n*p + l*m*n*p
+ l*m*n + l*m*p + l*n*p + m*n*p, a*b*c*i*j*k*m*n + a*b*c*i*j*k*m +
a*b*c*i*j*k*n*o + a*b*c*i*j*k*n + a*b*c*i*j*k*o + a*b*c*i*j*m*n*o +
a*b*c*i*j*m*n + a*b*c*i*j*m*o + a*b*c*i*j*n*o + a*b*c*i*k*m*n*o +
a*b*c*i*k*m*n + a*b*c*i*k*m*o + a*b*c*i*k*n*o + a*b*c*i*m*n*o +
a*b*c*j*k*m*n*o + a*b*c*j*k*m*n + a*b*c*j*k*m*o + a*b*c*j*k*n*o +
a*b*c*j*m*n*o + a*b*c*k*m*n*o + a*b*c*n + a*b*c + a*b*i*j*k*m*n*o +
a*b*i*j*k*m*n + a*b*i*j*k*m*o + a*b*i*j*k*n*o + a*b*i*j*m*n*o + a*b*i*k*m*n*o
+ a*b*i*n + a*b*i + a*b*j*k*m*n*o + a*b*j*n + a*b*j + a*b*k*n + a*b*k +
a*b*m*n + a*b*m + a*b*n*o + a*b*n + a*b*o + a*c*i*j*k*m*n*o + a*c*i*j*k*m*n +
a*c*i*j*k*m*o + a*c*i*j*k*n*o + a*c*i*j*m*n*o + a*c*i*k*m*n*o + a*c*i*n +
a*c*i + a*c*j*k*m*n*o + a*c*j*n + a*c*j + a*c*k*n + a*c*k + a*c*m*n + a*c*m +
a*c*n*o + a*c*n + a*c*o + a*i*j*k*m*n*o + a*i*j*n + a*i*j + a*i*k*n + a*i*k +
a*i*m*n + a*i*m + a*i*n*o + a*i*n + a*i*o + a*j*k*n + a*j*k + a*j*m*n + a*j*m
+ a*j*n*o + a*j*n + a*j*o + a*k*m*n + a*k*m + a*k*n*o + a*k*n + a*k*o +
a*m*n*o + a*m*n + a*m*o + a*n*o + b*c*i*j*k*m*n*o + b*c*i*j*k*m*n +
b*c*i*j*k*m*o + b*c*i*j*k*n*o + b*c*i*j*m*n*o + b*c*i*k*m*n*o + b*c*i*n +
b*c*i + b*c*j*k*m*n*o + b*c*j*n + b*c*j + b*c*k*n + b*c*k + b*c*m*n + b*c*m +
b*c*n*o + b*c*n + b*c*o + b*i*j*k*m*n*o + b*i*j*n + b*i*j + b*i*k*n + b*i*k +
b*i*m*n + b*i*m + b*i*n*o + b*i*n + b*i*o + b*j*k*n + b*j*k + b*j*m*n + b*j*m
+ b*j*n*o + b*j*n + b*j*o + b*k*m*n + b*k*m + b*k*n*o + b*k*n + b*k*o +
b*m*n*o + b*m*n + b*m*o + b*n*o + c*i*j*k*m*n*o + c*i*j*n + c*i*j + c*i*k*n +
c*i*k + c*i*m*n + c*i*m + c*i*n*o + c*i*n + c*i*o + c*j*k*n + c*j*k + c*j*m*n
+ c*j*m + c*j*n*o + c*j*n + c*j*o + c*k*m*n + c*k*m + c*k*n*o + c*k*n + c*k*o
+ c*m*n*o + c*m*n + c*m*o + c*n*o + i*j*k*n + i*j*k + i*j*m*n + i*j*m +
i*j*n*o + i*j*n + i*j*o + i*k*m*n + i*k*m + i*k*n*o + i*k*n + i*k*o + i*m*n*o
+ i*m*n + i*m*o + i*n*o + j*k*m*n + j*k*m + j*k*n*o + j*k*n + j*k*o + j*m*n*o
+ j*m*n + j*m*o + j*n*o + k*m*n*o + k*m*n + k*m*o + k*n*o + m*n*o,
b*c*d*j*k*l*n*o + b*c*d*j*k*l*n + b*c*d*j*k*l*o*p + b*c*d*j*k*l*o +
b*c*d*j*k*l*p + b*c*d*j*k*n*o*p + b*c*d*j*k*n*o + b*c*d*j*k*n*p +
b*c*d*j*k*o*p + b*c*d*j*l*n*o*p + b*c*d*j*l*n*o + b*c*d*j*l*n*p +
b*c*d*j*l*o*p + b*c*d*j*n*o*p + b*c*d*k*l*n*o*p + b*c*d*k*l*n*o +
b*c*d*k*l*n*p + b*c*d*k*l*o*p + b*c*d*k*n*o*p + b*c*d*l*n*o*p + b*c*d*o +
b*c*d + b*c*j*k*l*n*o*p + b*c*j*k*l*n*o + b*c*j*k*l*n*p + b*c*j*k*l*o*p +
b*c*j*k*n*o*p + b*c*j*l*n*o*p + b*c*j*o + b*c*j + b*c*k*l*n*o*p + b*c*k*o +
b*c*k + b*c*l*o + b*c*l + b*c*n*o + b*c*n + b*c*o*p + b*c*o + b*c*p +
b*d*j*k*l*n*o*p + b*d*j*k*l*n*o + b*d*j*k*l*n*p + b*d*j*k*l*o*p +
b*d*j*k*n*o*p + b*d*j*l*n*o*p + b*d*j*o + b*d*j + b*d*k*l*n*o*p + b*d*k*o +
b*d*k + b*d*l*o + b*d*l + b*d*n*o + b*d*n + b*d*o*p + b*d*o + b*d*p +
b*j*k*l*n*o*p + b*j*k*o + b*j*k + b*j*l*o + b*j*l + b*j*n*o + b*j*n + b*j*o*p
+ b*j*o + b*j*p + b*k*l*o + b*k*l + b*k*n*o + b*k*n + b*k*o*p + b*k*o + b*k*p
+ b*l*n*o + b*l*n + b*l*o*p + b*l*o + b*l*p + b*n*o*p + b*n*o + b*n*p + b*o*p
+ c*d*j*k*l*n*o*p + c*d*j*k*l*n*o + c*d*j*k*l*n*p + c*d*j*k*l*o*p +
c*d*j*k*n*o*p + c*d*j*l*n*o*p + c*d*j*o + c*d*j + c*d*k*l*n*o*p + c*d*k*o +
c*d*k + c*d*l*o + c*d*l + c*d*n*o + c*d*n + c*d*o*p + c*d*o + c*d*p +
c*j*k*l*n*o*p + c*j*k*o + c*j*k + c*j*l*o + c*j*l + c*j*n*o + c*j*n + c*j*o*p
+ c*j*o + c*j*p + c*k*l*o + c*k*l + c*k*n*o + c*k*n + c*k*o*p + c*k*o + c*k*p
+ c*l*n*o + c*l*n + c*l*o*p + c*l*o + c*l*p + c*n*o*p + c*n*o + c*n*p + c*o*p
+ d*j*k*l*n*o*p + d*j*k*o + d*j*k + d*j*l*o + d*j*l + d*j*n*o + d*j*n +
d*j*o*p + d*j*o + d*j*p + d*k*l*o + d*k*l + d*k*n*o + d*k*n + d*k*o*p + d*k*o
+ d*k*p + d*l*n*o + d*l*n + d*l*o*p + d*l*o + d*l*p + d*n*o*p + d*n*o + d*n*p
+ d*o*p + j*k*l*o + j*k*l + j*k*n*o + j*k*n + j*k*o*p + j*k*o + j*k*p +
j*l*n*o + j*l*n + j*l*o*p + j*l*o + j*l*p + j*n*o*p + j*n*o + j*n*p + j*o*p +
k*l*n*o + k*l*n + k*l*o*p + k*l*o + k*l*p + k*n*o*p + k*n*o + k*n*p + k*o*p +
l*n*o*p + l*n*o + l*n*p + l*o*p + n*o*p, a*c*d*i*k*l*m*p + a*c*d*i*k*l*m +
a*c*d*i*k*l*o*p + a*c*d*i*k*l*o + a*c*d*i*k*l*p + a*c*d*i*k*m*o*p +
a*c*d*i*k*m*o + a*c*d*i*k*m*p + a*c*d*i*k*o*p + a*c*d*i*l*m*o*p +
a*c*d*i*l*m*o + a*c*d*i*l*m*p + a*c*d*i*l*o*p + a*c*d*i*m*o*p +
a*c*d*k*l*m*o*p + a*c*d*k*l*m*o + a*c*d*k*l*m*p + a*c*d*k*l*o*p +
a*c*d*k*m*o*p + a*c*d*l*m*o*p + a*c*d*p + a*c*d + a*c*i*k*l*m*o*p +
a*c*i*k*l*m*o + a*c*i*k*l*m*p + a*c*i*k*l*o*p + a*c*i*k*m*o*p + a*c*i*l*m*o*p
+ a*c*i*p + a*c*i + a*c*k*l*m*o*p + a*c*k*p + a*c*k + a*c*l*p + a*c*l +
a*c*m*p + a*c*m + a*c*o*p + a*c*o + a*c*p + a*d*i*k*l*m*o*p + a*d*i*k*l*m*o +
a*d*i*k*l*m*p + a*d*i*k*l*o*p + a*d*i*k*m*o*p + a*d*i*l*m*o*p + a*d*i*p +
a*d*i + a*d*k*l*m*o*p + a*d*k*p + a*d*k + a*d*l*p + a*d*l + a*d*m*p + a*d*m +
a*d*o*p + a*d*o + a*d*p + a*i*k*l*m*o*p + a*i*k*p + a*i*k + a*i*l*p + a*i*l +
a*i*m*p + a*i*m + a*i*o*p + a*i*o + a*i*p + a*k*l*p + a*k*l + a*k*m*p + a*k*m
+ a*k*o*p + a*k*o + a*k*p + a*l*m*p + a*l*m + a*l*o*p + a*l*o + a*l*p +
a*m*o*p + a*m*o + a*m*p + a*o*p + c*d*i*k*l*m*o*p + c*d*i*k*l*m*o +
c*d*i*k*l*m*p + c*d*i*k*l*o*p + c*d*i*k*m*o*p + c*d*i*l*m*o*p + c*d*i*p +
c*d*i + c*d*k*l*m*o*p + c*d*k*p + c*d*k + c*d*l*p + c*d*l + c*d*m*p + c*d*m +
c*d*o*p + c*d*o + c*d*p + c*i*k*l*m*o*p + c*i*k*p + c*i*k + c*i*l*p + c*i*l +
c*i*m*p + c*i*m + c*i*o*p + c*i*o + c*i*p + c*k*l*p + c*k*l + c*k*m*p + c*k*m
+ c*k*o*p + c*k*o + c*k*p + c*l*m*p + c*l*m + c*l*o*p + c*l*o + c*l*p +
c*m*o*p + c*m*o + c*m*p + c*o*p + d*i*k*l*m*o*p + d*i*k*p + d*i*k + d*i*l*p +
d*i*l + d*i*m*p + d*i*m + d*i*o*p + d*i*o + d*i*p + d*k*l*p + d*k*l + d*k*m*p
+ d*k*m + d*k*o*p + d*k*o + d*k*p + d*l*m*p + d*l*m + d*l*o*p + d*l*o + d*l*p
+ d*m*o*p + d*m*o + d*m*p + d*o*p + i*k*l*p + i*k*l + i*k*m*p + i*k*m +
i*k*o*p + i*k*o + i*k*p + i*l*m*p + i*l*m + i*l*o*p + i*l*o + i*l*p + i*m*o*p
+ i*m*o + i*m*p + i*o*p + k*l*m*p + k*l*m + k*l*o*p + k*l*o + k*l*p + k*m*o*p
+ k*m*o + k*m*p + k*o*p + l*m*o*p + l*m*o + l*m*p + l*o*p + m*o*p}

I = ideal L
time C = gens gb( I, Algorithm=>Sugarless); -- this takes 29.3027 seconds, without Sugarless it takes currently takes 4 days or so, at least according to Franzi...!
time B = gbBoolean I; --  -- used 506.507 seconds Revision: 10953
-- Revision: 10961  -- used 249.015 seconds

-- we computed 987516 S Polynomials and added 1078 of them to the intermediate basis.
-- Revision: 10970 -- used 204.691 seconds


assert( sort gens B - sort C == 0 )
