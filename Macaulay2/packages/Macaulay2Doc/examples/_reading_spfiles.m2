fn = temporaryFileName()
fn << "z^6+3*x*z^4+6*y*z^4+3*x^2*z^2+12*x*y*z^2+12*y^2*z^2+x^3+6*x^2*y+12*x*y^2+8*y^3" << endl << close
get fn
R = ZZ/101[x,y,z]
f = value get fn
factor f
fn << "sample = 2^100
print sample
" << close
get fn
load fn
needs fn
input fn
peek get fn
lines get fn
peek lines get fn
stack lines get fn
removeFile fn
