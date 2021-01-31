R=QQ[s,t] 
S=QQ[x,y,z,w]
assert( (f = map(R,S,{s^4,s^3*t,s*t^3,t^4})) === map(R,S,{s^4, s^3*t, s*t^3, t^4}) )
assert( (I = kernel f) === ideal(y*z-x*w,z^3-y*w^2,x*z^2-y^2*w,y^3-x^2*z) )
assert( (E3 = Ext^3(coker gens I, S)) === cokernel map(S^{{5}},S^{{4},{4},{4},{4}},{{w, z, y, x}}) )
assert( (hilbertFunction(-5,E3)) === 1 )


end


print generateAssertions ///
R=QQ[s,t] 
S=QQ[x,y,z,w]
f = map(R,S,{s^4,s^3*t,s*t^3,t^4})
I = kernel f
E3 = Ext^3(coker gens I, S)
hilbertFunction(-5,E3)
///
