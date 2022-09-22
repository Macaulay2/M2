-- Test whether two sets of ideals are the same
R = ZZ/32003[r,s,t,u,v,w,x,y,z]
I = ideal( r*v+s*u, r*w+t*u, s*w+t*v, r*y+s*x, r*z+t*x, s*z+t*y,
     u*y+v*x, u*z+w*x, v*z+w*y)
D = decompose I 
E = {
	      ideal(u,r,y,x,z,t*v+s*w),
	      ideal(z,x,y,w,u,v),
	      ideal(v,u,s,w,y,t*x+r*z),
	      ideal(v,s,y,x,z,t*u+r*w),
	      ideal(x,y,r,s,u,v),
	      ideal(v,u,r,w,x,t*y+s*z),
	      ideal(v,s,r,t,y,w*x+u*z),
	      ideal(z,x,y,t,r,s),
	      ideal(u,s,r,t,x,w*y+v*z),
	      ideal(z,x,w,t,r,u),
	      ideal(s,r,t,w,z,v*x+u*y),
	      ideal(v,u,t,w,z,s*x+r*y),
	      ideal(z,y,w,t,s,v),
	      ideal(w,t,r,s,u,v),
	      ideal(t,w,y,x,z,s*u+r*v)
	      }

-- It is not true that:  set D === set E
-- since these do not need to be the same sets, 
-- as they have different generating sets.

Ds = set apply(D, I -> trim I)
Es = set apply(E, I -> trim I)
assert(Ds === Es)

Ds = set apply(D, I -> gens gb I)
Es = set apply(E, I -> gens gb I)
assert(Ds === Es)
