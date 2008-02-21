-- BUG#1, minors: here's something I first noticed in 0.9.95. 

R = QQ[a_1..a_6,x,y,z]
I = ideal(a_1+a_2+a_3+a_4+a_5+a_6,
     a_2*x-a_2*y+a_3*x-a_3*z-a_4*y-a_5*z+a_6*x-a_6*y-a_6*z,
     a_3*x*y-a_3*x*z-a_3*y*z+a_3*z^2-a_5*y*z+a_5*z^2-a_6*x*z+a_6*z^2)

I = ideal(a_1+a_2+a_3+a_4+a_5+a_6,
     a_2*x-a_2*y+a_3*x-a_3*z-a_4*y-a_5*z+a_6*x-a_6*y-a_6*z)

-- this ideal has lots of symmetry.  Here's a linear change of variables:

S = ring I
I' = sub(I,{x=>z,z=>x,S_0=>S_2,S_2=>S_0,S_3=>S_5,S_5=>S_3})
assert(I==I') -- equal, yep

-- here's a bug?  Fitting ideal of jacobian loses this symmetry:

m = minors(3,jacobian I); 
m' = minors(3, jacobian I');
assert(m == m')  -- not equal, yikes!

-- decomposition shows lack of symmetry:

time scan(decompose m, i -> print i)

-------------------------------------------
-- BUG#2, a quickie, also present in 0.9.95

R = QQ[x,y,z]
L := apply(4,i->exteriorPower(i,R^3))
L_1**L_1
basis oo  -- crash! 


end
phi = map(S,S,{x=>z,z=>x,S_0=>S_2,S_2=>S_0,S_3=>S_5,S_5=>S_3})
phi o10


-- Another bug from Graham Denham
R = ZZ[e_1..e_9, SkewCommutative=>true]
I = ideal(-e_1+e_2+e_4,-e_1-e_2+e_5,-e_1+e_3+e_6,-e_1-e_3+e_7,-e_2+e_3+e_8,-e_2-e_3+e_9)
gbTrace=3
gb I -- crashes in both 0.9.95 and 1.0.9test
transpose gens oo
assert (gens gb I == matrix {{e_6-e_7-e_8+e_9, e_5-e_7-e_8, e_4-e_7+e_9, 2*e_3+e_8-e_9, e_2+e_3-e_9, e_1+e_3-e_7, e_3*e_8+e_3*e_9+e_8*e_9}})
