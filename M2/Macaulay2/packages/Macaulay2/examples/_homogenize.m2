R = ZZ/101[x,y,z,Degrees => {1,2,3}]
f = 1 + y + z^2
homogenize(f,x)
homogenize(f,x,{1,0,-1})
