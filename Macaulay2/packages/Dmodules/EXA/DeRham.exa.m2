path = join ( path, {"../"})
load "Dloadfile.m2"

Dtrace 4

R = QQ[x,y,z]

-- Hypersurfaces
f = x+y+z     	   -- linear form
g = x^3-y^2*z^2	   -- cusp
h = x^3+y^3+z^3    -- fermat cubic

-- Cohomology of the complements
deRham(f)
deRham(g)
deRham(h)

-- Explicit information
deRhamAll(f)
deRhamAll(g)
deRhamAll(h)

--Boundary cases
deRham(0_R) -- empty set
deRham(1_R) -- affine space
deRhamAll(0_R)
deRhamAll(1_R)
