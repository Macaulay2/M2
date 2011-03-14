-- from ~/local/my-projects/Bernd and Lior's book/assocmodel.m2"
-- 

R = ZZ/32003[a,b,c,d,e,f,g,h,i,j,k,l]
J = ideal"acf2-b2eg,
       bdg2-c2fh,
       acj2-b2ik,
       bdk2-c2jl,
       egj2-f2ik,
       fhk2-g2jl,
       af2i-be2j,
       ag2i-ce2k,
       bg2j-cf2k,
       ah2i-de2l,
       bh2j-df2l,
       ch2k-dg2l,
       bgi-cej,
       -agj+bek,
       chj-dfk,
       -bhk+cfl"

load "toric.m2" -- in my code directory
debug PrimaryDecomposition
L = saturators J
L1 = minSatPPD(J, L)
L1#2
M = trim(J + ideal(L1#2))
codim M
J == intersect(L1#0,M)
L1#0

time assJ = apply(6..10, i -> (L := time decompose ann Ext^i(coker gens J, ring J); select(L, p -> codim p === i)))
assJ = join assJ
time E5 = ann Ext^5(coker gens J, ring J);
E5rest = E5 : L1#0;
codim E5rest
degree E5rest
transpose gens E5rest
primaryDecomposition E5rest
ass5 = ass E5rest
allAssJ = join({L1#0}, ass5, assJ)

matrix toList apply(0..#allAssJ-1, i -> toList apply(0..#allAssJ-1, j -> if isSubset(allAssJ_i, allAssJ_j) then -1 else 0))


-- radical of J?
P = ideal apply(flatten entries gens gb J, f -> product factors f)
P = trim P
transpose gens gb P
gens gb oo
restart
R = ZZ/32003 [y_(1,1), y_(1,2), y_(1,3), y_(1,4), y_(2,1), y_(2,2), y_(2,3), y_(2,4), y_(3,1), y_(3,2), y_(3,3), y_(3,4)]
J = ideal (y_(1,1)*y_(1,3)*y_(2,2)^2-y_(1,2)^2*y_(2,1)*y_(2,3),
     y_(1,2)*y_(1,4)*y_(2,3)^2-y_(1,3)^2*y_(2,2)*y_(2,4),
     y_(1,1)*y_(1,3)*y_(3,2)^2-y_(1,2)^2*y_(3,1)*y_(3,3),
     y_(1,2)*y_(1,4)*y_(3,3)^2-y_(1,3)^2*y_(3,2)*y_(3,4),
     y_(2,1)*y_(2,3)*y_(3,2)^2-y_(2,2)^2*y_(3,1)*y_(3,3),
     y_(2,2)*y_(2,4)*y_(3,3)^2-y_(2,3)^2*y_(3,2)*y_(3,4),
     y_(1,1)*y_(2,2)^2*y_(3,1)-y_(1,2)*y_(2,1)^2*y_(3,2),
     y_(1,1)*y_(2,3)^2*y_(3,1)-y_(1,3)*y_(2,1)^2*y_(3,3),
     y_(1,2)*y_(2,3)^2*y_(3,2)-y_(1,3)*y_(2,2)^2*y_(3,3),
     y_(1,1)*y_(2,4)^2*y_(3,1)-y_(1,4)*y_(2,1)^2*y_(3,4),
     y_(1,2)*y_(2,4)^2*y_(3,2)-y_(1,4)*y_(2,2)^2*y_(3,4),
     y_(1,3)*y_(2,4)^2*y_(3,3)-y_(1,4)*y_(2,3)^2*y_(3,4),
     y_(1,2)*y_(2,3)*y_(3,1)-y_(1,3)*y_(2,1)*y_(3,2),
     -y_(1,1)*y_(2,3)*y_(3,2)+y_(1,2)*y_(2,1)*y_(3,3),
     y_(1,3)*y_(2,4)*y_(3,2)-y_(1,4)*y_(2,2)*y_(3,3),
     -y_(1,2)*y_(2,4)*y_(3,3)+y_(1,3)*y_(2,2)*y_(3,4)
     )

R = ZZ/32003[a,b,c,d,e,f,g,h,i,j,k,l]
J = ideal (a*c*f^2-b^2*e*g,
       b*d*g^2-c^2*f*h,
       a*c*j^2-b^2*i*k,
       b*d*k^2-c^2*j*l,
       e*g*j^2-f^2*i*k,
       f*h*k^2-g^2*j*l,
       a*f^2*i-b*e^2*j,
       a*g^2*i-c*e^2*k,
       b*g^2*j-c*f^2*k,
       a*h^2*i-d*e^2*l,
       b*h^2*j-d*f^2*l,
       c*h^2*k-d*g^2*l,
       b*g*i-c*e*j,
       -a*g*j+b*e*k,
       c*h*j-d*f*k,
       -b*h*k+c*f*l)

J = ideal"acf2-b2eg,
       bdg2-c2fh,
       acj2-b2ik,
       bdk2-c2jl,
       egj2-f2ik,
       fhk2-g2jl,
       af2i-be2j,
       ag2i-ce2k,
       bg2j-cf2k,
       ah2i-de2l,
       bh2j-df2l,
       ch2k-dg2l,
       bgi-cej,
       -agj+bek,
       chj-dfk,
       -bhk+cfl"

R = ZZ/32003[vars(0..11)]
J = substitute(J,vars R)
printWidth=0
toExternalString J
-- Problem: compute the primary decomposition of this ideal
S = "ideal (a*c*f^2-b^2*e*g,b*d*g^2-c^2*f*h,a*c*j^2-b^2*i*k,b*d*k^2-c^2*j*l,e*g*j^2-f^2*i*k,f*h*k^2-g^2*j*l,a*f^2*i-b*e^2*j,a*g^2*i-c*e^2*k,b*g^2*j-c*f^2*k,a*h^2*i-d*e^2*l,b*h^2*j-d*f^2*l,c*h^2*k-d*g^2*l,b*g*i-c*e*j,-a*g*j+b*e*k,c*h*j-d*f*k,-b*h*k+c*f*l)"
T = replace(",",",\n       ",S)
S1 = replace("\\*","",S)
S2 = replace("\\^","",S1)
S3 = replace(",",",\n       ",S2)
print oo
print T
end
codim J
degree J
load "toric.m2" -- in my code directory
debug PrimaryDecomposition
L = saturators J
L1 = minSatPPD(J, L)
L1#2
M = trim(J + ideal(L1#2))
codim M
J == intersect(L1#0,M)

res M
M = saturate M
res M

-- The following is the set of associated primes of J, of codim >= 6
assJ = apply(6..10, i -> decompose ann Ext^i(coker gens M, ring M))

Jsat = sat J;
Jrest = J : Jsat;
J1 = intersect(Jrest,Jsat);
J1 == J -- no...
betti J
betti Jsat
betti Jrest
transpose gens Jrest

time A = ass J;
