-- This code appears to no longer be a bug (Sep 2010)

-- Use of "presentation Ring" in m2:
--  codim R -- this is doing too much work?  Or is the GB already computed for it?
--    same with hilbertSeries, char, 
--    poincare: uses presentation cokernel preentation M ??
--    trim -- seems OK?
--    newring, tensor
--    total Ext
--    unmixedradical in radical.m2 -- the noticed bug!


-- A bug noticed by David Eisenbud, 20 Dec 2006 (in an email)

unmixedradical = (I) -> (
     -- First lift I to a polynomial ring...
     << "doing unmixedradical with ideal = " << toExternalString I << endl;
     A := ring I;
     f := presentation A;
     B := ring f;
     I = lift(I,B);
     if I != ideal(1_B) and 
        I.generators =!= 0 
     then (
    	  c := codim I;
    	  size := 1;
	  R := A;
    	  while size <= c do (
	       << " size = " << size << " c = " << c << endl;
	       R = B/I;
	       dR := jacobian R;
      	       J := minors(size,dR);
     	       g1 := leadTerm gens gb presentation R;
	       g1 = g1 | lift(leadTerm J, B);

      	       if codim ideal g1 > c
	       then size = size+1
      	       else (
		    -- we would like the next line to read:
		    -- I = annihilator J;
		    I = ideal syz(transpose mingens J, 
		                  SyzygyRows=>1, Syzygies=>true);
		    I = lift(I,B); 
		    );
      	       );
	  );
     trim (I*A)
     )

radical1 = (I) -> (
    -- possibly massage input, by removing obvious extraneous powers?
    -- at least of the monomials in the ideal?
    << "doing radical1 with ideal = " << toExternalString I << endl;
    R := ring I;
    I1 := removeLowestDimension I;
    J := saturate(I, I1);
    J = unmixedradical J;
    if I1 == ideal(1_R)
        then J 
        else intersect(J, radical1 I1))

end
restart
load "1-eisenbud-radical.m2"

kk = ZZ/101
S=kk[vars(0..2*5)]
J1 = ideal"-be+af,-de+cf,-dg+ch,-bi+aj"
unmixedradical J1

radical J1

removeLowestDimension J1
unmixedradical J1

J = ideal"-de+cf,-bg+ah,-fg+eh,-bi+aj,-di+cj"
unmixedradical J
radical1 J
use S
J = ideal (d*i-c*j,b*i-a*j,f*g-e*h,b*g-a*h,d*e-c*f,b*h*j,a*h*j,a*g*j,d*f*j,c*f*j,c*e*j,b*d*j,a*d*j,a*c*j,a*g*i,c*e*i,a*c*i,d*f*h,c*f*h,b*f*h,a*f*h,c*e*h,a*e*h,c*e*g,a*e*g)
radical1 J
time ans = radical J
ans'radj = ideal"di-cj,bi-aj,fg-eh,bg-ah,de-cf,bhj,ahj,agj,dfj,cfj,cej,bdj,adj,acj,agi,cei,aci,dfh,cfh,bfh,afh,ceh,aeh,ceg,aeg"
assert (ans == ans'radj)

<ring 12 a-z r
<ideal j -be+af -de+cf -dg+ch -bi+aj
<ideal j -de+cf -bg+ah -fg+eh -bi+aj -di+cj
<radical j k
type k

di-cj bi-aj fg-eh bg-ah de-cf bhj ahj agj dfj cfj cej bdj adj acj agi cei aci dfh cfh bfh afh ceh aeh ceg aeg
----------------------------------
kk = ZZ/101
S=kk[vars(0..2*5)]
m=genericMatrix(S,2,5)
i=minors(2,m)
L=(subsets(9))_86
j=ideal ((gens i)*map(source gens i, S^{#L:-2},i_L));
radical j

-- The original prograom
kk = ZZ/101
S=kk[vars(0..2*5)]
m=genericMatrix(S,2,5)
i=minors(2,m)
count=-1
for L in  subsets(9) do(
count=count+1;
j=ideal ((gens i)*map(source gens i, S^{#L:-2},i_L));
print (count, L, j==radical j)
)
