kk=ZZ/101
R=kk[a,b,c,d]
i=ideal{c*a-b^2,d^2-a*b+c*b}
p1=ideal{b,c,d}
p2=ideal{d,c-a,b-a}
p3=ideal{d,c-a,b+a}
mingens(p1+i)
mingens(p2+i)
mingens(p3+i)
h=(ideal{d+c}+i:p1)
A=intersect(h,p2)
B=intersect(h,p3)
betti A
betti B
Ri=R/i
LA=lift(syz mingens substitute(A,Ri),R)
LB=lift(syz mingens substitute(B,Ri),R)
E=kk[x_0..x_3,SkewCommutative=>true]
symExt= (m,R) ->(
ev := map(R,ring m,vars R);
mt := transpose jacobian m;
jn := gens kernel mt;
q  := vars(ring m)**id_(target m);
ev(q*jn))
RA=symExt(LA,E)
RB=symExt(LB,E)
betti(fA=res coker transpose syz RA)
betti(fB=res coker transpose syz RB)
P=kk[p_1..p_6]**kk[y_0..y_3]
pluecker=ideal{y_0*y_1-p_1,y_0*y_2-p_2,y_0*y_3-p_3,y_1*y_2-p_4,y_1*y_3-p_5,y_2*y_3-p_6}
plgb=gb pluecker
MA=substitute(fA.dd_3,matrix{{y_0..y_3}})
try MA % plgb						    -- this line crashed before
     	       	    	      	   	     	       	    -- now it just gives an error
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test bug.out"
-- End:
