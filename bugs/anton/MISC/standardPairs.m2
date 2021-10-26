restart
QQ[x,y,z]; standardPairs monomialIdeal(x*y^3*z, x*y^2*z^2)
  
end

help standardPairs
assert all(I_*, f->all(                                           
	standardPairs I
        , mx->(
	    (m,x) := toSequence mx;
            m' := f//m;                                                                                            
            R := ring m';
	    saturate(ideal m', sub(ideal product x,R)) != ideal 1_R
	    )))
    