--------------------------------------------
-- This Document is Reserved for Examples --
--------------------------------------------

-- output should be:

-- alg dependent vars, ideal, map

          p       s
I < k[x] <= k[y] <- k[t]
            J<
	    
we take I we currently return p^-1, we want p,s,J
don't compute the inverse asking for it. 





-- Examples should be listed in a resonable order
-- Comments should be given about why each example is good

--========================================================

--Examples:
clearAll
installPackage "NoetherNormalization"
methods noetherNormalization

--Ex#1
-- this is the example from the paper
-- this makes it a good first example
R = QQ[x_4,x_3,x_2,x_1, MonomialOrder => Lex]; --the same ordering as in the paper
I = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1);
noetherNormalization(I)


--Examples of not so good I
--Ex#2
R = QQ[x_5,x_4,x_3,x_2,x_1,MonomialOrder => Lex]
I = ideal(x_1^3 + x_1*x_2, x_2^3-x_4+x_3, x_1^2*x_2+x_1*x_2^2)
noetherNormalization I


--Ex#3
R = QQ[x_1,x_2,x_3,MonomialOrder => Lex]
I = ideal(x_1*x_2,x_1*x_3)
noetherNormalization(I)

--Ex#4
R = QQ[x_3,x_2,x_1,MonomialOrder => Lex]
I = ideal(x_1*x_2, x_1*x_3)
G = gb I
gens G
d = dim I
X = sort gens R -- note that this "sort" is very important
varPrep(X,G)
np = maxAlgPerm(R,X,G,d)
maxAlgPermC(R,X,G,d)
maxAlgPermB(R,X,G,d,{})


--Ex#5
R = QQ[x_1..x_6,MonomialOrder => Lex]
R = QQ[x_6,x_5,x_4,x_3,x_2,x_1,MonomialOrder => Lex]
I = ideal(x_1*x_2,x_1*x_3, x_2*x_3,x_2*x_4,x_2*x_5,x_3*x_4,x_3*x_5,x_4*x_5, x_4*x_6, x_5*x_6)
G = gb I
d = dim I
X = sort gens R -- note that this "sort" is very important
varPrep(X,G)
np = maxAlgPerm(R,X,G,d)
G = gb np I
(U,V) = varPrep(X,G)
noetherNormalization I
x_5<x_4



-- we need more complex examples.