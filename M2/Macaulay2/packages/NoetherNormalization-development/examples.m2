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
uninstallPackage "NoetherNormalization"
installPackage "NoetherNormalization"
methods noetherNormalization

--Nat's examples

R = QQ[x_7,x_6,x_5,x_4,x_3,x_2,x_1, MonomialOrder => Lex];
I = ideal(x_2^2+x_1*x_2+x_5^2+1, x_1*x_2*x_3*x_4+x_5^4, x_6^4*x_3+x_4^2+8, x_7*x_6*x_5^2+x_5*x_2^2+12);
gens gb I
noetherNormalization I






--Ex#1
-- this is the example from the paper
-- this makes it a good first example
R = QQ[x_4,x_3,x_2,x_1, MonomialOrder => Lex]; --the same ordering as in the paper
I = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1);





--Examples of not so good I
--Ex#2
R = QQ[x_5,x_4,x_3,x_2,x_1,MonomialOrder => Lex]
I = ideal(x_1^3 + x_1*x_2, x_2^3-x_4+x_3, x_1^2*x_2+x_1*x_2^2)
noetherNormalization I

R = QQ[x_5,x_4,x_3,x_2,x_1,MonomialOrder => Lex] -- this is a nice example...
I = ideal(x_2*x_1-x_5^3, x_5*x_1^3)              -- compare with the same example in singular. 
gens gb I
noetherNormalization I

R = QQ[x_5,x_4,x_3,x_2,x_1,MonomialOrder => Lex]
I = ideal(x_4^3*x_3*x_2-x_4, x_2*x_1-x_5^3, x_5*x_1^3)
gens gb I
noetherNormalization I




--Ex#3
R = QQ[x_1,x_2,x_3,MonomialOrder => Lex]
I = ideal(x_1*x_2,x_1*x_3)
noetherNormalization(I)

--Ex#4
R = QQ[x_3,x_2,x_1,MonomialOrder => Lex]
I = ideal(x_1*x_2, x_1*x_3)
G = gb I

prune ideal gens G
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

--Dan's finite field killing examples
xy(x+y)
(xy-1)(x+y)
x^2*y+x*y^2+1

R = ZZ/2[x,y, MonomialOrder => Lex];
I = ideal((x^2*y+x*y^2+1))
noetherNormalization I

-- we need more complex examples.



