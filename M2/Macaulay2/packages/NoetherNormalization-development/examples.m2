--------------------------------------------
-- This Document is Reserved for Examples --
--------------------------------------------


--PUT SOME EXAMPLES INTO THE PACKAGE

viewHelp use


--Examples:
clearAll
uninstallPackage "NoetherNormalization"
installPackage "NoetherNormalization"
loadPackage "NoetherNormalization"
methods noetherNormalization
help NoetherNormalization
help noetherNormalization

viewHelp


-- Joe's example

clearAll
installPackage "NoetherNormalization"
loadPackage "NoetherNormalization"
n = 2;
A = QQ[x_(1,1)..x_(n,n),y_(1,1)..y_(n,n),MonomialOrder => Lex]
X = transpose genericMatrix(A,n,n)
Y = transpose genericMatrix(A,y_(1,1),n,n)
bracket = ideal flatten entries (X*Y - Y*X)
f = map(A,A,toList apply(0..(2*n^2-1), i -> sum(gens A)_{0..i}))
f bracket

viewHelp sum


(f,J,j) = noetherNormalization(bracket,Verbose=>true,LimitSequence => {5,10})
transpose gens J


--Example 1
clearAll
A = QQ[x_1..x_3][x_4]
A = QQ[x_1..x_4]
I = ideal(x_1^2 + x_1*x_4+1,x_1*x_2*x_3*x_4+1)
(f,J,j) = noetherNormalization(I,Verbose=>true,LimitList => {5})
A/f I

A = QQ
I = promote(ideal 0,QQ)
noetherNormalization(I,Verbose=>true)



transpose gens gb J
()
ring I
ring x_1
ring x_4

--Example 2
R = QQ[x_2,x_3,x_4,x_1,x_5,MonomialOrder=>Lex] -- this is a nice example...
R = QQ[x_1..x_5, MonomialOrder => Lex]; -- this is a nice example...
I = ideal(x_2*x_1-x_5^3, x_5*x_1^3);              -- compare with the same example in singular. 
(f,J,j) = noetherNormalization (I,Verbose => true)
transpose gens gb J


-- Not inverse maps!
S_2*S_1 -- Not inverse maps!
S_4*S_3
S_3*S_4
S_5*S_6
S_6*S_5
S_3*S_5*S_6*S_4


--Example 2.5
R = QQ[x_1..x_5,MonomialOrder=>Lex] -- this is a nice example...
I = ideal(x_2*x_1-x_5^3, x_5*x_1^3)              -- compare with the same example in singular. 
noetherNormalization(I,Verbose => true)
f = noetherPosition(I)
transpose gens gb f I
dim I
sort gens R
gens R





--Example 3 -- this one the indep vars are different
R = QQ[x_5,x_4,x_3,x_2,x_1]
I = ideal(x_1^3 + x_1*x_2, x_2^3-x_4+x_3, x_1^2*x_2+x_1*x_2^2)
noetherNormalization(I,Verbose => true)
f := (noetherNormalization(I))_1


support (independentSets(I,Limit=>1))_0

--Example 4
R = QQ[x_1,x_2,x_3]
I = ideal(x_1*x_2,x_1*x_3)
noetherNormalization(I,Verbose => true)
support (independentSets(I,Limit=>1))_0
X = (noetherNormalization(I,Verbose => true))_0
f = (noetherNormalization(I,Verbose => true))_1
R/f(I)
X
apply(X, i-> f i)



--Example 5
R := QQ[x_5,x_4,x_3,x_2,x_1, MonomialOrder => Lex]
I = ideal(x_4^3*x_3*x_2-x_4, x_2*x_1-x_5^3, x_5*x_1^3)
S = noetherNormalization(I,Verbose => true)
f = S_1
f x_2
x_2
describe ring x_2


--Example 6
R = QQ[x_1..x_5] --80
I = ideal(x_4^3*x_3*x_2-x_4, x_2*x_1-x_5^3, x_5*x_1^3)
noetherNormalization(I,Verbose => true)
support (independentSets(I,Limit=>1))_0


--Example 6.5
R = QQ[x_5,x_4,x_3,x_2,x_1] --20
I = ideal(x_4^3*x_3*x_2-x_4, x_2*x_1-x_5^3, x_5*x_1^3)
noetherNormalization(I,Verbose => true)
support (independentSets(I,Limit=>1))_0

--Example 7 Nat, check this one later. CANNOT DO in ALT NN
R = QQ[x_6,x_5,x_4,x_3,x_2,x_1];
I = ideal(x_6^2+x_5*x_3*x_4-2,x_4^4*x_3^2+x_1,x_2*x_1^3);
noetherNormalization(I,Verbose => true)
support (independentSets(I,Limit=>1))_0


--Example 8 -- kills m2 in AltNN! 
R = QQ[x_6,x_5,x_4,x_3,x_2,x_1];
I = ideal(x_6^3+x_5^2*x_3*x_4-2,x_4^4*x_3^2+x_1,x_2*x_1^3);
noetherNormalization(I,Verbose => true)
support (independentSets(I,Limit=>1))_0


--We cannot compute even the gb with this ordering
R = QQ[x_1..x_4];
I = ideal(-(3/2)*x_3^3*x_2-(4/5)*x_2^2+4*x_1^5-x_1,x_3^3*x_1-(5/8)*x_3^2*x_2*x_1^2+(2/5)*x_2+(8/3)*x_1^3)
noetherNormalization I
support (independentSets(I,Limit=>1))_0




-- output should be:

-- alg independent vars, ideal, map

          p       s
I < k[x] <= k[y] <- k[p^-1(U)]
            J<
	    
we take I we currently return p^-1, we want p,s,J --MIKE AGREES
don't compute the inverse asking for it. 

cache the inverse using something like

keys f.cache


-- Singular is better...

R = QQ[x_5,x_4,x_3,x_2,x_1,MonomialOrder => Lex] 
I = ideal(x_2*x_1-x_5^3, x_5*x_1^3)              
gens gb I
noetherNormalization I



R = QQ[x_5,x_4,x_3,x_2,x_1,MonomialOrder => Lex]
I = ideal(x_4^3*x_3*x_2-x_4, x_2*x_1-x_5^3, x_5*x_1^3)
gens gb I
noetherNormalization I
--this guys a problem, what to do?

R = QQ[x_1..x_4,MonomialOrder => Lex];
I = ideal((4/7)*x_3^2*x_4-(4/3)*x_4^2-(3/7)*x_3,(5/4)*x_2*x_4^2+(7/8)*x_1+(7/2),-(10/9)*x_1^2*x_4-(7/9)*x_2^2+(7/4)*x_4+(3/2))
noetherNormalization(I,Verbose => true)



-- Examples should be listed in a reasonable order
-- Comments should be given about why each example is good

--========================================================



--Examples:
clearAll
uninstallPackage "NoetherNormalization"
installPackage "NoetherNormalization"
methods noetherNormalization
help noetherNormalization

R = QQ[x_3,x_3,x_2,x_1, MonomialOrder => Lex];
I = ideal(-(3/2)*x_3^3*x_2-(4/5)*x_2^2+4*x_1^5-x_1,x_3^3*x_1-(5/8)*x_3^2*x_2*x_1^2+(2/5)*x_2+(8/3)*x_1^3)



--Ex#1
-- this is the example from the paper
-- this makes it a good first example
R = QQ[x_1..x_4,MonomialOrder => Lex];
R = QQ[x_4,x_3,x_2,x_1, MonomialOrder => Lex]; --the same ordering as in the paper
R = QQ[x_2,x_3,x_4,x_1, MonomialOrder => Lex];
I = ideal(x_2^2+x_1*x_2+1, x_1*x_2*x_3*x_4+1);
noetherNormalization I
I = ideal((6/5)*x_4*x_1-(8/7)*x_1^3-(9/4),(3/7)*x_4*x_3+(7/8)*x_3*x_2^2+x_1-(5/3),-(5/6)*x_4*x_2-(5/6)*x_3^2*x_1)
G = gb I
X = sort gens R -- note that this "sort" is very important
benchmark "varPrep(X,G)"
benchmark "support (independentSets(I,Limit => 1))_0"


--Examples of not so good I
--Ex#2
R = QQ[x_5,x_4,x_3,x_2,x_1,MonomialOrder => Lex]
I = ideal(x_1^3 + x_1*x_2, x_2^3-x_4+x_3, x_1^2*x_2+x_1*x_2^2)
noetherNormalization I
G = gb I
X = sort gens R -- note that this "sort" is very important
varPrep(X,G)
ZZ[x]
support (independentSets(ideal(x),Limit => 1))_0
independentSets(ideal(x))

dim(ZZ[x]/(7,x))
dim (ZZ[x]/ideal(7,x))

--Ex#3
R = QQ[x_1,x_2,x_3,MonomialOrder => Lex]
I = ideal(x_1*x_2,x_1*x_3)
noetherNormalization(I)
G = gb I
X = sort gens R -- note that this "sort" is very important
benchmark "varPrep(X,G)"
benchmark "support (independentSets(I,Limit => 1))_0"
benchmark "independentSets(I,Limit => 1)"
altVarPrep(X,I)

--Ex#4
R = QQ[x_3,x_2,x_1,MonomialOrder => Lex]
I = ideal(x_1*x_2, x_1*x_3)
G = gb I
X = sort gens R -- note that this "sort" is very important
varPrep(X,G)
independentSets I


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

R = ZZ/2[x,y]
I = ideal((x^2*y+x*y^2+1))
noetherNormalization I

-- we need more complex examples.

viewHelp


--Nat's examples

R = QQ[x_7,x_6,x_5,x_4,x_3,x_2,x_1, MonomialOrder => Lex];
I = ideal(x_2^2+x_1*x_2+x_5^2+1, x_1*x_2*x_3*x_4+x_5^4, x_6^4*x_3+x_4^2+8, x_7*x_6*x_5^2+x_5*x_2^2+12);
gens gb I
noetherNormalization I

