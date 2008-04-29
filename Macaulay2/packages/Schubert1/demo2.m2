-- Here are some more demos based on examples from classic schubert, not from the Schubert manual, some with Schubert code in comments

loadPackage "Schubert1"

-- # of elliptic cubics on a sextic 4 fold, a schubert-classic example from Rahul:
-- grass(3,6,c):
grass(3,6,c)
-- B:=Symm(3,Qc):
B = symm_3 Qc
-- Proj(X,dual(B),z):
proj(X,dual B,z)
-- A:=Symm(6,Qc)-Symm(3,Qc)&@o(-z):
A = symm(6,Qc) - symm(3,Qc) * o(-z)
-- c18:=chern(rank(A),A):
c18 = chern(rank(A),A)
-- lowerstar(X,c18):
lowerstar(X,c18)
-- integral(Gc,%);
integral oo
-- Ans =  2734099200
integral chern A
assert( oo == 2734099200 )

--

grass(3,5,c)
proj(X,dual symm_7 Qc,z)
time B = symm(15,Qc) - symm(3,Qc) * o(-z)
time integral chern B
assert( 99992296084705144978200 == oo)
