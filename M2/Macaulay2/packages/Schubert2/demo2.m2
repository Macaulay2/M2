-- Here are some more demos, not from the Schubert manual, some with Schubert code in comments

loadPackage "Schubert2"

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
X_* c18
-- integral(Gc,%);
integral oo
-- Ans =  2734099200
integral chern A
assert( oo == 2734099200 )

(Gc,p) = Grassmannian(3,5,{Sc,Qc});
(X,q) = Proj(dual symm_7 Qc,{K,L});
time B = symm(15,Qc) - symm(3,Qc) * dual L
time integral chern B
assert( 99992296084705144978200 == oo)
