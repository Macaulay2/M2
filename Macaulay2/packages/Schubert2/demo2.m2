-- Here are some more demos based on examples from classic schubert, not from the Schubert manual, some with Schubert code in comments

needsPackage "Schubert2"

pt = base()

-- # of elliptic cubics on a sextic 4 fold, a schubert-classic example from Rahul:
-- Enumerative Geometry of Calabi-Yau 4-Folds
-- Communications in Mathematical Physics
-- Volume 281, Number 3 / August, 2008
-- Pages 621-653
-- Enumerative Geometry of Calabi-Yau 4-Folds
-- A. Klemm and R. Pandharipande

-- grass(3,6,c):
Gc = flagBundle({3,3}, pt, VariableNames => {,c})
(Sc,Qc) = Gc.Bundles
-- B:=Symm(3,Qc):
B = symmetricPower_3 Qc
-- Proj(X,dual(B),z):
X = projectiveBundle'(dual B, VariableNames => {,{z}})
-- A:=Symm(6,Qc)-Symm(3,Qc)&@o(-z):
A = symmetricPower_6 Qc - symmetricPower_3 Qc ** OO(-z)
-- c18:=chern(rank(A),A):
c18 = chern(rank A,A)
-- lowerstar(X,c18):
X.StructureMap_* c18
-- integral(Gc,%);
integral oo
assert( oo == 2734099200 )
-- Ans =  2734099200
integral chern A
assert( oo == 2734099200 )
--

-- a similar example
-- in P^4 ... what does this count?
Gc = flagBundle({2,3}, pt, VariableNames => {,c})
(Sc,Qc) = Gc.Bundles
B = symmetricPower_7 Qc
X = projectiveBundle'(dual B, VariableNames => {,{z}})
A = symmetricPower_15 Qc - symmetricPower_3 Qc ** OO(-z)
integral chern A
assert( 99992296084705144978200 == oo)
