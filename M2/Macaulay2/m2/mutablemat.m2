--		Copyright 2005 by Daniel R. Grayson

MutableMatrix = new Type of HashTable
raw MutableMatrix := m -> m.RawMutableMatrix
ring MutableMatrix := m -> m.Ring
MutableMatrix == ZZ := (m,i) -> raw m == i
ZZ == MutableMatrix := (i,m) -> raw m == i
entries MutableMatrix := m -> (
     R := ring m;
     applyTable(entries raw m, r -> new R from r))
toString MutableMatrix := m -> "mutableMatrix " | toString entries m
net MutableMatrix := m -> (
     m = raw m;
     if m == 0 then return "0";
     stack toSequence apply(lines toString m, x -> concatenate("| ",x,"|")))
map(Ring,RawMutableMatrix) := opts -> (R,m) -> (
     new MutableMatrix from {
	  symbol Ring => R,
	  symbol RawMutableMatrix => m
	  }
     )
new Matrix from MutableMatrix := (typeofMatrix       ,m) -> map(ring m,rawMatrix        raw m)
new MutableMatrix from Matrix := (typeofMutableMatrix,m) -> map(ring m,rawMutableMatrix raw m)

mutableMatrix = method()
mutableMatrix Matrix := m -> new MutableMatrix from m
mutableMatrix List := m -> new MutableMatrix from matrix m

MutableMatrix _ Sequence := (m,rc) -> new ring m from (raw m)_rc
MutableMatrix == MutableMatrix := (m,n) -> raw m == raw n

setRowChange = method()
setRowChange(MutableMatrix,MutableMatrix) := (m,r) -> rawRowChange(raw m, raw r)
setRowChange(MutableMatrix,Matrix) := (m,r) -> rawRowChange(raw m, rawMutableMatrix raw r)

setColumnChange = method()
setColumnChange(MutableMatrix,MutableMatrix) := (m,c) -> rawColumnChange(raw m, raw c)
setColumnChange(MutableMatrix,Matrix) := (m,c) -> rawColumnChange(raw m, rawMutableMatrix raw c)

assgn := method()
assgn(RawMutableMatrix,ZZ,ZZ,RingElement) := (m,r,c,x) -> rawSetMatrixEntry(m,r,c,raw x)
assgn(RawMutableMatrix,ZZ,ZZ,ZZ) := (m,r,c,x) -> rawSetMatrixEntry(m,r,c,raw x)
assgn(RawMutableMatrix,ZZ,ZZ,QQ) := (m,r,c,x) -> rawSetMatrixEntry(m,r,c,raw x)
installAssignmentMethod(symbol _, MutableMatrix, Sequence, (m,rc,x) -> ((r,c) -> assgn(raw m,r,c,x)) rc)

rowSwap = method()
rowSwap(RawMutableMatrix,ZZ,ZZ) := (m,i,j) -> rawMatrixRowSwap(m,i,j)
rowSwap(MutableMatrix,ZZ,ZZ) := (m,i,j) -> rawMatrixRowSwap(raw m,i,j)

columnSwap = method()
columnSwap(RawMutableMatrix,ZZ,ZZ) := (m,i,j) -> rawMatrixColumnSwap(m,i,j)
columnSwap(MutableMatrix,ZZ,ZZ) := (m,i,j) -> rawMatrixColumnSwap(raw m,i,j)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
