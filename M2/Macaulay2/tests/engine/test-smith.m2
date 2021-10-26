--status: this old test depends on internal things and probably should be deleted


needs "raw-util.m2"
-- need getRing for mutable matrices
reduceRowsCols = (p,i,j) -> (
     -- assumption: p_(i,j) is a unit.
     -- remove all elements in the same row or column as (i,j).
     nrows := rawNumberOfRows p;
     ncols := rawNumberOfColumns p;
     pivotelem := rawMatrixEntry(p,i,j);
     for c from 0 to ncols-1 do
         if c =!= j then (
	     a := rawMatrixEntry(p,i,c);
	     if a =!= 0 then
	       rawMatrixColumnChange(p,c,-a//pivotelem,j,false);
	     );
     for r from 0 to nrows-1 do
         if r =!= i then (
	     a := rawMatrixEntry(p,r,j);
	     if a =!= 0 then
	       rawMatrixRowChange(p,r,-a//pivotelem,i,false);
	     );
     )
reduceColGCD = (p,i,j,col) -> (
     e := rawToInteger rawMatrixEntry(p,i,col);
     pivotelem := rawToInteger rawMatrixEntry(p,i,j);
     if e =!= 0 then (
	  g := gcd(e,pivotelem);
	  a := (-pivotelem)//g;
	  b := e//g;
	  (GCD,c,d) := toSequence gcdCoefficients(e,pivotelem);
	  (a,b,c,d) = (raw (a_ZZ), raw(b_ZZ), raw(c_ZZ), raw(d_ZZ));
	  rawMatrixColumnOperation2(p,col,j,a,b,c,d,false);
     ))
reduceRowGCD = (p,i,j,row) -> (
     << "reduceRowGCD(" << i << ", " << j << ", " << row << ")" << endl;
     e := rawToInteger rawMatrixEntry(p,row,j);
     pivotelem := rawToInteger rawMatrixEntry(p,i,j);
     if e =!= 0 then (
	  g := gcd(e,pivotelem);
	  a := (-pivotelem)//g;
	  b := e//g;
	  (GCD,c,d) := toSequence gcdCoefficients(e,pivotelem);
	  (a,b,c,d) = (raw (a_ZZ), raw(b_ZZ), raw(c_ZZ), raw(d_ZZ));
	  rawMatrixRowOperation2(p,row,i,a,b,c,d,false);
     ))

reduceRowsColsGcd = (p,i,j) -> (
     -- assumption: p_(i,j) is non-zero?
     -- remove all elements in the same row or column as (i,j).
     -- replaces p_(i,j) with the gcd of all such elements
     nrows := rawNumberOfRows p;
     ncols := rawNumberOfColumns p;
     pivotelem := rawMatrixEntry(p,i,j);
     for c from 0 to ncols-1 do
         if c =!= j then reduceColGCD(p,i,j,c);
     for r from 0 to nrows-1 do
         if r =!= i then reduceRowGCD(p,i,j,r);
     )
