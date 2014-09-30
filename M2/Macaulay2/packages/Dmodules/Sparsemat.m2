-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

debug Core

reduceCompress = method()
reduceCompress Matrix := (m) -> (
     R := ring m;
     msparse := rawMutableMatrix(raw m, true); -- true: make this a dense matrix...
     rawReduceByPivots msparse;
     mout := compress map(R,rawMatrix msparse);
     colCounter := numgens source mout - 1;
     rowCounter := numgens target mout - 1;
     --if (mout == id_(target mout)) then (mout = null)
     --else (
     while (rowCounter >= 0 and colCounter >= 0 and
	  mout_colCounter == (id_(target mout))_rowCounter) do (
	  colCounter = colCounter - 1;
	  rowCounter = rowCounter - 1;
	  );
     if (rank source mout == 0) then mout = mout
     else if (colCounter == -1 and rowCounter == -1) then mout = gens R^0
     else (
	  if (colCounter == -1) then colCounter = 0;
	  if (rowCounter == -1) then rowCounter = 0;
	  mout = compress mout_{0..colCounter}^{0..rowCounter};
	  );
     mout
     )
