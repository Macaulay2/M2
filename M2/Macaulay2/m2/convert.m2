--		Copyright 1993-1999 by Daniel R. Grayson

ConvertInteger
ConvertMissing
ConvertApply = x -> x			  -- (f,T1,...,Tm)
ConvertJoin = x -> if class x === Sequence then prepend (identity, x) else (identity, x)
ConvertRepeat = T -> singleton T
ConvertFixedRepeat = x -> x		  -- (n,T1,...,Tm)
ConvertList  = T -> (toList, ConvertRepeat T)


