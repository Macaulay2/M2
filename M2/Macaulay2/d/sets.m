(* Mathematica version of sets implemented with tables *)

keys[f_] := Map[ Function[x, x[[1,1,1]]], DownValues[f] ]

values[f_] := Map[ Function[x, x[[2]]], DownValues[f] ]

elements = keys

set[x_] := Module[{set}, Scan[ Function[i, set[i]=True], x ]; set]

union[z___] := Module[{set}, 
		Scan[ Function[
			  x, 
		 	  Scan[Function[i, set[i]=True], keys[x]]], 
		      {z}]; 
		set]

try[n_] := Print[Timing[
		Module[{t,u},
		   Print[Timing[ t = set[Table[{i}, {i,1,2n}]] ]];
		   Print[Timing[ u = set[Table[{i}, {i,n,3n}]] ]];
		   Print[Timing[ Length[elements[union[t,u]]] ]];
		]]]
