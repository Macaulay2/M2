--		Copyright 1994 by Daniel R. Grayson

table = (rows,cols,f) -> apply(rows,i->apply(cols,j->f(i,j)))

document { quote table,
     TT "table(u,v,f)", " -- yields a table m in which m_i_j is f(u_i,v_j).",
     PARA,
     "A table is a list of lists, all of the same length.  The entry m_i_j is 
     computed as f(u_i,v_j).",
     PARA,
     "table(m,n,f) -- yields, when m and n are integers, a table of size m by n
     whose entries are obtained by evaluating f() repeatedly.",
     PARA,
     "See also ", TO "isTable", ", and ", TO "subtable", ".",
     }

applyTable = (m,f) -> apply(m, v -> apply(v,f))

document { quote applyTable,
     TT "applyTable(m,f)", " -- applies the function f to each element of the table m.",
     PARA,
     "It yields a table of the same shape as m containing the resulting values.",
     PARA,
     "See also ", TO "table", "."
     }

subtable = (u,v,m) -> table(u, v, (i,j)->m_i_j)

document { quote subtable,
     TT "subtable(u,v,m)", " -- yields the subtable of the table m obtained from the
     list u of row numbers and the list v of column numbers.",
     PARA,
     EXAMPLE "m = table(5,5,identity)",
     EXAMPLE "subtable({1,3,4},elements(2..4), m)"
     }

isTable = m -> (
     class m === List and
     #m > 0 and
     all(m, row -> class row === List and #row === #m#0))

document { quote isTable,
     TT "isTable m", " -- yields the value true if m is a rectangular matrix represented
     as a list of lists, otherwise yields the value false.  (Notice 
     that it is not possible to represent a 0-by-k matrix as a list
     of lists.)"
     }

transpose List := m -> (
     if isTable m
     then pack(mingle m,# m)
     else if # m === 0
          then {}
	  else error ("expected ", name m, " to be a table"))
document { quote transpose,
     TT "transpose m", " -- yields the transpose n of the table or homomorphism m."
     }

-----------------------------------------------------------------------------


