--		Copyright 1994 by Daniel R. Grayson

table = (rows,cols,f) -> apply(rows,i->apply(cols,j->f(i,j)))

applyTable = (m,f) -> apply(m, v -> apply(v,f))

subtable = (u,v,m) -> table(u, v, (i,j)->m_i_j)

isTable = m -> (
     class m === List and
     #m > 0 and
     all(m, row -> class row === List and #row === #m#0))

transpose List := m -> (
     if isTable m
     then pack(mingle m,# m)
     else if # m === 0
          then {}
	  else error ("expected ", toString m, " to be a table"))


