cat = (firstvar,rows,cols) -> (
     R := ring firstvar;
     x := index firstvar;
     map(R^(#rows), #cols, (i,j) -> R_(x+rows_i+cols_j)))

diag = (m) -> (
     m1 := flatten m;
     n := numgens source m1;
     R := ring m1;
     map(R^n, n, (i,j) -> if i === j then n_(0,i) else 0))


     