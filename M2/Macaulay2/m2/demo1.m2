f := x -> (
     a := "hi there";
     b := 1/x;
     b+1)

g = y -> (
     c := f(y-1);
     d := f(y-2);
     c+d)
