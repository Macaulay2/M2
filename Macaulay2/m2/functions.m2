--		Copyright 1994 by Daniel R. Grayson

Function @@ Function := (f,g) -> x -> f g x

ultimate = method()
ultimate(Function,Thing) := (f,x) -> (
     while try (ox := x; x = f x; ox =!= x) else false do ();
     x)

