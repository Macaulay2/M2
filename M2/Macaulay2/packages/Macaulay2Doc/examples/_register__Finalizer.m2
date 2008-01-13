R = QQ[a..d];
makeGB = (n) -> (g := gb((ideal vars R)^4); registerFinalizer(g, "gb("|n|")"););
for i from 1 to 10 do (makeGB i);
collectGarbage()
