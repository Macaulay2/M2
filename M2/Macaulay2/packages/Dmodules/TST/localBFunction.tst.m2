-- Copyright 1999-2009 by Anton Leykin and Harrison Tsai

needsPackage "Dmodules"
Dtrace 1
pInfo(1, "testing localBFunction...")

R = QQ[x,y]; f = x^2*(x+y+1); P = ideal(x,y);
b = localBFunction(f,P) 
assert(toString b == "s^2+(3/2)*s+1/2")
assert(localBFunction(f,ideal 0_R) == 1)
assert(toString localBFunction(f,ideal 1_R) == toString globalBFunction f)

