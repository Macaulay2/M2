-- Tests using Gorenstein ideals
-- 

gor = (n,d) -> (
     R = ZZ/101[vars(0..n-1), MonomialSize=>8];
     F = random(R^1, R^{-d});
     ideal fromDual F
     )

makeGor = (n,d)-> (
     I = gor(n,d);
     "gor" | n | "'" | d | "= () -> (\n" |
     "  R = " | toExternalString R | ";\n  F = " |
       toExternalString F_(0,0) | ";\n  I = " |
       replace(",",",\n    ",toExternalString I) | ";)\n"
     )

makeGorFile = (filename) -> (
     fil := openOut filename;
     fil << makeGor(5,3) << endl;
     fil << makeGor(6,3) << endl;
     fil << makeGor(7,3) << endl;     
     fil << makeGor(8,3) << endl;
     fil << makeGor(9,3) << endl;
     fil << makeGor(10,3) << endl;
     fil << makeGor(11,3) << endl;
     fil << makeGor(5,4) << endl;
     fil << makeGor(6,4) << endl;
     fil << makeGor(7,4) << endl;     
     fil << makeGor(8,4) << endl;
     fil << makeGor(9,4) << endl;
     fil << makeGor(10,4) << endl;
     fil << makeGor(11,4) << endl;
     fil << makeGor(5,5) << endl;
     fil << makeGor(6,5) << endl;
     fil << makeGor(7,5) << endl;     
     fil << makeGor(8,5) << endl;
     fil << makeGor(9,5) << endl;
     fil << makeGor(10,5) << endl;
     fil << makeGor(11,5) << endl;
     close fil
     )

end
load "goren.m2"
makeGor(5,3)
makeGorFile "gor-tests.m2"


time load "gor-tests.m2"
gor6'5()
time res I
gor7'5()
time res(I, Strategy=>1)
time res(I, Strategy=>0, SortStrategy=>2^16)  -- 140.7 sec, after fixing respoly2 stash: 79.75 sec
gor7'5()

time res I -- 89.06 sec

time res(I, Strategy=>0, SortStrategy=>2^13+2^18)  -- 
