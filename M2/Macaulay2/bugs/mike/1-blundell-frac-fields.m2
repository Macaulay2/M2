--First work in ZZ/3
R=GF(3)
--Invert 2
2_R^(-1)
--Now take 1/2
1_R/2
--Notice that there were no problems, so we try a bigger ring:
S=GF(3)[x,y]
--Invert 2
2_S^(-1)
--Now take 1/2
1_S/2
--Even though it could have stayed in S, it went to frac(S), but that's ok.
--It gets weird when we want to invert variables too:
T=frac(GF(3)[x])[y]
--Invert 2
2_T^(-1)
--Now take 1/2, but it's going to force an exit in the process
1_T/2
