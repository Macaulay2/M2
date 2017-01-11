restart
debug needsPackage "NumericalSchubertCalculus"

-- different ways to enter the problem {2,1} {1,1} {1} in Gr(2,5)
--
-- with wrong partitions 
Pblm = {
    ({2,1},random(FFF^5,FFF^5)),
    ({1,1}, random(FFF^5,FFF^5)),
    ({1,1}, random(FFF^5,FFF^5))
    };

verifyInput(Pblm, 2,5)
break

Pblm = {
    ({1,2},random(FFF^5,FFF^5)),
    ({1,1}, random(FFF^5,FFF^5)),
    ({1}, random(FFF^5,FFF^5))
    };

verifyInput(Pblm, 2,5)
break

--
-- with brackets (right ones) 

Pblm = {
    ({2, 4},random(FFF^5,FFF^5)),
    ({3, 4}, random(FFF^5,FFF^5)),
    ({3, 5}, random(FFF^5,FFF^5))
    };

verifyInput(Pblm, 2,5)

-- with Wrong Brackets:

Pblm = {
    ({2, 4},random(FFF^5,FFF^5)),
    ({4, 3}, random(FFF^5,FFF^5)),
    ({3, 5}, random(FFF^5,FFF^5))
    };

verifyInput(Pblm, 2,5)
break

--
Pblm = {
    ({2, 4},random(FFF^5,FFF^5)),
    ({3}, random(FFF^5,FFF^5)),
    ({3, 5}, random(FFF^5,FFF^5))
    };

verifyInput(Pblm, 2,5)
break


--- With Wrong Flags
Pblm = {
    ({2, 4},random(FFF^5,FFF^5)),
    ({3, 4}, random(FFF^2,FFF^5)),
    ({3, 5}, random(FFF^5,FFF^5))
    };

verifyInput(Pblm, 2,5)
break


F1 := random(FFF^5,FFF^3)
Pblm = {
    ({2, 4},random(FFF^5,FFF^5)),
    ({3, 4}, random(FFF^5,FFF^5)),
    ({3, 5}, 
	F1|F1_{0,2}
	)
    };

verifyInput(Pblm, 2,5)
break

