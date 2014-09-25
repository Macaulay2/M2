needsPackage "NumericalSchubertCalculus"

n = 4
k = 2

OsculatingFlags = {
    id_(FFF^4),
    rsort id_(FFF^4), 
    sub(transpose matrix {{1,1,1,1},{0,1,2,3},{0,0,2,6},{0,0,0,1}},FFF),
    sub(transpose matrix {{1,2,4,8}, {0,1,4,12}, {0,0,1,6}, {0,0,0,1}},FFF)
    };
