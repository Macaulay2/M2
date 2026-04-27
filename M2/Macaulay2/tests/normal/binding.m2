ZZ_! := ZZ^! := ZZ_~ := ZZ^~ := identity
assert same{4, 4_!, 4^!, 4_~, 4^!}

ZZ |_ ZZ :=
ZZ ^> ZZ := ZZ ^>= ZZ := ZZ ^< ZZ := ZZ ^<= ZZ :=
ZZ _> ZZ := ZZ _>= ZZ := ZZ _< ZZ := ZZ _<= ZZ := (a,b) -> a^b
assert same{8,
    2 |_ 3,
    2 ^> 3, 2 ^>= 3, 2 ^< 3, 2 ^<= 3,
    2 _> 3, 2 _>= 3, 2 _< 3, 2 _<= 3}
