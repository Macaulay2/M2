-- From an email of 4 May 2008 from David Madore:

R0 = (ZZ/32003)[u]
K0 = frac R0
K = K0[v]/(v^2+u^2)
invv = substitute(-1/u^2,K)*v
v*invv
toField K
1/v  -- actual crash.

-- added 9 Sep 2010:

v^-1  -- crashes! still crashes, 22 May 2017.

1//v -- this one works


-- bug: it should be checked that a fraction field isn't over another fraction field
-- or better, get it to work!