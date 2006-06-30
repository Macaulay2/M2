-- Determine the options to resolution

R = ZZ/31991 [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, MonomialSize=>8]
I = ideal" pr-os+mt,          
     jr-is+gt,
     pq-ns+lt,
     oq-nr+kt,
     mq-lr+ks,
     jq-hs+ft,
     iq-hr+et,
     gq-fr+es,
     dq-cr+bs-at,
     jo-ip+dt,
     mn-lo+kp,
     jn-hp+ct,
     in-ho+bt,
     gn-fo+ep+at,
     dn-co+bp,
     jm-gp+ds,
     im-go+dr,
     hm-fo+ep+cr-bs+at,
     jl-fp+cs,
     il-fo+cr+at,
     hl-fn+cq,
     gl-fm+as,
     dl-cm+ap,
     jk-ep+bs-at,
     ik-eo+br,
     hk-en+bq,
     gk-em+ar,
     fk-el+aq,
     dk-bm+ao,
     ck-bl+an,
     gh-fi+ej,
     dh-ci+bj,
     df-cg+aj,
     de-bg+ai,
     ce-bf+ah"
GEO = 2^16
AUTO1 = 2^14
AUTO2 = 2^15
LEVEL = 2^13
LEVELSTRIP = 2^13 + 2^18  -- not a minimal resolution... but generally fast to compute

DESCENDING = 8
REVERSE = 16
DEGREE = 32

COMPARE'LEX = 0
COMPARE'LEX'EXTENDED = 1
COMPARE'LEX'EXTENDED2 = 2
COMPARE'ORDER = 3
COMPARE'MONORDER = 4

-- multiply by these:
SKELETON = 1
REDUCTIONS = 64

-- skeleton sort: e.g. SKELETON*(COMPARE'LEX + DESCENDING + REVERSE + DEGREE)
-- reduction sort (always by increasing degree)

-- order is 6 bits:
-- first 3 bits give kind of sort (COMPARE'LEX, ...)
-- 2^3, 4th bit: DESCENDING
-- 2^4, 5th bit: REVERSE
-- 2^5, 6th bit: DEGREE

I = ideal flatten entries gens I;
-- time C = res(I, Strategy=>0, SortStrategy=>AUTO2 + AUTO1);  BUG!!
time C = res(I, Strategy=>0, SortStrategy=>GEO+LEVEL) -- this one isn't minimal... why not? is this a BUG?

I = ideal flatten entries gens I;
time C = res(I, Strategy=>0, SortStrategy=>GEO + AUTO2 + AUTO1);
I = ideal flatten entries gens I;
time C = res(I, Strategy=>0, SortStrategy=>GEO+LEVELSTRIP) -- this one isn't minimal... why not? is this a BUG?

gbTrace = 3
I = ideal flatten entries gens I;
time C = res(I, Strategy=>0, SortStrategy=>GEO + AUTO2 + AUTO1 + SKELETON*(COMPARE'LEX + DESCENDING + REVERSE + DEGREE));
time C = res(I, Strategy=>0, SortStrategy=>GEO + AUTO2 + AUTO1 + REDUCTIONS*(COMPARE'LEX + DESCENDING + REVERSE + DEGREE));
time C = res(I, Strategy=>0, SortStrategy=>GEO + REDUCTIONS*(COMPARE'ORDER + DESCENDING + REVERSE));
time C = res(I, Strategy=>0, SortStrategy=>GEO + REDUCTIONS*(COMPARE'ORDER + DESCENDING));
time C = res(I, Strategy=>0, SortStrategy=>GEO + COMPARE'ORDER + REVERSE + REDUCTIONS*(COMPARE'ORDER + REVERSE));
time C = res(I, Strategy=>0, SortStrategy=>GEO + REDUCTIONS*(COMPARE'MONORDER));
gbTrace=0

time C = res(I, Strategy=>0)
status(C,Monomials=>true,TotalPairs=>false)
betti C === new BettiTally from {
     (0,{0}) => 1, 
     (1,{2}) => 35, 
     (2,{3}) => 140, 
     (3,{4}) => 189, 
     (3,{5}) => 112, 
     (4,{6}) => 735, 
     (5,{7}) => 1080, 
     (6,{8}) => 735, 
     (7,{9}) => 112, 
     (7,{10}) => 189,
     (8,{11}) => 140, 
     (9,{12}) => 35, 
     (10,{14}) => 1
     }
