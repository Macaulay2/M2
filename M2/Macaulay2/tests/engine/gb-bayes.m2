if instance(gbTrace,ZZ) then gbTrace = 1 else gbTrace 1

R = ZZ/32003[p_(1,1,1,1,1)..p_(2,2,2,2,2)]
R = ZZ/32003[p_(1,1,1,1,1)..p_(2,2,2,2,2), MonomialSize=>8]
-- Bayesian graph ideal #148
J = ideal(
         -p_(1,1,2,1,1)*p_(2,1,1,1,1)+p_(1,1,1,1,1)*p_(2,1,2,1,1),
         -p_(1,1,2,1,2)*p_(2,1,1,1,2)+p_(1,1,1,1,2)*p_(2,1,2,1,2),
         -p_(1,1,2,2,1)*p_(2,1,1,2,1)+p_(1,1,1,2,1)*p_(2,1,2,2,1),
         -p_(1,1,2,2,2)*p_(2,1,1,2,2)+p_(1,1,1,2,2)*p_(2,1,2,2,2),
         -p_(1,2,2,1,1)*p_(2,2,1,1,1)+p_(1,2,1,1,1)*p_(2,2,2,1,1),
         -p_(1,2,2,1,2)*p_(2,2,1,1,2)+p_(1,2,1,1,2)*p_(2,2,2,1,2),
         -p_(1,2,2,2,1)*p_(2,2,1,2,1)+p_(1,2,1,2,1)*p_(2,2,2,2,1),
         -p_(1,2,2,2,2)*p_(2,2,1,2,2)+p_(1,2,1,2,2)*p_(2,2,2,2,2),
         -p_(1,1,1,1,2)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,1,2),
         -p_(1,1,1,2,1)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,2,1),
         -p_(1,1,1,2,1)*p_(1,2,1,1,2)+p_(1,1,1,1,2)*p_(1,2,1,2,1),
         -p_(1,1,1,2,2)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,2,2),
         -p_(1,1,1,2,2)*p_(1,2,1,1,2)+p_(1,1,1,1,2)*p_(1,2,1,2,2),
         -p_(1,1,1,2,2)*p_(1,2,1,2,1)+p_(1,1,1,2,1)*p_(1,2,1,2,2),
         -p_(1,1,2,1,2)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,1,2),
         -p_(1,1,2,2,1)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,2,1),
         -p_(1,1,2,2,1)*p_(1,2,2,1,2)+p_(1,1,2,1,2)*p_(1,2,2,2,1),
         -p_(1,1,2,2,2)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,2,2),
         -p_(1,1,2,2,2)*p_(1,2,2,1,2)+p_(1,1,2,1,2)*p_(1,2,2,2,2),
         -p_(1,1,2,2,2)*p_(1,2,2,2,1)+p_(1,1,2,2,1)*p_(1,2,2,2,2),
         -p_(1,1,1,1,2)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,1,2),
         -p_(1,1,1,2,2)*p_(1,2,1,2,1)+p_(1,1,1,2,1)*p_(1,2,1,2,2),
         -p_(1,1,2,1,2)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,1,2),
         -p_(1,1,2,2,2)*p_(1,2,2,2,1)+p_(1,1,2,2,1)*p_(1,2,2,2,2),
         -p_(1,1,1,2,1)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,2,1),
         -p_(1,1,1,2,2)*p_(1,2,1,1,2)+p_(1,1,1,1,2)*p_(1,2,1,2,2),
         -p_(1,1,2,2,1)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,2,1),
         -p_(1,1,2,2,2)*p_(1,2,2,1,2)+p_(1,1,2,1,2)*p_(1,2,2,2,2),
         -p_(1,1,1,1,2)*p_(1,1,1,2,1)+p_(1,1,1,1,1)*p_(1,1,1,2,2)
	   +p_(1,1,1,2,2)*p_(1,1,2,1,1)-p_(1,1,1,2,1)*p_(1,1,2,1,2)
	   -p_(1,1,1,1,2)*p_(1,1,2,2,1)-p_(1,1,2,1,2)*p_(1,1,2,2,1)
	   +p_(1,1,1,1,1)*p_(1,1,2,2,2)+p_(1,1,2,1,1)*p_(1,1,2,2,2)
	   +p_(1,1,1,2,2)*p_(1,2,1,1,1)+p_(1,1,2,2,2)*p_(1,2,1,1,1)
	   -p_(1,1,1,2,1)*p_(1,2,1,1,2)-p_(1,1,2,2,1)*p_(1,2,1,1,2)
	   -p_(1,1,1,1,2)*p_(1,2,1,2,1)-p_(1,1,2,1,2)*p_(1,2,1,2,1)
	   -p_(1,2,1,1,2)*p_(1,2,1,2,1)+p_(1,1,1,1,1)*p_(1,2,1,2,2)
	   +p_(1,1,2,1,1)*p_(1,2,1,2,2)+p_(1,2,1,1,1)*p_(1,2,1,2,2)
	   +p_(1,1,1,2,2)*p_(1,2,2,1,1)+p_(1,1,2,2,2)*p_(1,2,2,1,1)
	   +p_(1,2,1,2,2)*p_(1,2,2,1,1)-p_(1,1,1,2,1)*p_(1,2,2,1,2)
	   -p_(1,1,2,2,1)*p_(1,2,2,1,2)-p_(1,2,1,2,1)*p_(1,2,2,1,2)
	   -p_(1,1,1,1,2)*p_(1,2,2,2,1)-p_(1,1,2,1,2)*p_(1,2,2,2,1)
	   -p_(1,2,1,1,2)*p_(1,2,2,2,1)-p_(1,2,2,1,2)*p_(1,2,2,2,1)
	   +p_(1,1,1,1,1)*p_(1,2,2,2,2)+p_(1,1,2,1,1)*p_(1,2,2,2,2)
	   +p_(1,2,1,1,1)*p_(1,2,2,2,2)+p_(1,2,2,1,1)*p_(1,2,2,2,2));

-------------------
-- GB timing ------
-------------------
time gb J; -- 0.9.2: 7.56 sec
-*
 -- 0.9.5, unoptimized, with debugging: 116.1 sec
 --        optimized: 40.13 sec
 -- in 0.9.5:
 {2}(29){3}(30){4}(79){5}(243){6}(671){7}(845){8}(1364){9}(1582){10}(2130)
   -- {11}(1252){12}(620){13}(100){14}(28)     -- used 116.1 seconds
 total: 1 678
          0: 1   .
          1: .  21
          2: .  14
          3: .  31
          4: .  68
          5: .  65
          6: . 106
          7: . 110
          8: . 141
          9: .  76
         10: .  39
         11: .   5
         12: .   2
o10 = total: 1 678
          0: 1   .
          1: .  21
          2: .  14
          3: .  31
          4: .  68
          5: .  65
          6: . 106
          7: . 110
          8: . 141
          9: .  76
         10: .  39
         11: .   5
         12: .   2
-- 0.9.2
i4 : time gb J
{2}(0)gggggggggogogggogogogogggogog{3}(30)mmmmmmmmmmmmooooooooooooomooom
{4}(97)mmmmmmmmommmooomoommmooooooooommmooooooooommmooooooooommoooooommmooommmmmo
ooooo{5}(342)moooommoooommmoooommmmoooommmmoooommmmmoooommmmooooooooooommmo
ommmoommmoooooooooooooooommomooommoommmooomoommooooooooomomoomoomomooomoomm
oooooooooooooooooooooommoooommoooommoooomoooooooooooooommommmoommoooooooooo
oomooooooooomoooooooooomooooooooooo{6}(1085)oooooooommomoooooooomooooomoooo
momoomooooooooooomooomooooomommmoooooooooooooooooomooomooooomomoooooooommmo
ooooooooooooooooooooooooooommoooooooooooooooooooooooooooooomooooooooommmooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
ooomoooooooooooooooooomoooooooooooooooooomooooooooooooooooooooooooooooooooo
oooooooooooomooooooooooooooooooooooomoommooooooooooooooomoommoooooooooooooo
oooooooooooooooooooooooooooooooooooommoooooooooooomommooooooooooommmooooooo
oooooooooooooooooooooooooooomoooooomoooooooooooomoomomoooooooomoomoommmoooo
oooooooooommmoooooooooooooooomommoooooooooomooooooooooooooooooooooomooooooo
oooooooooooomoooooooooooooooooooomoooooooooooooo{7}(1253)oooooomoomoooooooo
oooooooooooooomoooooooommmooooooooomooomoooooomooooooomooooooomoooooooooooo
oooomoomoooooooooooooomoooommoooooooooooooooommoooooooooooooooooooooomooooo
oooooomoooomoooooomoooooooooooooooooooomoooooooomooooooooomoooooooooooooooo
ooooooooooooomoooooooooomooooooooooooooooooooooooooomoooooooooooooooommoooo
ooomoooooooooooooooooommoooooooooooooooooomooooommooooooooooooomoomommomoom
ommommoooooomomoooooooooomoooomoooooommoooooooooooooooooooooooooooooooooooo
ooooooomomooooooooooooooommommomooooooooommoooooomoooooooommmooomoooooooooo
oooooooooooooooooooooooooommoooooomooooooooooooooommoooooomoooooooommommomo
oooooommoooooooooooooooooooomoooooooooooooooooooooooooooomomoooooooooooomoo
ooooooomomoooooooooooooomoooooooommmmooooooooooooooooooooooooooomomoooooooo
ooooooooooooooooommooooooomooooooooommoooooooooommooooooooooooooooooomomooo
ooooooooooooomoooooooooooomomoooooooooooooooomooooooooooooooooooooooooooooo
oooooooomooooooooooooooooooooooooooooooooooooooooooo{8}(1776)ooooooooomoooo
oooooooooooomooooooooooooooooomooomooooooooooomoooooooooooooooooooooooomooo
ooooooooooooooooomooooooooooooooomomoooooooooommmomoooooooommooooooooooooom
ooooomoooooooooommooooooooooomoooooooooomoooomooomoomoooooooooooooooooooooo
oooooooooooooooooooomoooooooooooooooooooooooooooomommmooooooooommoooooooooo
ooomooooooooooooooooomooooooooooooooooooommomoooooooomooooooooooooooommomoo
oomoomomoooooooooommoooomoooooooooooomoomooomoooooomoooooomomooooooomoomooo
oooomooooooooooooooomooooooooooooooooooooomoooomooooooomoooooooomomoooooooo
ommomooomooomoooomooooooooooooooooooooooooooooooomoooooooooooooomoooomoooom
oooooomomooooooooooomomoooooomoooooooooooooooooooooooooooooooooooooomooomoo
ooooooooooooooooomooooooooooooooommooooooooooommooooooooooooooooooooomomooo
ooooooomomooooooooooooooooooooooooooooooooooomomooooooooooooooooooooooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooomomooooooo
ooomomooooooooooooooooooooooooooooooooooomoooooooooooooooooooomomoooooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
oooooooooooooooooooooooooooooooooommooooooooooommoooooooooooooooooooooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
ooooooooooooooooooommmmmmoooooooooooooooooomooooooooooooooooooooooooooooooo
oooooomoooooooooooooooooooooooooooooooooooooooooooooomooooooooooooooooooooo
moooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
oooooooooooooooooooooo{9}(1899)mooomoommmmmmmommooommmmoooooooooooomomooooo
ooooooooommmmmmooommmmoooooooooooooooommoooommmmomoommooooooooooooooooooomo
moooooooooooooooooooooooooooooooomoooooooooooooooommmmmmmooooooooooooooommo
ooooooooooooooooooomoomooooooooooooomooooooooooooooooomooooooooooommmmmmooo
oooooooomoooooommoooooooooooooooooooooooooooooomooooooooooooooooooooooomooo
oooooooooooooomoooooooooooooomooooooooooooommmmmmmooommmooooooooooommoooooo
ooooomomooooommmmooooooomoomomooooooooooooooooooooooooooooooooooooooooooooo
omooooooooooooooooooooooooooooooooooooooooomomomoooomooooooomooomoooooooooo
ooomoooooooooooooommmoooooooooooooooooooooooooooooooooooooooooooooooooooooo
oooooooomoooooooooooooooooooooooooooooooooooooooooooooooooooomooooooooooooo
ooooooooooooooooooooooooooomoooooomoooooooooooooooooooooooooooooooooooooomo
oooooooooooooooooooooooooooooooooooooooomooooooomoooooooooooooooooooooooooo
oooooooooomoooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
omoooooooooooooooooooooomooooooooooooooooooooooooooooooooooooooooomooomoooo
ommoooooooooooooooooooommmooooooooomommooommooooooommoooooooomooomoooooomoo
oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooom
oomoooooommoooooooooooooooooooomooooomomoooooooooommmmoooooooooomoomooooooo
moooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooomooooooooo
oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooom
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooomooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
ooooooooooooooooooooooooooooooo{10}(2473)omooooooooooooooomoooooooooooooooo
ooooooooooomoooooooooooooooooomooooooooooooooooooooooooooooooooooooooomoooo
oooomomomooooooooooooooomooooomooooooooooooooooooooooooooomoooooooooooooooo
ooooooooooooooooooooooomoooooooooooooooooooooooooooooooooooooooooooomoooomo
moooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooomooooo
ooooooooooooooooooooooooooomomomooooommooooooooooooomooooooooooooommooooooo
oooooomoooooooooooooooooooooooooooooooooooooooooooooooooommooooooomoooooooo
ooooooooooooooooooooooooooooommomomoooooooooooooooooooooooooooooooooooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
oooooooooooooooooooooooooooooooooooooooomooooooooooooomoooooomomooooooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
oooooooooooooooommooooooooooooooooooooooooooooooooooooooooooooomoooommooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
oooooooooooooooooooooooooooooooooooomooooooooomooooooooooooooooooomoooooooo
ooooooomoooooooooooooooooooooooooooooooomomomooooommooooooooooooomooooooooo
oooommmoooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
oooooomomoooooooomooooooooooooooooooooooooooooooooooooooomooooooomomomooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooomoooooooooo
oooooooooooooooooooooooooooooooooooooooooooooooooomoooooooooooooooooooooooo
oooooooooooooooooooooooooooooooooooooooooooooooooooomooooooooomoooooooooooo
ooomooooooooomoooooooooooooooooooomooooooooooooooommooooooooooooooooooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
mooooooooomoooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
oooooooooooooooooooooooooooooooomooooooooooooooooomoooooooooooooooooooooooo
ooooooooooooooooooooooooooooooooooooomooooooooomooooooooooooooooooooooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
oooo{11}(1476)oooooooooooooooooooomoomooooommooooooooooooooomoooooooooooooo
oommooooooooooooooooooooooooooooooooooooommoooooooooooooooooooooooooooooooo
ooooooooomomooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooomooo
oooooooooooooooomoooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooomooooooooooooooooooo
omoooooooooooooooooooooooooooooooooomoooooooooooooooooooooooooooooooooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooomooo
oomoooooooooooooooooooooomoooooooooooooomooooooooooooooooooooooooooooooooom
ooooooooooooooooooooooooooooooooomooomoooooooooooooooooooooooooomoooooooooo
ooooooooooooooooooooooooomooooooooooooooooooooooooooooooooooooooooooooooooo
ooooooooooooooomooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
ooooooooooooooooooooooooooooooooooooooooooomoooooooooooooomoooooooooooooooo
oooooooooooooooooooooooooooooomoooooooooooooooooooooooooooooooooooooooooooo
oooooooooooooooooooooooooooooooooooooooooooooooooooooooomoooooooooooooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
oooooooooooooooooooooooooooooooooooooooommmommooooooooooooooooooooooooooooo
oooooooooooooooooooooooooooooooooooooooommmomoooooooooooooooooooooooooooooo
ooooooooooooooooooooooooooooooo{12}(726)ooooooooooooooooooooooooooooooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
oooooooooooooooooooomoooooooooooooooooooooooooooooooooooooooooooooooooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
ooooooooooooooooooooomoooooooooooooooooooooooooooooooooomoooooooooooooooooo
oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooomoooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
oooooooooooooooooomoooooooooooooooooooooooooooooooooooooooooooooooooooooooo
ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
ooooooooooooooooooooooooooooooooooooooooo{13}(133)ooooooooooooooooooooooooo
ooooooooooooooooooooooooooooooooooooooooomoooooooooooooooooooomoooooooooooo
oooooooooooooooooooooooooooo{14}(33)ooooooooooooooooooooooooooooooooo
     -- used 7.9 seconds
*-

---------------------------
-- simple ideal quotient --
---------------------------
time J1 = J : p_(1,1,1,1,1); -- 0.9.2: 38.43 sec
-*
-- betti J1 -- gives
o11 = generators: total: 1 236
                      0: 1   .
                      1: .  21
                      2: .   .
                      3: .  18
                      4: .   .
                      5: .   .
                      6: .   4
                      7: . 161
                      8: .  30
                      9: .   2
  -- 0.9.2 time: 38.43 sec
  -- 0.9.5 time: 
*-

-- This works in 0.9.5 (slow, but works)
A = R/J;
m = matrix{{p_(1,1,1,1,1)}}
time syz m; -- this one seems to work?
time mingens ideal oo;
betti oo

time A = R/J;
m = matrix{{p_(1,1,1,1,1)}}
time g = syz gb(m,
           Strategy=>LongPolynomial,
           Syzygies=>true,SyzygyRows=>1); -- this works!
      -- So why does J : p_(1,1,1,1,1) fail???

quotelem0 = (I,f) -> (
     -- I is an ideal, f is an element
     If := matrix{{f}} | (gens I);
     g := syz gb(If,
	  Strategy=>LongPolynomial,
	  Syzygies=>true,
	  SyzygyRows=>1);
     g)

time J1 = quotelem0(J, p_(1,1,1,1,1));
time J1min = mingens ideal J1;  -- these two take 21.2 sec in 0.9.2

-- 0.9.5: time J1 = quotelem0(J, p_(1,1,1,1,1)); -- time is 96.08 unoptimized
--{1}(1){2}(29){3}(51){4}(234){5}(408){6}(922){7}(1242){8}(1693){9}(2260)
--   -- {10}(2099){11}(1165){12}(697){13}(311){14}(2)     -- used 96.08 seconds

gens gb J1;

---------------------------
-- saturation -------------
---------------------------
time Jsat = saturate(J,sub(p_(1,1,1,1,1),R));
  -- what algorithm is being used here?
  -- 
  -- betti Jsat -- as reported by 0.9.2 (these are mingens)
  -- o7 = gens      total: 1 264
  --                    0: 1   .
  --                    1: .  21
  --                    2: .   .
  --                    3: .  18
  --                    4: .   .
  --                    5: .   .
  --                    6: .   4
  --                    7: . 161
  --                    8: .  44
  --                    9: .  10
  --                   10: .   6
  -- 0.9.2 time: 133.11 sec
  -- 0.9.5 time: 

time Jsat = saturate(J,sub(p_(1,1,1,1,1),R),Strategy=>Linear);
  -- this does one GB
  -- 0.9.2 time: 
  -- 0.9.5 time: 
  
Rt = (coefficientRing R)[t, gens R, MonomialOrder=>Eliminate 1]
Jt = substitute(J,Rt) + ideal(t*p_(1,1,1,1,1)-1);
gbTrace = 3
time gb Jt;
  -- 0.9.5 time: -- used 270.03 seconds
  -- 0.9.2 time: -- used 102.78 seconds
time g = gens gb Jt; -- 2.5 sec
g1 = selectInSubring(1,g);
J1 = ideal g1;
time J1min = mingens J1; -- answer should have the same betti as Jsat above.
  -- 0.9.5 time: -- 
  -- 0.9.2 time: -- used 12.28 seconds
betti J1min  
