-- Bug found early Feb 2008 by Graham Denham.  Fixed 4 Feb 2008
R = ZZ[e_1..e_9, SkewCommutative=>true]
I = ideal(-e_1+e_2+e_4,-e_1-e_2+e_5,-e_1+e_3+e_6,-e_1-e_3+e_7,-e_2+e_3+e_8,-e_2-e_3+e_9)
gb I -- crashes in both 0.9.95 and (earlier versions of) 1.0.9test
assert (gens gb I == matrix {{e_6-e_7-e_8+e_9, e_5-e_7-e_8, e_4-e_7+e_9, 2*e_3+e_8-e_9, e_2+e_3-e_9, e_1+e_3-e_7, e_3*e_8+e_3*e_9+e_8*e_9}})
