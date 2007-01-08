-- here we test exception handling in the engine

assert try (
     R = QQ[t,u,x,MonomialOrder => Lex, MonomialSize => 32];
     gens gb ideal (t*u,t-x^(2^30),u-x^(2^30));		    -- gb has x^(2^31) in it, which will overflow
     false ) else true
