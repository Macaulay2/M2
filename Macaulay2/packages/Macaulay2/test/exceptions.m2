-- here we test exception handling in the engine

R = QQ[t,u,x,MonomialOrder => Lex, MonomialSize => 32]

assert try (
     gens gb ideal (t*u,t-x^(2^30),u-x^(2^30));		    -- gb has x^(2^31) in it, which will overflow
     false ) else true

assert try (
     dual R^{2^31};					    -- free module degrees are stored in 32 bits
     false ) else true
