-- Many monomial operations are apparently not checking their arguments

R = ZZ/101[a..d,MonomialSize=>8]
f = a^16
f^16 -- should overflow
f^17 -- wrong
f^15 == a^(16*15)

R = ZZ/101[a..d,MonomialSize=>16]
f = a^256
f^256 -- should overflow

R = ZZ/101[a..d,MonomialSize=>32]
f = a^65536
f = f^65536 -- this one gives overflow

R = ZZ/101[a..d,MonomialSize=>16,Degrees=>{256,256,256,256}]
f = a^256 -- should overflow (why??? MES: 27 Jan 2008)

R = ZZ/101[a..d,Degrees=>{4:65536}]
f = a^65536 -- should not be negative!

end

