kk = ZZ/32003
J = id_(kk^5)
I = mutableMatrix(J,Dense=>false)
I1 = mutableMatrix(I,Dense=>true)
I2 = mutableMatrix(I1,Dense=>false)
assert(J == matrix I2)
assert(J == matrix I1)

-- Local Variables:
-- M2-send-to-buffer: "*gud*"
-- End:

