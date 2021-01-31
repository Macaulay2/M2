-- no-check-flag (see https://github.com/Macaulay2/M2/issues/1392)
-- test that alarms work

time try ( alarm(1); while true do 1 ) else true

R = QQ[vars(0..24)]
f = () -> (alarm 4; try res coker vars R else "ran out of time")
time f()
