kk = ZZ/101
S=kk[vars(0..11)]

I = ideal"-be+af,-de+cf,-dg+ch,-bi+aj"
time radical I -- fast now

J = ideal"-de+cf,-bg+ah,-fg+eh,-bi+aj,-di+cj"
time intersect decompose J -- fast
time radical J --  a problem
