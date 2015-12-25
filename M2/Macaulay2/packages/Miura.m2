pR=(kk,v,w)->kk[v,MonomialOrder=>{Weights => w, Weights=>toList(1 ..#w) }];
qR=(R,p)->R/ideal p
inv=L-> quotient(ideal first first entries gens gb L, L)
reduced=L->inv inv L
add=(J,K)-> reduced (J*K)
double=J->add(J,J)
multi=(J,m)->(
    if m==0 then return ideal 1_I
    else if m\%2==0 then double multi(J,m//2)
    else add(double multi(J,(m-1)//2),J)
    )
