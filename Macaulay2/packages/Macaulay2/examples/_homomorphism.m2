R = QQ[x,y,z]/(y^2-x^3)
H = Hom(ideal(x,y), R^1)
g = homomorphism H_{1}
source g
target g

          H1 = prune H
          homomorphism(H1.cache.pruningMap * H1_{1})
          

          f = basis(3,H)
          rand = random(R^(numgens source f), R^1)
          h = homomorphism(f * rand)
          source h
          target h
          
