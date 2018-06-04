# Questions for Q&A session

- The ReactionNetworks Package uses indexed variables.  How do they work?  Consider the following code:

```
needsPackage("ReactionNetworks")
N=oneSiteModificationA()
R=createRing N
k=N.ReactionRates
```

  At this point, `k_0` should be recognized as a variable in the ring
  `R`.  It has the same name after all.  How do I get the variable in
  `R`?  How do I get the index `{0,1}` of the variable `k_{0,1}`.


- Your question here.