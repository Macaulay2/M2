-- let's make sure the random number generator doesn't change

assert(
     apply(20, i -> random 20)
     ==
     {10, 11, 15, 12, 1, 1, 10, 17, 18, 1, 14, 0, 6, 16, 10, 8, 19, 5, 4, 14}
     )
