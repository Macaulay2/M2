atan(sqrt(3.0)/2,1/2)
-- Notice this is not quite pi/6, but it is within a reasonable epsilon.
epsilon = 10.^-15;
abs(atan(sqrt(3.0)/2,1.0/2) - pi/6) < epsilon
