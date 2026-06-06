# TropicalToric
A [Macaulay2](http://www2.macaulay2.com/Macaulay2/) package for toric intersection theory using tropical geometry.
## Installation
[Download](https://github.com/AlessioBorzi/TropicalToric/archive/refs/heads/main.zip) the package, extract it. Open a terminal and `cd` to the extracted folder, open Macaulay2 and install the package with
```
installPackage "TropicalToric"
```
## Example
First, load the package with
```
needsPackage "TropicalToric"
```
As an example, we can compute the class of a line in the projective plane
```
X = toricProjectiveSpace 2;
R = QQ[x,y];
I = ideal(x+y+1);
L = classFromTropical(X,I)
```
We can compute intersection products of a toric divisor and a toric cycle and their intersection number
```
C = X_0 * X_{0}
degCycle(C)
```
## Documentation
For more information, access the documentation with the `help` command
```
help TropicalToric
```
or using a browser with
```
viewHelp TropicalToric
```
