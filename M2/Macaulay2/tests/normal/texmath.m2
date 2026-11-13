stripTexMathSpaces = s -> concatenate for i to #s-1 list (
    if s#i === " " and (i === 0 or s#(i-1) =!= "\\") then "" else s#i)
stripTexMathBraces = s -> (
    while #s >= 2 and s#0 === "{" and s#(#s-1) === "}" do s = substring(s, 1, #s-2);
    s)
normalizeTexMath = stripTexMathBraces @@ stripTexMathSpaces
assertTexMath = (x, s) -> assert Equation(normalizeTexMath texMath x, normalizeTexMath s)

assertTexMath(0, ///0///)
assertTexMath(-7, ///-7///)
assertTexMath(3/5, ///\frac{3}{5}///)
assertTexMath(2.5, ///{2.5}///)
assertTexMath(true, ///\texttt{true}///)
assertTexMath(infinity, ///\infty///)
assertTexMath(-infinity, ///{-\infty}///)

assertTexMath("alpha_beta", ///\texttt{alpha\char95 beta}///)
assertTexMath(symbol alpha, ///\mathit{alpha}///)
assertTexMath(ZZ, ///{\mathbb Z}///)
assertTexMath(QQ, ///{\mathbb Q}///)

assertTexMath((1,2,3), ///\left(1,\,2,\,3\right)///)
assertTexMath({1,2,3}, ///\left\{1,\:2,\:3\right\}///)
assertTexMath([1,2,3], ///\left[1,\,2,\,3\right]///)
assertTexMath(new Option from (a=>1), ///a\ \Rightarrow \ 1///)
assertTexMath(new HashTable, ///\texttt{HashTable}\left\{\,\right\}///)
assertTexMath(new HashTable from {a=>1,b=>{2,3}}, ///\texttt{HashTable}\left\{a\ \Rightarrow \ 1,\:b\ \Rightarrow \ \left\{2,\:3\right\}\right\}///)
assertTexMath(new MutableList from {1,2}, ///\texttt{MutableList}\left\{\ldots 2\ldots\right\}///)

R = QQ[x,y,z]
assertTexMath(R, ///R///)
assertTexMath(x*y^2, ///x\,y^{2}///)
assertTexMath(x^2 + y*z - 3, ///x^{2}+y\,z-3///)
assertTexMath(ideal(x^2,y*z), ///\texttt{ideal}{}\left(x^{2},\,y\,z\right)///)
assertTexMath(R/ideal(x^2-y), ///\frac{R}{x^{2}-y}///)
assertTexMath(R^2, ///R^{2}///)
assertTexMath(map(R,R,{x+y,y,z}), ///\texttt{map}{}\left(R,\,R,\,\left\{x+y,\:y,\:z\right\}\right)///)

assertTexMath(αbar_1, ///\bar{\alpha}_{1}///)
assertTexMath(addot, ///\ddot{a}///)
