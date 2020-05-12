newPackage(
        "IntroToM2Package", --Same name as the filename
	Version => "1.0", 
	Date => "May 12, 2020",
	Authors => {
	    {Name => "Jane Doe", Email => "jane.doe@m2.edu", HomePage => "https://janedoem2.github.io/"},
	    {Name => "Jane Doe", Email => "jane.doe@m2.edu", HomePage => "https://janedoem2.github.io/"}
	    },
	Headline => "A Sample Package",
	DebuggingMode => true -- help debug during dev time. Turn to false at deployment
        )


export{
    -- Options
    "myPower", 
    
    -- Methods
    "powersList1",
    "powersList2",
    "powersList3",  -- this is an internal method now.      
    "myDegree"
    }

-- list of needed packages. 
needsPackage"Depth"

-----------------------------------------------------------
-----------------------------------------------------------
--Your Methods here
-----------------------------------------------------------
-----------------------------------------------------------

--#######################
-- Problem 1:
--
-- input: A List and an optional argument, myPower, as an integer
-- output: A List with the entries of the input list rased to myPower

-- version 1
powersList1 = method(TypicalValue => List, Options => {myPower => 2})
powersList1(List) := List => opts -> userList -> (
    local powersList;
    
    powersList = apply(userList, i -> i^(opts.myPower) );
    
    return powersList
    )


-- version 2
powersList2 = method(TypicalValue => List, Options => {myPower => 2})
powersList2(List) := List => o -> userList -> (
    
    apply(userList, i -> i^(o.myPower) ) -- the last output is automoatically returned

    )


-- version 3
powersList3 = method(TypicalValue => List, Options => {myPower => 2})
powersList3(List) := List => opts -> userList -> (
    local mutantList; local l;
    
    mutantList = new MutableList from userList; -- allows you to change elements of  list
    
    l = #mutantList; -- length of mutable list
    for i from 0 to l-1 do (    
	mutantList#i = ( mutantList#i ) ^ ( opts.myPower );
	);
    
    return new List from mutantList -- need to change back to a List
    )






--#######################
-- Problem 2:
-- input: A monomial ideal I and an integer n
-- output: The list of generators of degree n. If there are 
--     no generators of degree n, then the output is an empty list.
-- 
   
myDegree = method(TypicalValue => List) -- no options, but for documentation reasions we have `TypcicalValue` defined.
myDegree (Ideal, ZZ) := List => (I, n) -> ( -- two inputs here, and ideal and an integer
    local L; local myDeg;
       
    L = flatten entries gens I;
    
    myDeg = apply(L, l -> if (first degree(l) == n) then l else 0);    
    myDeg = delete(0,myDeg);
    
    if myDeg == {}
    then return myDeg
    else return first myDeg
)    


-----------------------------------------------------------
-----------------------------------------------------------
--Your Tests Here
-----------------------------------------------------------
-----------------------------------------------------------


TEST ///
-- test 0
-- Problem 1 Tests
assert( {1,4,9} == powersList1({1,2,3}) )
assert( {1,4,9} == powersList2({1,2,3}) )
assert( {1,4,9} == powersList3({1,2,3}) )

assert( {1,8,27} == powersList1({1,2,3}, myPower => 3) )
assert( {1,8,27} == powersList2({1,2,3}, myPower => 3) )
assert( {1,8,27} == powersList3({1,2,3}, myPower => 3) )
///



TEST ///
-- test 1
-- Problem 2 Tests
R = ZZ/101[x,y,z]
I = ideal"x2,y3,z4"
assert( y^3 == myDegree (I,3))
assert( x^2 == myDegree (I,2)) 
assert( {} == myDegree(I,5))

S = ZZ/101[a,b,c]
J = ideal"x2y3z,xyz3,xy7z"
assert( x^2*y^3*z == myDegree (J,6))
assert( x*y*z^3 == myDegree (J,5))
assert( x*y^7*z == myDegree (J,9))
assert( {} == myDegree(J,2))
///


-----------------------------------------------------------
-----------------------------------------------------------
--Documentation
-----------------------------------------------------------
-----------------------------------------------------------

beginDocumentation()

document { 
  Key => IntroToM2Package, -- same as filename
  Headline => "A package for packages",
   
   PARA {
       "This package is a package for packages. There are methods like, ", TO "myDegree", 
       ". And you can write LaTeX:  an ideal ", 
       TEX /// $$I^{(n)} = \cap_{p \in Ass(R/I)}(I^nR_p \cap R ),$$ ///,
       "."},

   PARA {"Alternatively, another paragraph"},
   
   UL { 
       {"M. Hochster and C. Huneke.", EM " Comparison of symbolic and ordinary powers of ideals.", 
	   " Invent. Math. 147 (2002), no. 2, 349–369."}, 
       {"R. Villarreal.", EM " Monomial algebras.", " Second edition. Monographs and Research Notes 
	   in Mathematics. CRC Press, Boca Raton, FL, 2015. xviii+686 pp. ISBN: 978-1-4822-3469-5."}, 
       {"Hailong Dao, Alessandro De Stefani, Eloísa Grifo, Craig Huneke, and Luis Núñez-Betancourt. ", 
	   EM "Symbolic powers of ideals.", "in Singularities and foliations. Geometry, topology and applications, pp. 387-432, Springer Proc. Math. Stat., 222, Springer, Cham, 2018. See ", HREF("https://arxiv.org/abs/1708.03010","https://arxiv.org/abs/1708.03010"), "."} 
       },
  
   SUBSECTION "Contributors", "The following people have generously
   contributed code or worked on our code at various Macaulay2
   workshops.",
     
     UL {
	 "You ",
	 "Me",
	 "Chad"
	}

}  



doc /// -- creating a page of documentation
   Key
       myDegree
       (myDegree, Ideal, ZZ)
   Headline
       computes list of generators of given degree
   Usage
       myDegree(I,n)
   Inputs
        I:Ideal 
	    a monomial ideal
	n:ZZ
	    the degree we are looking for
   Outputs
       :List
           the list of generators of the 
   Description
       Text  
       	   Some words to say things. You can even use LaTeX $R = k[x,y,z]$. 

       Example
           R = QQ[x,y,z,a,b]
     	   I = intersect(ideal(x,y,z),ideal(a,b))
    	   myDegree(I,3)
	   
       Text
       	   More words, but don't forget to indent. 
	   
   SeeAlso
       --codim
       --assPrimesHeight
   Caveat
       myDegree is was Problem 2 in the tutorial yesterday.
///


doc ///
   Key
       powersList1
       (powersList1, List)
   Headline
       computes list of generators of given degree
   Usage
       powersList1(L)
   Inputs
        L:List
   Outputs
       :List
           the list of generators of the 
   Description
       Text  
       	   Some words to say things. You can even use $R = k[x,y,z]$. 

       Example
           R = QQ[x,y,z,a,b]
     	   I = intersect(ideal(x,y,z),ideal(a,b))
    	   myDegree(I,3)
	   
       Text
       	   More words, but don't forget to indent. 
	   
   SeeAlso
       --codim
       --assPrimesHeight
   Caveat
       myDegree is was Problem 2 in the tutorial yesterday.
///


doc /// -- option documentation
     Key 
         myPower
     Headline 
         optional input for powersList1
     Usage 
         powersList1(List,myPower=>ZZ)
     Inputs 
     	  L:List
     Outputs
          :List
     Description	  
       Text
	   What it does

       Example 
	   powersList1({1,2,3}, myPower=>3)

///


doc /// -- option documentation
     Key 
         [powersList1,myPower]
     Headline 
         optional input for powersList1
     Usage 
         powersList1(List,myPower=>ZZ)
     Inputs 
     	  L:List
     Outputs
          :List
     Description	  
       Text
	   What it does

       Example 
	   powersList1({1,2,3}, myPower=>3)

///

----------------------------------------------------
----------------------------------------------------
end
----------------------------------------------------
----------------------------------------------------


restart
path = {"~/GitHub/Workshop-2020-Cleveland/"}|path

-- load "IntroToM2Package.m2" 

uninstallPackage"IntroToM2Package"
loadPackage"IntroToM2Package"

restart
uninstallPackage"IntroToM2Package"
restart
installPackage"IntroToM2Package"
viewHelp"IntroToM2Package"
check"IntroToM2Package"
   

-- TEST DOWN HERE WITH F11

L = apply(15,l -> l+1)
