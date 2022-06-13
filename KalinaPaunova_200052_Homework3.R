#Problem 1
FFunction = function(n){if (n < 1){print("NA")
  }else {Result = 1
              for(p in 1:n){
                Result=Result*p
              }
return(Result)
  }
}


#Problem 2 
#Write a function which takes a vector and returns its standard deviation.
#You should get the same results as the sd() function.

SDFunction = function(n){
    ln=length(n)
    sqDev = (n -  mean(n))^2
    Result = sqrt(sum(sqDev)/(ln-1))
       return(Result)
   }
  vec = c(1:10)
  
 
  
  