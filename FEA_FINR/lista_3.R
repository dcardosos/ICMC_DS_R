# Q10

f <- function(x){ x^2 }


simpson <- function(f, a, b, n){
  
  h = (b - a) / n
   
  second_term = f(a + 0*h) + f(a + n*h) # n = 0 and n = 10
  
  for(i in 1:(n-1)){
    
    xi = a + i*h
    
    if(i %% 2 == 0){
      
      second_term = second_term + 2*f(xi)
      
    } else {
      
      second_term = second_term + 4*f(xi)
    }
    
  }
  
  (h/3) * second_term
  
}

simpson(f, 1, 7, 10)  # 114
integrate(f, lower = 1, upper = 7) # 114



