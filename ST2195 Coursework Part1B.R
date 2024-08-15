simulate_random <- function(s) {
  N = 2000
  s = s
  
  J = 4
  x0 <- floor(runif(J, min=1, max=101))
 
  X = matrix(0, J, N)
  
  f <- function(x){
    1/2*exp(-abs(x))
  }
  
  for (j in 1:J) {
    for (i in 1:N) {
      if (i == 1) {
        prev_x = x0[j]
      } else {
        prev_x = X[j, i - 1]
      }
      
      x = rnorm(1, mean = prev_x, sd = s)
      
      # Count Ratio
      #print(x)
      #print(prev_x)
      r = f(x) / f(prev_x)
      #print(f(x))
      #print(f(prev_x))
      # Generate random number u from uniform distribution
      u = runif(1, min = 0, max = 1)
      
      # If u < r, set X[i] = x, else set X[i] = prev_x.
      if (log(u) < log(r)) {  
      # if (u < r) { 
        X[j, i] <- x
      } else {
        X[j, i] <- prev_x
      }
    }
  }
  
  M = numeric(J)
  for(i in 1:J) {
    M[i] = mean(X[i,])
  }
  
  V = numeric(J)
  for(i in 1:J) {
    V[i] = 1/N * sum((X[i,] - M[i])**2)
  }
  
  W = 1/J * sum(V)
  overall_M = 1/J * sum(M)
  
  B = 1/J * sum((M-overall_M) ** 2)
  
  R =  ((B + W) / W) ** 0.5
  
  return (R)
}

s = seq(from=0.001, to=1, by=0.001)
df = data.frame(s)
R =  apply(df, MARGIN=1, FUN=simulate_random)
R

#ggplot(df, aes(x = s)) + 
#  geom_function(fun=simulate_random, colour="red")

plot(s, R, type='l', col = "darkorange", main = "Graph of Estimated R Values for Various s")
