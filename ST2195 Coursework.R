x0 <- 28
N <- 10000
s <- 1
  
X <- numeric(N)

f <- function(x){
  1/2*exp(-abs(x))
}

for (i in 1:N) {
  #  Simulating random number
  
  # As our X only contains X1 ... XN, and we have a separate x0 variable, we
  # have to separate the 2 situations.  We assign the designated X[i-1] to "prev_x"
  # 1. Read from initialized x0 variable if its the first loop
  # 2, All other cases read from X[i - 1]
  if (i == 1) {
    prev_x = x0
  } else {
    prev_x = X[i-1]
  }
  
  x = rnorm(1, mean = prev_x, sd = s)
  
  # Count Ratio
  r = f(x) / f(prev_x)
  
  # Generate random number u from uniform distribution
  u = runif(1, min = 0, max = 1)
  
  # If u < r, set X[i] = x, else set X[i] = prev_x.
  if (log(u) < log(r)) {  
    X[i] <- x
  } else {
    X[i] <- prev_x
  }
}

#print(X)

#install.packages("ggplot2")
#library(ggplot2)

df <- data.frame(X)

ggplot(df, aes(x = X)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "pink") +
  geom_density(colour="black") +
  geom_function(fun=f, colour="purple")

print(mean(X))
print(sd(X))
