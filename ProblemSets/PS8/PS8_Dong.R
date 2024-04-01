library(nloptr)

# Q4

# Set the seed of the random number generator by issuing the (R) command set.seed(100)
# X is a matrix of dimension N = 100, 000 by K = 10 containing normally distributed random numbers, except the first column which should be a column of 1’s.
# ε (call it eps in your code) is a vector of length N containing random numbers distributed N (0, σ^2), where σ = 0.5 (so σ^2 = 0.25)
# β (call it beta in your code) is a vector of length 10. Let β have the following values:
# β = [1.5 −1 −0.25 0.75 3.5 −2 0.5 1 1.25 2]′
# Now generate Y which is a vector equal to Xβ + ε.

# Set seed for reproducibility
set.seed(100)

# Define dimensions
N = 100000
K = 10

# Generate X matrix
X = matrix(rnorm(N * K), nrow = N, ncol = K)
X[, 1] = 1  # Set first column to 1's

# Generate epsilon vector

eps = rnorm(N, mean = 0, sd = sqrt(0.25))

# Define beta vector
beta = c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Generate Y vector
Y = X %*% beta + eps

# Q5

# Using the matrices you just generated, compute βˆOLS, which is the OLS estimate of β using the closed-form solution 
# (i.e. compute βˆOLS = (X′X)^(−1)(X′Y). 
# [HINT: check here for matrix algebra operations in R] How does your estimate compare with the true value of β in (1)?

# Compute beta hat OLS
beta_hat_OLS <- solve(t(X) %*% X) %*% t(X) %*% Y

# Compare with true value of beta
comparison <- cbind(True_Beta = beta, Estimated_Beta = beta_hat_OLS)
comparison

# Q6

# Compute βˆOLS using gradient descent (as we went over in class). Make sure you appropriately code the gradient vector! Set the “learning rate” (step size) to equal 0.0000003.

# set up a stepsize
alpha <- 0.00003

# set up a number of iterations
maxiter <- 500000

## Our objective function
objfun <- function(beta,y,X) {
  return ( sum((Y-X%*%beta)^2) )
}

# define the gradient of our objective function
gradient <- function(beta,y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}

## read in the data
# Y <- iris$Sepal.Length
# X <- model.matrix(~Sepal.Width+Petal.Length+Petal.Width+Species,iris)

## initial values
beta <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

# randomly initialize a value to beta
# set.seed(100)

# create a vector to contain all beta's for all steps
beta.All <- matrix("numeric",length(beta),maxiter)

# gradient descent method to find the minimum
iter  <- 1
beta0 <- 0*beta
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,Y,X)
  beta.All[,iter] <- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}

# print result and plot all xs for every iteration
print(iter)
print(paste("The minimum of f(beta,y,X) is ", beta, sep = ""))


# Q7

# Compute βˆOLS using nloptr’s L-BFGS algorithm. Do it again using the Nelder-Mead algorithm. Do your answers differ?

## Our objective function
objfun <- function(beta,y,X) {
  return (sum((Y-X%*%beta)^2))
  # equivalently, if we want to use matrix algebra:
  # return ( crossprod(y-X%*%beta) )
}

## Gradient of our objective function
gradient <- function(beta,y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}

## read in the data
# y <- iris$Sepal.Length
# X <- model.matrix(~Sepal.Width+Petal.Length+Petal.Width+Species,iris)

## initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=Y,X=X)
print(result)

## Our objective function
objfun <- function(beta,y,X) {
  return (sum((Y-X%*%beta)^2))
  # equivalently, if we want to use matrix algebra:
  # return ( crossprod(y-X%*%beta) )
}

## Gradient of our objective function
gradient <- function(beta,y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}

## read in the data
# y <- iris$Sepal.Length
# X <- model.matrix(~Sepal.Width+Petal.Length+Petal.Width+Species,iris)

## initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=Y,X=X)
print(result)

# Q8

# Now compute βˆ MLE using nloptr’s L-BFGS algorithm. The code for the gradient vector of this problem is listed below:
#  gradient <- function (theta ,Y,X) {
#     grad <- as. vector ( rep (0, length (theta )))
#     beta <- theta [1:( length ( theta) -1)]
#     sig <- theta [ length (theta )]
#     grad [1:( length ( theta) -1)] <- -t(X)%*%(Y - X%*%beta )/(sig ^2)
#     grad[ length (theta )] <- dim (X)[1] /sig - crossprod (Y-X%*%beta )/(sig^3)
#    return ( grad )
#  }

## Our objective function
objfun  <- function(theta,y,X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((Y-X%*%beta)/sig)^2) ) 
  return (loglike)
}

## Gradient of the objective function
gradient <- function (theta,y,X) {
  grad     <- as.vector(rep(0,length(theta)))
  beta     <- theta [1:(length(theta)-1)]
  sig      <- theta [length(theta)]
  grad[1:(length(theta)-1)] <- -t(X)%*%(Y - X%*%beta)/(sig^2)
  grad[length(theta)]       <- dim(X)[1]/sig-crossprod (Y-X%*%beta)/(sig^3)
  return ( grad )
}

## initial values
theta0 <- runif(dim(X)[2]+1) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e4)

## Optimize!
result <- nloptr( x0=theta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=Y,X=X)
print(result)
betahat  <- result$solution[1:(length(result$solution)-1)]
sigmahat <- result$solution[length(result$solution)]

# Q9

# Now compute βˆOLS the easy way: using lm() and directly calling the matrices Y and X (no need to create a data frame). Make sure you tell lm() not to include the constant! This is done by typing lm(Y ~ X -1)
# Use modelsummary to export the regression output to a .tex file. In your .tex file, tell me about how similar your estimates of βˆ are to the “ground truth” β that you used to create the data in (1).

library(modelsummary)
q.9 <- lm(Y ~ X-1)
model <- modelsummary(q.9, output = "latex")
print(model)
