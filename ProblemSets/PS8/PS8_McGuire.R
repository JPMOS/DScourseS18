library(nloptr)
library(MASS)
library(stargazer)

##############################################
#  4.     C R E A T E    D A T A
##############################################

set.seed(100)
x <- matrix(rnorm(100000 * 9), nrow = 100000, ncol=9)
head(x)
one <- rep(1, 100000)

x <- cbind(one, x)
head(x)

eps <- (rnorm(100000, mean = 0, sd = 0.5))

beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

Y <- x %*% beta + eps

##############################################
#  5. C L O S E D   F O R M    S O L U T I O N
##############################################
beta1 <- ginv(crossprod(x)) %*% crossprod(x, Y)
beta1

# How does it differ?
# It's off by a slight degree. 


##############################################
#   6.  G R A D I E N T     D E S C E N T
##############################################

alpha <- 0.0000003
maxiter <- 500000

objfun <- function(beta,Y,x) {
  return (sum((Y-x%*%beta)^2))
}

gradient <- function(beta,Y,x) {
  return ( as.vector(-2*t(x)%*%(Y-x%*%beta)) )
}

beta2 <- runif(length(beta))
beta2.All <- matrix("numeric",length(beta), iter)

iter <- 1
beta0 <- 0 * beta
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,Y,x)
  beta.All[,iter] <- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}

print(iter)
print(paste("The minimum of f(beta,y,X) is ", beta, sep = ""))




##############################################
#   7.  L - B F G S   &  N E L D E R - M E A D
##############################################

options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"= 0.0000003,"maxeval"=1e3)
result <- nloptr(x0=beta0,  eval_f=objfun,  eval_grad_f=gradient,  opts=options,  Y=Y, x=x)
print(result)


options2 <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=0.0000003)
result2 <- nloptr(x0=beta0,  eval_f=objfun,  eval_grad_f=gradient,  opts=options2,  Y=Y, x=x)
print(result2)

# Answers differ, Nelder-Mead is closer to perfect. 


NLOPT_LD_LBFGS


##############################################
#   8.   Beta MLE      L-BFGS  
##############################################

theta <- beta # Where does theta come from?

objfun2  <- function(theta,Y,x) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((Y-x%*%beta)/sig)^2) ) 
  return (loglike)
}

## initial values
theta0 <- beta0 #start at uniform random numbers equal to number of coefficients

gradient2 <- function(theta,Y, x) {
  grad <- as.vector( rep(0, length(theta)))
  beta <- theta[1:( length(theta) -1)]
  sig <- theta[ length(theta )]
  grad[1:( length(theta) -1)] <- -t(x) %*% (Y - x%*%beta)/(sig^2)
  grad[ length(theta)] <- dim(x)[1] /sig - crossprod(Y-x%*%beta )/(sig^3)
  return ( grad )
}
gradient2

options3 <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"= 0.0000003,"maxeval"=1e3)
result3 <- nloptr(x0=theta0,  eval_f=objfun2,  eval_grad_f=gradient2,  opts=options3,  Y=Y, X=x)
print(result3)

betahat  <- result3$solution[1:(length(result$solution)-1)]
sigmahat <- result3$solution[length(result$solution)]


##############################################
#   9.               O L S  
##############################################


fit1 <- lm(Y ~ x -1)
coefficients(fit1)
stargazer(fit1)

