### Problem Set 8 
### Karley Nadolski
### Econ 5253

library(nloptr)
library(tidyverse)

set.seed(100)

  # X is a N*K matrix, containing normally distributed numbers (first column is 1)
  N <- 10000
  K <- 10
  sigma <- 0.5
  
  X <- matrix(rnorm(N*K,mean=0,sd=sigma),N,K)
  X[,1] <- 1
  
  # eps is a vector of length N with random numbers normally distributed 
        # eps ~ N(0, sig = 0.5)
        eps <- rnorm(N,mean=0,sd=0.5)
        
  # beta is a vector of length 10
        beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
        
  # generate Y 
        # Y = Xbeta + eps
        Y <- X%*%beta + eps

  #5. Using the matrices generated, compute beta hat OLS with matrix algebra
        # beta hat OLS = (X'X)^-1X'y
        library(matlib)
        beta.math <- inv(crossprod(X))%*%(t(X)%*%Y)
        
        results <- tibble(truth = betaTrue, math.est = beta.hat.ols)
        # This shows the relationship between the beta values and the
        # estimates from computing the closed form solution to OLS. 
            # The estimates are very similar to the true values of the betas. 
            # The estimate that is the furthest off from the true value is 
            # still only off by by 0.01.
        
  #6. Compute beta estimates using gradient descent
        # set up a stepsize
        alpha <- 0.0000003
        
        # set up a number of iterations
        maxiter <- 500000
        
        ## Our objective function
        objfun <- function(beta,y,X) {
          return ( sum((y-X%*%beta)^2) )
        }
        
        # define the gradient of our objective function
        gradient <- function(beta,y,X) {
          return (as.vector(-2*t(X)%*%(y-X%*%beta)) )
        }
        
        
        ## read in the data
        Y <- X%*%betaTrue + eps
        X <- matrix(rnorm(N*K,mean=0,sd=sigma),N,K)
        X[,1] <- 1
        
        ## initial values
        beta.g <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients
        
        ?# randomly initialize a value to beta
        set.seed(100)
        
        # create a vector to contain all beta's for all steps
        beta.All <- matrix("numeric",length(beta.g),maxiter)
        
        # gradient descent method to find the minimum
        iter  <- 1
        beta0 <- 0*beta.g
        while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
          beta0 <- beta.g
          beta.g <- beta0 - alpha*gradient(beta0,Y,X)
          beta.All[,iter] <- beta.g
          if (iter%%10000==0) {
            print(beta.g)
          }
          iter <- iter+1
        }
        
        # print result and plot all xs for every iteration
        print(iter)
        print(paste("The minimum of f(beta,y,X) is ", beta.g, sep = ""))
        
        
        results <- tibble(truth = betaTrue, math.est = beta.hat.ols, gradient.est = beta.g)
        
  # 7. Compute beta estimates (OLS) using nloptr's L-BFGS algorithm (first) and
  #    the Nelder-Mead algorithm (second). Do the answers differ? 
        
        # OLS with L-BFGS
            # Setting parameters for the algorithm 
              options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
            # Initial Values 
              beta0 <- runif(dim(X)[2])
        
            # Optimizing with BFGS
              beta.bfgs <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=Y,X=X)
              print(beta.bfgs$solution)
              
        # OLS with Nelder-Mead
            # Setting parameters for Nelder-Mead
              options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)
              
            # Optimizing with Nelder-Mead
              beta.nm <- nloptr(x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=Y,X=X)
              print(beta.nm$solution)
              
              alg.estimates <- tibble("L-BFGS" = beta.bfgs$solution, "Nelder-Mead" = beta.nm$solution)
              alg.estimates
              # The estimates from L-BFGS and Nelder-Mead are the same 
              
              # Comparing all previous methods of estimating OLS beta hats  
              results <- tibble(truth = betaTrue, math.est = beta.hat.ols, gradient.est = beta.g, beta.lbfgs = beta.bfgs$solution, beta.nm = beta.nm$solution)
              
  # 8. Now compute beta hat MLE using L-BFGS
            # Objective function
              objfun  <- function(theta,y,X) {
                # need to slice our parameter vector into beta and sigma components
                beta    <- theta[1:(length(theta)-1)]
                sig     <- theta[length(theta)]
                # write objective function as *negative* log likelihood (since NLOPT minimizes)
                loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
                return (loglike)
              }
              
            # Coding the gradient vector   
              gradient <- function (theta ,Y,X) {
                grad <- as.vector ( rep (0, length ( theta )))
                beta <- theta [1:( length ( theta ) -1)]
                sig <- theta [ length ( theta )]
                grad [1:( length ( theta ) -1)] <- -t(X)%*%(Y - X%*% beta )/(sig ^2)
                grad [ length ( theta )] <- dim (X) [1] /sig - crossprod (Y-X%*% beta )/(sig^3)
                return ( grad )
              }
              
            # Initial values
              theta0 <- runif(dim(X)[2]+1) #start at uniform random numbers equal to number of coefficients
              
            # Algorithm parameters
              options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)

            # Optimizing MLE with L-BFGS
              mle.bfgs <- nloptr( x0=theta0,eval_f=objfun,opts=options,y=Y,X=X)
              print(mle.bfgs)
              betahat.mle  <- mle.bfgs$solution[1:(length(mle.bfgs$solution)-1)]
              sigmahat.mle <- mle.bfgs$solution[length(mle.bfgs$solution)]
              
  # 9. Now use lm() to find the beta hat OLS estimates 
            library(modelsummary)
            beta.lm <- lm(Y ~ X - 1)
            modelsummary(beta.lm, output = "PS8_Nadolski.tex")
              
              
      