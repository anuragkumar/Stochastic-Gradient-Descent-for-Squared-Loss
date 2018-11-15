stochasticGradientDescentMSE <- function(X,Y, n, learning_rate=0.01, epsilon=0.00001, iterations=10000)
{
  X <- cbind(1, X) # 50*2
  theta <- matrix(0, nrow=2, ncol=1) #2*1
  i <- 0
  old_loss <- 0
  while( i < iterations)
  {
    #find a random variable between 1 to n
    #find X[random] and Y[random]
    #calculate error
    #calculate gradient descent
    random <- runif(1,1,n)
    
    gradientDescent <- -(2)*X[random,]%*%(Y[random,] - X[random,]%*%theta)
    theta <- theta - learning_rate*gradientDescent
    mse_loss <- sum((Y - X%*%theta)^2)
    if(abs(mse_loss - old_loss) < epsilon)
    {
      break
    }
    old_loss <- mse_loss
    i <- i+1
  }
  return(theta)
}

x<-runif(50, min = -2, max = 2)
x<- matrix(x)
error <- rnorm(50, 0, 2)
error<- matrix(error)
y <- 2+3*x + error
stochasticGradientDescentMSE(x,y,50)
