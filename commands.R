# Linear Regression

resid <- function(y_hat, y){
  return(y - y_hat)
}

## The Analytical solution
l_regression <- function(X, y){
  X <- cbind(1, X)
  A <- solve(t(X) %*% X) # calculates the inverse
  theta <- A %*% t(X) %*% y
  return(theta)
}


X <- cars$speed
y <- cars$dist

theta <- l_regression(X, y)
theta

sum(resid(cbind(1,X) %*% theta, y))



## The Gradient Descent approach:


grad_desc <- function(X, y, alpha = 0.006, epsilon = 10^-10){
  i <- 0
  X <- cbind(1, X)
  theta <- matrix(data = 1, nrow = ncol(X), ncol= 1)
  cost <- (1/(2*nrow(X))) * t(X %*% theta - y) %*% (X %*% theta - y)
  print(cost)
  delta <- 1
  while(delta > epsilon){
    i <- i + 1
    theta <- theta - (alpha / nrow(X)) * (t(X) %*% (X %*% theta - y))
    cval <- (1/(2*nrow(X))) * t(X %*% theta - y) %*% (X %*% theta - y)
    cost <- append(cost, cval)
    delta <- abs(cost[i+1] - cost[i])
  }
  print(sprintf("Completed in %i iterations.", i))
  return(theta)
}

X <- cars$speed
y <- cars$dist

theta <- grad_desc(X, y)
sum(resid(cbind(1,X) %*% theta, y))
theta

## The R function:

lm_model <- lm(y ~ X) 
theta <- summary(lm_model)$coefficients[,1]
theta

sum(resid(cbind(1,X) %*% theta, y))



head(mtcars)
summary(mtcars)

barplot(mtcars$mpg, names.arg =row.names(mtcars))


pairs(mtcars, 
      lower.panel = NULL)

plot(y = mtcars$mpg, x = mtcars$disp)
formula <- mtcars$mpg ~ mtcars$disp
model <- lm(formula)

mpg = -0.041 * disp + 29.6

plot(y = mtcars$mpg, x = mtcars$disp)
abline(model)
coef(model)

adv <- read.csv("Advertising.csv", header=T,
                colClasses = c("NULL", NA, NA, NA, NA))
adv2 <- read.csv("Advertising.csv")

head(adv)


adv.lm <- lm(adv$sales ~ adv$TV)
summary(adv.lm)

adv.lm2 <- lm(adv$sales ~ adv$newspaper)
summary(adv.lm2)

plot(x=adv$newspaper, y=adv$sales)
abline(adv.lm2)

complex_formula <- sales ~ TV + radio + newspaper
adv.lm3 <- lm(formula=complex_formula, data = adv) 
summary(adv.lm3)
install.packages("car")
library(car)

prestige.dataset <- Prestige

head(prestige.dataset)

pairs(prestige.dataset, lower.panel = NULL)

prestige.lm1 <- lm(prestige ~ education, data=prestige.dataset)

summary(prestige.lm1)

prestige.lm2 <- lm(prestige ~ income, 
                   data=prestige.dataset)
summary(prestige.lm2)

prestige.lm3 <- lm(prestige ~ education + income,
                   data = prestige.dataset)

summary(prestige.lm3)

pop <- data.frame(uspop)
head(pop)
pop$year <- seq(from=1790, to=1970, by=10)
pop$uspop <- as.numeric(pop$uspop)
head(pop)
plot(x = pop$year, y= pop$uspop)
pop.lm1 <- lm(uspop ~ year + I(year^2), 
              data=pop)
summary(pop.lm1)
plot(x = pop$year, y= pop$uspop)
abline(pop.lm1)

plot(y=residuals(pop.lm1), x=fitted(pop.lm1))
abline(a=0,b=0)
pop.lm2 <- lm(uspop ~ poly(year, 2), 
              data=pop)

summary(pop.lm2)
plot(y=residuals(pop.lm2), 
     x=fitted(pop.lm2))
abline(a=0,b=0)

plot(y=pop$uspop, x=pop$year)
lines(sort(pop$year), 
      fitted(pop.lm2)[order(pop$year)])




