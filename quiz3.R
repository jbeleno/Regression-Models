data("mtcars")
data("cars")

question1<- function(){
    fit <- lm(mpg ~ I(factor(cyl)) + wt, data = mtcars)
    fit$coefficients[3]
}

question2<- function(){
    fit <- lm(mpg ~ I(factor(cyl)) + wt, data = mtcars)
    fit$coefficients[3]
    
    # Without weight as confounding variable
    fit2 <- lm(mpg ~ I(factor(cyl)), data = mtcars)
    fit2$coefficients[3]
}

# Consider the 𝚖𝚝𝚌𝚊𝚛𝚜 data set. Fit a model with mpg as the o
# outcome that considers number of cylinders as a factor variable 
# and weight as confounder. Now fit a second model with mpg as the 
# outcome model that considers the interaction between number of 
# cylinders (as a factor variable) and weight. Give the P-value for 
# the likelihood ratio test comparing the two models and suggest a model
# using 0.05 as a type I error rate significance benchmark.
question3 <- function(){
    fit1 <- lm(mpg ~ I(factor(cyl)) + wt, data = mtcars)
    fit2 <- update(fit1, mpg ~ I(factor(cyl)) + wt + I(factor(cyl))*wt)
    
    anova(fit1, fit2)
}

# Remember that wt is in 1000 lb so I(wt * 0.5) is multiplying by 2 
# i.e. 1 ton
question4 <- function(){
    fit <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
}

question5 <- function(){
    x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
    y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
    
    fit <- lm(y ~ x)
    
    round(hatvalues(fit)[1 : 5], 4)
}

question6 <- function(){
    x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
    y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
    
    fit <- lm(y ~ x)
    
    # First look for high hatvalue
    round(hatvalues(fit)[1 : 5], 4)
    
    # Look for the dfbeta for the high value above
    round(dfbetas(fit)[1 : 5, 2], 3)
}