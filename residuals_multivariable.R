# Load libraries
library(datasets)
library(ggplot2)

# Load data
data(swiss)

# I think that these concepts will be covered later
linearModel <- function(){
    par(mfrow = c(2, 2))
    fit <- lm(Fertility ~ . , data = swiss); 
    plot(fit)
    
    # O RLY? plot is the first one (Residuals vs Fitted values)
    # Looking for heteroskadisity or some pattern 
    #
    # QQ Normal is to look normality of errors
    #
    # The third graphic is like the first one, but standarized
    # to compare it later with other values
    # 
    # The fourth is for getting Residuals vs Leverage and also
    # is for looking patterns, low residuals and high leverage or
    # high residual and high leverage
}

# This is a plot to explain the concepts of influence and leverage
# of a point to know when a point is outlier
leverageAndInfluence <- function(){
    par(mfrow = c(1, 1))
    n <- 100; x <- rnorm(n); y <- x + rnorm(n, sd = .3)
    plot(c(-3, 6), c(-3, 6), type = "n", frame = FALSE, xlab = "X", ylab = "Y")
    abline(lm(y ~ x), lwd = 2)
    points(x, y, cex = 2, bg = "lightblue", col = "black", pch = 21)
    points(0, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)
    points(0, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
    points(5, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
    points(5, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)
}

# Residuals have the same units than the outcome Y

# Case 1: A point (10, 10) creates a correlation where doesn't exist
case1 <- function(){
    n <- 100
    x <- c(10, rnorm(n))
    y <- c(10, c(rnorm(n)))
    plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
    abline(lm(y ~ x))
    
    fit <- lm(y ~ x)
    # dfbetas value for the first point (10, 10) is terrible high in 
    # comparision with other values
    round(dfbetas(fit)[1 : 10, 2], 3)
    #  1      2      3      4      5      6      7      8      9     10 
    # 6.037 -0.010 -0.001  0.017 -0.080 -0.006  0.009 -0.002  0.083  0.004 
    
    # hatvalues value for the first point (10, 10) is terrible high in 
    # comparision with other values
    round(hatvalues(fit)[1 : 10], 3)
    # 1     2     3     4     5     6     7     8     9    10 
    # 0.477 0.012 0.010 0.010 0.028 0.010 0.012 0.010 0.018 0.012 
}

# It was added a point far away from others, but that fits with the 
# regression model
case2 <- function(){
    n <- 100
    x <- rnorm(n)
    y <- x + rnorm(n, sd = .3)
    x <- c(5, x)
    y <- c(5, y)
    plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
    fit2 <- lm(y ~ x)
    abline(fit2) 
    
    # In this case dfbetas don't show me anything wrong
    round(dfbetas(fit2)[1 : 10, 2], 3)
    #  1      2      3      4      5      6      7      8      9     10 
    # -0.137 -0.097 -0.017  0.001 -0.015  0.034  0.284 -0.028 -0.032  0.180 
    
    # But hatvalues does
    round(hatvalues(fit2)[1 : 10], 3)
    # 1     2     3     4     5     6     7     8     9    10 
    # 0.212 0.021 0.022 0.025 0.013 0.013 0.037 0.038 0.014 0.030 
}

finalExample <- function(){
    ## Don't everyone hit this server at once.  Read the paper first.
    dat <- read.table('http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt', header = FALSE)
    pairs(dat)
    
    summary(lm(V1 ~ . -1, data = dat))$coef
    # P-values are significant
    
    # O RLY? image with ASCII characters when look for residuals
    fit <- lm(V1 ~ . - 1, data = dat)
    plot(predict(fit), resid(fit), pch = '.')
}