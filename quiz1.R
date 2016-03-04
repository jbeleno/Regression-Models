# Quiz1

question1 <- function(){
    x <- c(0.18, -1.54, 0.42, 0.95)
    w <- c(2, 1, 3, 1)
    
    weighted.mean(x, w)
}

question2 <- function(){
    x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
    y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
    
    lm(y ~ x - 1)[1]
}

question3 <- function(){
    data("mtcars")
    
    # First parameter is outcome, second is the predictor
    lm(mpg ~ wt, data = mtcars)[1]
}

question6 <- function(){
    x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
    xm <- sum(x)/row(x)
    sd <- sd(x)
    
    y <- (x[1]-xm)/sd
    y
}

question7 <- function(){
    x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
    y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
    
    lm(y ~ x)[1]
}

question9 <- function(){
    x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
    
    mean(x)
}