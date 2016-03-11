# Give a P-value for the two sided hypothesis test of whether 
# β1 from a linear regression model is 0 or not.
question1 <- function(){
    x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
    y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
    fit <- lm(y ~ x)
    summary(fit)$coefficients
    # 0.05296
}

question3 <- function(){
    data("mtcars")
    fit <- lm(mpg ~ wt, data = mtcars)
    x <- c(mean(mtcars$wt))
    predict(fit, newdata = data.frame(wt = x), interval = ("confidence"))
    # lower bound: 18.991
}

question5 <- function(){
    data("mtcars")
    fit <- lm(mpg ~ wt, data = mtcars)
    x <- c(3)
    predict(fit, newdata = data.frame(wt = x), interval = ("prediction"))
    # Upper bound: 27.57355
}


question6 <- function(){
    # Short ton is 2000lb instead of 1000lb by default in wt
    data("mtcars")
    fit <- lm(mpg ~ I(wt/2), data = mtcars)
    sumCoef <- summary(fit)$coefficients
    sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2]
    # Lower: -12.97262
}

# Refer back to the mtcars data set with mpg as an outcome and weight (wt)
# as the predictor. About what is the ratio of the the sum of the squared 
# errors, ∑ni=1(Yi−Ŷ i)2∑i=1n(Yi−Y^i)2 when comparing a model with just an 
# intercept (denominator) to the model with the intercept and slope (numerator)?
question9 <-function(){
    data(mtcars)
    y <- mtcars$mpg
    x <- mtcars$wt
    fit_car <- lm(y ~ x)
    sum(resid(fit_car)^2) / sum((y - mean(y)) ^ 2)
}