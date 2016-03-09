# Inference
# Load libraries
library(UsingR)
library(ggplot2)
data(diamond)

# Define main variables
y <- diamond$price; x <- diamond$carat; n <- length(y)

# The hard way
summaryHardWay <- function(){
    beta1 <- cor(y, x) * sd(y) / sd(x)
    beta0 <- mean(y) - beta1 * mean(x)
    e <- y - beta0 - beta1 * x
    sigma <- sqrt(sum(e^2) / (n-2)) 
    ssx <- sum((x - mean(x))^2)
    seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ .5 * sigma 
    seBeta1 <- sigma / sqrt(ssx)
    tBeta0 <- beta0 / seBeta0; tBeta1 <- beta1 / seBeta1
    pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
    pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)
    coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
    colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
    rownames(coefTable) <- c("(Intercept)", "x")
    coefTable
}

# The easy way
summaryEasyWay <- function(){
    fit <- lm(y ~ x); 
    summary(fit)$coefficients
}

# Confidence intervals
confidenceIntervals <- function(){
    sumCoef <- summaryEasyWay()
    sumCoef[1,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[1, 2]
    (sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2]) / 10
}

# Prediction interval != Confidence interval, prediction is wide than confidence
predictionAndConfidence <- function(){
    fit <- lm(y ~ x)
    
    newx = data.frame(x = seq(min(x), max(x), length = 100))
    p1 = data.frame(predict(fit, newdata= newx,interval = ("confidence")))
    p2 = data.frame(predict(fit, newdata = newx,interval = ("prediction")))
    p1$interval = "confidence"
    p2$interval = "prediction"
    p1$x = newx$x
    p2$x = newx$x
    dat = rbind(p1, p2)
    names(dat)[1] = "y"
    
    g = ggplot(dat, aes(x = x, y = y))
    g = g + geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.2) 
    g = g + geom_line()
    g = g + geom_point(data = data.frame(x = x, y=y), aes(x = x, y = y), size = 4)
    g
    
}