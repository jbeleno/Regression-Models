# Linear Least Squares
# coef(lm(Y ~ X)) coefficients for intercept and slope of the linear model
#  y ~ x + 0 or y ~ 0 + x to get rid the intercept in the formula

library(UsingR)
library(manipulate)
library(dplyr)
data(galton)

newGraphic <- function(){
    freqData <- as.data.frame(table(galton$child, galton$parent))
    names(freqData) <- c("child", "parent", "freq")
    freqData$child <- as.numeric(as.character(freqData$child))
    freqData$parent <- as.numeric(as.character(freqData$parent))
    
    y <- galton$child
    x <- galton$parent
    beta1 <- cor(y, x) *  sd(y) / sd(x)
    beta0 <- mean(y) - beta1 * mean(x)
    rbind(c(beta0, beta1), coef(lm(y ~ x)))
    
    beta1 <- cor(y, x) *  sd(x) / sd(y)
    beta0 <- mean(x) - beta1 * mean(y)
    rbind(c(beta0, beta1), coef(lm(x ~ y)))
    
    yc <- y - mean(y)
    xc <- x - mean(x)
    beta1 <- sum(yc * xc) / sum(xc ^ 2)
    c(beta1, coef(lm(y ~ x))[2])
    
    yn <- (y - mean(y))/sd(y)
    xn <- (x - mean(x))/sd(x)
    c(cor(y, x), cor(yn, xn), coef(lm(yn ~ xn))[2])
    
    g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
    g <- g  + scale_size(range = c(2, 20), guide = "none" )
    g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
    g <- g + geom_point(aes(colour=freq, size = freq))
    g <- g + scale_colour_gradient(low = "lightblue", high="white")  
    g <- g + geom_smooth(method="lm", formula=y~x)
    g
}