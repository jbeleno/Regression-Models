# Regression
#
# Basic Least Squares
# Let Yi be the value for i, where i = 1,...,n then the middle is the value
# of μ that minimizes
# SUM((Yi - μ)^2)
# Physical center of mass of the histogram
# μ = Ym
library(UsingR)
library(manipulate)
library(dplyr)
data(galton)

initChildrenHeight <- function(){
    manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))
}

myHist <- function(mu){
    mse <- mean((galton$child - mu)^2) # Mean Square Error
    g <- ggplot(galton, aes(x = child)) + geom_histogram(
                                            fill = "salmon", colour = "black", 
                                            binwidth=1)
    g <- g + geom_vline(xintercept = mu, size = 3)
    g <- g + ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 2), sep = ""))
    g
}

lee_mean <- function(){
    g <- ggplot(galton, aes(x = child)) + geom_histogram(
                                            fill = "salmon", colour = "black", 
                                            binwidth=1)
    g <- g + geom_vline(xintercept = mean(galton$child), size = 3)
    g
}

# In the example we want to find the value of β (slope) that minimize the value
# of SUM((Yi -Xi*β)) -> least squares for parents and children height

# Throught the origin
substractionDataToSolve <- function(){
    y <- galton$child - mean(galton$child)
    x <- galton$parent - mean(galton$parent)
    freqData <- as.data.frame(table(x, y))
    names(freqData) <- c("child", "parent", "freq")
    freqData$child <- as.numeric(as.character(freqData$child))
    freqData$parent <- as.numeric(as.character(freqData$parent))
    
    manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))
}

myPlot <- function(beta){
    g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
    g <- g  + scale_size(range = c(2, 20), guide = "none" )
    g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
    g <- g + geom_point(aes(colour=freq, size = freq))
    g <- g + scale_colour_gradient(low = "lightblue", high="white")                     
    g <- g + geom_abline(intercept = 0, slope = beta, size = 3)
    mse <- mean( (y - beta * x) ^2 )
    g <- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
    g
}

solution <- function(){
    lm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)
}
