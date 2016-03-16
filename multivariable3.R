# Multivariable Regression examples

# Load libraries
library(datasets)
library(ggplot2)
library(dplyr)

# Load data
data(swiss)

# I set just two values to religion due to the behaivor that they had in
# that time just catholics and protestants, you can see that in a 
# hist(swiss$Catholic)
binaryReligion <- function(){
    swiss = mutate(swiss, CatholicBin = 1 * (Catholic > 50))
}

# This is a plot of Fertility against % in Agriculture where the colors 
# represent the religion
plotBinaryData <- function(swiss){
    g = ggplot(swiss, aes(x = Agriculture, y = Fertility, colour = factor(CatholicBin)))
    g = g + geom_point(size = 6, colour = "black") + geom_point(size = 4)
    g = g + xlab("% in Agriculture") + ylab("Fertility")
    g
}

# Without having in account religion
dataNoReligion <- function(swiss){
    summary(lm(Fertility ~ Agriculture, data = swiss))$coef
    
    fit = lm(Fertility ~ Agriculture, data = swiss)
    g1 = g
    g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
    g1
}

# As 1 means catholics and 0 protestants, the intercept will be the data for 
# protestants, and the Catholic-1 row added to the intercept is the Catholic data
dataWithReligion <- function(swiss, g){
    summary(lm(Fertility ~ Agriculture + factor(CatholicBin), data = swiss))$coef
    
    # Parallel lines, but different slopes
    fit = lm(Fertility ~ Agriculture + factor(CatholicBin), data = swiss)
    g1 = g
    g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
    g1 = g1 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3], slope = coef(fit)[2], size = 2)
    g1
    
    # Different intercepts and slopes
    summary(lm(Fertility ~ Agriculture * factor(CatholicBin), data = swiss))$coef
    
    fit = lm(Fertility ~ Agriculture * factor(CatholicBin), data = swiss)
    g1 = g
    g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
    g1 = g1 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3], 
                          slope = coef(fit)[2] + coef(fit)[4], size = 2)
    g1
    
    
}