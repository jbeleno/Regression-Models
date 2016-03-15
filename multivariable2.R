# Load libraries
require(datasets)
require(GGally)
require(ggplot2)

data(swiss)
data(InsectSprays)

# Information about the dataset
info <- function(){
    ?swiss
}

# Quick overview in correlations in each variable against others
quickCorrelations <- function(){
    g = ggpairs(swiss, lower = list(continuous = wrap(my_fn, method="lm")))
    g
}

# Function to return points and geom_smooth
# allow for the method to be changed
my_fn <- function(data, mapping, method="loess", ...){
    p <- ggplot(data = data, mapping = mapping) + 
        geom_point() + 
        geom_smooth(method=method, ...)
    p
}

# To get fertility as outcome and anything else as predictor
fertilityCoarseModel <- function(){
    # The point in lm means anything else, in variable terms
    summary(lm(Fertility ~ . , data = swiss))$coefficients
    
    #                   Estimate  Std. Error   t value     Pr(>|t|)
    # (Intercept)      66.9151817 10.70603759  6.250229 1.906051e-07
    # Agriculture      -0.1721140  0.07030392 -2.448142 1.872715e-02
    # Examination      -0.2580082  0.25387820 -1.016268 3.154617e-01
    # Education        -0.8709401  0.18302860 -4.758492 2.430605e-05
    # Catholic          0.1041153  0.03525785  2.952969 5.190079e-03
    # Infant.Mortality  1.0770481  0.38171965  2.821568 7.335715e-03
    #
    # Our models estimates an expected 0.17 decrease in standardized fertility 
    # for every 1% increase in percentage of males involved in agriculture in 
    # holding the remaining variables constant.
    #
    # Education and examination are correlated, but probably one is a function 
    # from the other so it's bad to include both in a model.
}

# Fertility as outcome and Agriculture as predictor
fertilityGranularityModel <- function(){
    # The data is now in agriculture real units
    summary(lm(Fertility ~ Agriculture, data = swiss))$coefficients
}

# Controled scenary
example1 <- function(){
    n <- 100 
    x2 <- 1 : n
    x1 <- .01 * x2 + runif(n, -.1, .1)
    y = -x1 + x2 + rnorm(n, sd = .01)
    
    # Against one variable
    summary(lm(y ~ x1))$coef
    
    # Against two variables
    summary(lm(y ~ x1 + x2))$coef
}

plotExample1 <- function(y, x1, x2){
    dat = data.frame(y = y, x1 = x1, x2 = x2, ey = resid(lm(y ~ x2)), 
                     ex1 = resid(lm(x1 ~ x2)))
    
    g = ggplot(dat, aes(y = y, x = x1, colour = x2))
    g = g + geom_point(colour="grey50", size = 5) 
        + geom_smooth(method = lm, se = FALSE, colour = "black") 
    g = g + geom_point(size = 4) 
    g
}

# Here we can see an strong negative curve that we didn't have in account
plotResidualsExample1 <- function(dat){
    g2 = ggplot(dat, aes(y = ey, x = ex1, colour = x2))  
    g2 = g2 + geom_point(colour="grey50", size = 5) 
        + geom_smooth(method = lm, se = FALSE, colour = "black") 
        + geom_point(size = 4) 
    g2
}

# Unnecessary variables in a linear model
# NA as result for those variables
unnecesaryVariables <- function(){
    z <- swiss$Agriculture + swiss$Education
    lm(Fertility ~ . + z, data = swiss)
    
    # Call:
    # lm(formula = Fertility ~ . + z, data = swiss)
    
    # Coefficients:
    #    (Intercept)       Agriculture       Examination   
    #       66.9152           -0.1721           -0.2580
    #   Education          Catholic  Infant.Mortality z 
    #    -0.8709            0.1041            1.0770  NA 
}

# Plot about sprays
plotExample2 <- function(){
    g = ggplot(data = InsectSprays, aes(y = count, x = spray, fill  = spray))
    g = g + geom_violin(colour = "black", size = 2)
    g = g + xlab("Type of spray") + ylab("Insect count")
    g
}

# Spray A disapear and that means that everything is compared against
# Spray A, example C - A ~ -12, and the intercept is the mean for Spray A
factorModelSpray <- function(){
    summary(lm(count ~ spray, data = InsectSprays))$coef
    
    # The same that
    summary(lm(count ~ 
                   I(1 * (spray == 'B')) + I(1 * (spray == 'C')) + 
                   I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
                   I(1 * (spray == 'F'))
               , data = InsectSprays))$coef
    # When I use I(1*(spray == 'B')) mean I'm forcing the factor to convert it
    # in a numeric value
    
    # To avoid this, I need to remove the intercept
    # And this shit works dude, but the code above works when you
    # want to compare p-values from one variable against other variables
    # Because p-values here are just compared against zero
    summary(lm(count ~ spray - 1, data = InsectSprays))$coef
    library(dplyr)
    summarise(group_by(InsectSprays, spray), mn = mean(count))
    
    # Finally if you want to relevel the factor use
    spray2 <- relevel(InsectSprays$spray, "C")
    summary(lm(count ~ spray2, data = InsectSprays))$coef
}