# Linear Regression Models

function1 <- function(){
    library(UsingR)
    data(diamond)
    library(ggplot2)
    g = ggplot(diamond, aes(x = carat, y = price))
    g = g + xlab("Mass (carats)")
    g = g + ylab("Price (SIN $)")
    g = g + geom_point(size = 7, colour = "black", alpha=0.5)
    g = g + geom_point(size = 5, colour = "blue", alpha=0.2)
    g = g + geom_smooth(method = "lm", colour = "black")
    g  # If you ommit paramters in lm it takes y as outcome and x as predictor
}

modelData <- function(){
    fit <- lm(price ~ carat, data = diamond)
    # summary(fit)
    coef(fit) # slope and intercept
    # (Intercept)       carat 
    # -259.6259   3721.0249
    # This means we estimate an expected 3721.0249 (SIN) dollar increase in the
    # price for every carat increase in mass of diamond and the intercept (-259.6259)
    # is the expected value for a 0 carat diamond (nobody cares about a diamond of o carats)
}

meanCentered <- function(){
    # Let's mean center the X variable to interpret in a better way the intercept
    fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
    coef(fit2) # I function allows you to write arithmetic operation in lm function
    # (Intercept) I(carat - mean(carat)) 
    # 500.0833              3721.0249
    # Slope is still the same, but the intercept changes the value for the expected value
    # of an average diamond (~ 0.2 carats)
}

scalingData <- function(){
    fit3 <- lm(price ~ I(carat * 10), data = diamond)
    coef(fit3) # If we want the results based on 1/10 units of carats
    #  (Intercept) I(carat * 10) 
    # -259.6259      372.1025 
}

predictValues <- function(){
    newx <- c(0.16, 0.27, 0.34) # Diamonds
    coef(fit)[1] + coef(fit)[2] * newx # The model to predict the values of diamonds
    # predict(fit, newdata = data.frame(carat = newx)) does the same
}