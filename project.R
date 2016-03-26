# Final project

# Load libraries
require(dplyr)
require(ggplot2)
require(GGally)

# Some exploratory analysis 
?mtcars

head(mtcars)
str(mtcars)

# Convert the factor data from numeric values
mtcars <- mutate(mtcars, am = factor(am, labels = c("automatic", "manual")))

# Quick overview of data correlations
g = ggpairs(mtcars, lower = list(continuous = wrap(my_fn, method="lm")))
g

# BoxPlot
g <- ggplot(mtcars, aes(x=am, y=mpg), colour = am)
g <- g + geom_boxplot()
g <- g + xlab("% in Agriculture") + ylab("Fertility")
g

# Fine model
model1 <- lm(mpg ~ am, data = mtcars)
model2 <- lm(mpg ~ am + cyl, data = mtcars)
model3 <- lm(mpg ~ am + disp, data = mtcars)
model4 <- lm(mpg ~ am + hp, data = mtcars)
model5 <- lm(mpg ~ am + drat, data = mtcars)
model6 <- lm(mpg ~ am + wt, data = mtcars)
model7 <- lm(mpg ~ am + qsec, data = mtcars)
model8 <- lm(mpg ~ am + vs, data = mtcars)
model9 <- lm(mpg ~ am + gear, data = mtcars)
model10 <- lm(mpg ~ am + gear, data = mtcars)

anova(model1, model2) # p-value: 8.01e-07
anova(model1, model3) # p-value: 5.748e-07
anova(model1, model4) # p-value: 2.92e-08
anova(model1, model5) # p-value: 0.0107 (Rejected)
anova(model1, model6) # p-value: 1.867e-07
anova(model1, model7) # p-value: 6.271e-06
anova(model1, model8) # p-value: 6.501e-06
anova(model1, model9) # p-value: 0.03733 (Rejected)
anova(model1, model10)# p-value: 0.03733 (Rejected)

# This didn't work bro
model11 <- lm(mpg ~ am + cyl + disp + hp + wt + qsec + vs, data = mtcars)


# Residuals
par(mfrow=c(2, 2))
plot(fit)

# T-test
t.test( mpg ~ am, data = mtcars)