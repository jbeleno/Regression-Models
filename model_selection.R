# Model selection
data("swiss")

# When Y just depends on a variable, the variable doesn't present
# so much variability in its standard error, even when we add more
# variables in the regression model that doesn't depends on the value 
# of the first variable X1 in this case
nonDependenciesSD <- function(){
    n <- 100; nosim <- 1000
    x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n); 
    betas <- sapply(1 : nosim, function(i){
        y <- x1 + rnorm(n, sd = .3)
        c(coef(lm(y ~ x1))[2], 
          coef(lm(y ~ x1 + x2))[2], 
          coef(lm(y ~ x1 + x2 + x3))[2])
    })
    round(apply(betas, 1, sd), 5)
    # x1      x1      x1 
    # 0.03110 0.03141 0.03175 
}

# When a variable that highly affects in the model is added with others
# variables that includes X1, it affects so much the startdard error in
# X1, increasing it in a high factor, for this reason is necessary to ask
# if is necessary to include the other variables in the model, but if we don't 
# maybe our model is biased
dependenciesDS <- function(){
    n <- 100; nosim <- 1000
    x1 <- rnorm(n); x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
    x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2); 
    betas <- sapply(1 : nosim, function(i){
        y <- x1 + rnorm(n, sd = .3)
        c(coef(lm(y ~ x1))[2], 
          coef(lm(y ~ x1 + x2))[2], 
          coef(lm(y ~ x1 + x2 + x3))[2])
    })
    round(apply(betas, 1, sd), 5)
    # x1      x1      x1 
    # 0.03141 0.04192 0.10313 
}

# There is more variance inflation factor (VFI) when is highly correlated with 
# other variables in the model
varianceVFI <- function(){
    library(car)
    fit <- lm(Fertility ~ . , data = swiss)
    vif(fit)
    sqrt(vif(fit)) #I prefer sd 
    
    # Agriculture      Examination        Education         Catholic Infant.Mortality 
    # 1.511334         1.917138         1.665816         1.391819         1.052398 
}

# We can use nested models to obtain data about if we need to include a variable 
# in the model just seeing the p-values in ANOVA,we also can see the degree of freedom
# in each nested model and the change with the before model
nestedModel <- function(){
    fit <- lm(Fertility ~ . , data = swiss)
    
    fit1 <- lm(Fertility ~ Agriculture, data = swiss)
    fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
    fit5 <- update(fit, Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)
    anova(fit1, fit3, fit5)
}