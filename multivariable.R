# Multivariable regression

# First create 3 normal distributions of n values
n = 100; x = rnorm(n); x2 = rnorm(n); x3 = rnorm(n)

# Second create a multivariable linear function with some little noise
y = 1 + x + x2 + x3 + rnorm(n, sd = .1)

# Thirth find the residuals
ey = resid(lm(y ~ x2 + x3))
ex = resid(lm(x ~ x2 + x3))
sum(ey * ex) / sum(ex ^ 2)
# coef(lm(ey ~ ex - 1)) the same above regression to the origin R^2 (I think)

# The coefficients for each variable and the intercept
coef(lm(y ~ x + x2 + x3)) 