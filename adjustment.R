# Adjustment

# This is the ideal situation where I collect data for the almost the same
# x variable and show the results in a well fitted line, marginal effect is
# almost the same with the differences of intercept.
simulation1 <- function(){
    n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2));
    beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
    y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
    plot(x, y, type = "n", frame = FALSE)
    abline(lm(y ~ x), lwd = 2)
    abline(h = mean(y[1 : (n/2)]), lwd = 3)
    abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
    fit <- lm(y ~ x + t)
    abline(coef(fit)[1], coef(fit)[2], lwd = 3)
    abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
    points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
    points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
}

# Simulation 2 isn't randomized, but we apply a treatment in both cases
# depending on the value of X, this is hard to compare, because there is no
# overlap
simulation2 <- function(){
    n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), 1.5 + runif(n/2));
    beta0 <- 0; beta1 <- 2; tau <- 0; sigma <- .2
    y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
    plot(x, y, type = "n", frame = FALSE)
    abline(lm(y ~ x), lwd = 2)
    abline(h = mean(y[1 : (n/2)]), lwd = 3)
    abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
    fit <- lm(y ~ x + t)
    abline(coef(fit)[1], coef(fit)[2], lwd = 3)
    abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
    points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
    points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
}

# Simpson Paradox, it's hard to compare too, because althought exist some points 
# that overlaps, sometimes isn't good enought to infere information from that
simulation3 <-function(){
    n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), .9 + runif(n/2));
    beta0 <- 0; beta1 <- 2; tau <- -1; sigma <- .2
    y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
    plot(x, y, type = "n", frame = FALSE)
    abline(lm(y ~ x), lwd = 2)
    abline(h = mean(y[1 : (n/2)]), lwd = 3)
    abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
    fit <- lm(y ~ x + t)
    abline(coef(fit)[1], coef(fit)[2], lwd = 3)
    abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
    points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
    points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
}

# The marginal effect is by far different from the differences in intercept
# and for that reason we can say that we don't have a significant effect from
# one that lead the other
simlation4 <- function(){
    n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(.5 + runif(n/2), runif(n/2));
    beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
    y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
    plot(x, y, type = "n", frame = FALSE)
    abline(lm(y ~ x), lwd = 2)
    abline(h = mean(y[1 : (n/2)]), lwd = 3)
    abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
    fit <- lm(y ~ x + t)
    abline(coef(fit)[1], coef(fit)[2], lwd = 3)
    abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
    points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
    points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
}


# Here is an effect really crazy, but shows us that sometimes things
# should not be interpreted as the text says
simulation5 <- function(){
    n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2, -1, 1), runif(n/2, -1, 1));
    beta0 <- 0; beta1 <- 2; tau <- 0; tau1 <- -4; sigma <- .2
    y <- beta0 + x * beta1 + t * tau + t * x * tau1 + rnorm(n, sd = sigma)
    plot(x, y, type = "n", frame = FALSE)
    abline(lm(y ~ x), lwd = 2)
    abline(h = mean(y[1 : (n/2)]), lwd = 3)
    abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
    fit <- lm(y ~ x + t + I(x * t))
    abline(coef(fit)[1], coef(fit)[2], lwd = 3)
    abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2] + coef(fit)[4], lwd = 3)
    points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
    points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
}

# This shows us a colored difference in a binary treatment
simulation6 <- function(){
    p <- 1
    n <- 100; x2 <- runif(n); x1 <- p * runif(n) - (1 - p) * x2
    beta0 <- 0; beta1 <- 1; tau <- 4 ; sigma <- .01
    y <- beta0 + x1 * beta1 + tau * x2 + rnorm(n, sd = sigma)
    plot(x1, y, type = "n", frame = FALSE)
    abline(lm(y ~ x1), lwd = 2)
    co.pal <- heat.colors(n)
    points(x1, y, pch = 21, col = "black", bg = co.pal[round((n - 1) * x2 + 1)], cex = 2)
}

# Residuals in simulation 6 in 3D
threeDresiduals <- function(x1, x2, y){
    library(rgl)
    plot3d(x1, x2, y)
}