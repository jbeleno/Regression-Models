# Mishmash or Hodgepodge

# This solves any function by aproximation of linear models in each section
# according to the number of partitions in this case 20, but this aproximation
# shows us a function that has some problems with derivate in conection points
splineFunction <- function(){
    n <- 500; x <- seq(0, 4 * pi, length = n); y <- sin(x) + rnorm(n, sd = .3)
    knots <- seq(0, 8 * pi, length = 20); 
    splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
    xMat <- cbind(1, x, splineTerms)
    yhat <- predict(lm(y ~ xMat - 1))
    plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
    lines(x, yhat, col = "red", lwd = 2)
    
    
    # When we use quadratic functions instead of just linear equations in each part
    # we get a derivative function
    splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot)^2)
    xMat <- cbind(1, x, x^2, splineTerms)
    yhat <- predict(lm(y ~ xMat - 1))
    plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
    lines(x, yhat, col = "red", lwd = 2)
    
    # But exist a complete topic about this called regression splines, it's a science
    # to know how many point to set for this model, because it can become in a problem
    # a way to solve this is using regularization(a lot of points)
    
    # If we have to set just a point in our modeland we have a hockye stick like a model
}

fourierFunction <- function(){
    # Chord finder, playing the white keys on a piano from octave c4 - c5
    notes4 <- c(261.63, 293.66, 329.63, 349.23, 392.00, 440.00, 493.88, 523.25)
    
    # 2 seconds of data
    t <- seq(0, 2, by = .001); n <- length(t)
    
    # Package savewav get real data from sound
    # but I'm simulating it
    c4 <- sin(2 * pi * notes4[1] * t)
    e4 <- sin(2 * pi * notes4[3] * t) 
    g4 <- sin(2 * pi * notes4[5] * t)
    
    # Create a chord based on 3 notes
    chord <- c4 + e4 + g4 + rnorm(n, 0, 0.3)
    
    # I get all the functions of notes
    x <- sapply(notes4, function(freq) sin(2 * pi * freq * t))
    
    # And check the results of applying a linear regression
    fit <- lm(chord ~ x - 1)
    
    # Plot the results
    plot(c(0, 9), c(0, 1.5), xlab = "Note", ylab = "Coef^2", axes = FALSE, frame = TRUE, type = "n")
    axis(2)
    axis(1, at = 1 : 8, labels = c("c4", "d4", "e4", "f4", "g4", "a4", "b4", "c5"))
    for (i in 1 : 8) abline(v = i, lwd = 3, col = grey(.8))
    lines(c(0, 1 : 8, 9), c(0, coef(fit)^2, 0), type = "l", lwd = 3, col = "red")
    
    # (How you would really do it), this is because FFT is faster than linear models
    a <- fft(chord); plot(Re(a)^2, type = "l")
    
    
    # To learn
    # Generalized Linear Models
    # Longitudinal multi-level data
}