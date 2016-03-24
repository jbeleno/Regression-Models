# Poisson distribution: is used when counting something per a unit of time
library(sandwich)

# The Poisson tends to a normal as lambda gets large.
example1 <- function(){
    par(mfrow = c(1, 3))
    plot(0 : 10, dpois(0 : 10, lambda = 2), type = "h", frame = FALSE)
    plot(0 : 20, dpois(0 : 20, lambda = 10), type = "h", frame = FALSE)
    plot(0 : 200, dpois(0 : 200, lambda = 100), type = "h", frame = FALSE) 
}

# Mean = Variance
# We can use Julian Days to enhace the work of plot hits in a website per day
example2 <- function(){
    x <- 0 : 10000; lambda = 3
    mu <- sum(x * dpois(x, lambda = lambda))
    sigmasq <- sum((x - mu)^2 * dpois(x, lambda = lambda))
    c(mu, sigmasq)
}

webHits <- function(){
    download.file("https://dl.dropboxusercontent.com/u/7710864/data/gaData.rda",
                  destfile="./data/gaData.rda",method="curl")
    load("./data/gaData.rda")
    gaData$julian <- julian(gaData$date)
    head(gaData)
    
    # Simple plot
    plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
    
    # First try using a linear regression
    plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
    lm1 <- lm(gaData$visits ~ gaData$julian)
    abline(lm1,col="red",lwd=3)
    
    # Getting coefficients in this model, the problem is that on the first day of the site
    # The site didn't have so much traffic and the intercept is zero because that, the
    # slope means the increment of hits per day, we added + 1 (MACHETE) because log can't be 0, but is
    # highly recommended to start from the days where the site started with visits, but for 
    # now is expected a 0.2% of increase of visits per day
    # Log = Ln is an excelent tool to see some data
    round(exp(coef(lm(I(log(gaData$visits + 1)) ~ gaData$julian))), 5)
    # (Intercept) gaData$julian 
    # 0.00000       1.00231 
    
    # Now applying a poisson regression, we can see our line has a small curve and that was
    # we wanted
    plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
    glm1 <- glm(gaData$visits ~ gaData$julian,family="poisson")
    abline(lm1,col="red",lwd=3); lines(gaData$julian,glm1$fitted,col="blue",lwd=3)
    
    # Residuals for poisson regression, we can see variance is higher for lower mean values
    # That's a problem bro, sometimes we can use a quase-poisson regression asumming 
    # our variance is just a constant multiplied by the mean
    plot(glm1$fitted,glm1$residuals,pch=19,col="grey",ylab="Residuals",xlab="Fitted")
    
    # HOW DO I DO TO KNOW WHICH MODEL PICK???? 
    
    # Quase-Poisson
    confint(glm1)
    
    # Sandwich variance estimator
    confint.agnostic(glm1)
    
    # We also can try to stimate rates like %visit per day based on the total amount of
    # visits in the site, also remember that we need to add a constant to avoid log(0)
    glm2 <- glm(gaData$simplystats ~ julian(gaData$date),offset=log(visits+1),
                family="poisson",data=gaData)
    plot(julian(gaData$date),glm2$fitted,col="blue",pch=19,xlab="Date",ylab="Fitted Counts")
    points(julian(gaData$date),glm1$fitted,col="red",pch=19)
    
    # Fitted model
    glm2 <- glm(gaData$simplystats ~ julian(gaData$date),offset=log(visits+1),
                family="poisson",data=gaData)
    plot(julian(gaData$date),gaData$simplystats/(gaData$visits+1),col="grey",xlab="Date",
         ylab="Fitted Rates",pch=19)
    lines(julian(gaData$date),glm2$fitted/(gaData$visits+1),col="blue",lwd=3)
    
    # When the zero value occurs often is necessary to check zero-inflation data
}

# When Poisson fails we can use the sandwich variance estimator for generalized estimating
# equations made in Jhon Hopkins University
confint.agnostic <- function (object, parm, level = 0.95, ...)
{
    cf <- coef(object); pnames <- names(cf)
    if (missing(parm))
        parm <- pnames
    else if (is.numeric(parm))
        parm <- pnames[parm]
    a <- (1 - level)/2; a <- c(a, 1 - a)
    pct <- stats:::format.perc(a, 3)
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
                                                               pct))
    ses <- sqrt(diag(sandwich::vcovHC(object)))[parm]
    ci[] <- cf[parm] + ses %o% fac
    ci
}