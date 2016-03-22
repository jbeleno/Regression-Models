# Logistic Regression

# Download data from Dropbox and load it 
# Ravens are US American Football Team
#download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda"
#              , destfile="./data/ravensData.rda",method="curl")
load("./data/ravensData.rda")
head(ravensData)

library(manipulate)

# To understand how the logistic regression works we can visualize an interactive
# graphic to see the likelihood of values create the curve that is shown, if
# exist more probabilities to get a zero, the the curve will tend to zero values
# if not it'll tend to one values
example1 <- function(){
    x <- seq(-10, 10, length = 1000)
    manipulate(
        plot(x, exp(beta0 + beta1 * x) / (1 + exp(beta0 + beta1 * x)), 
             type = "l", lwd = 3, frame = FALSE),
        beta1 = slider(-2, 2, step = .1, initial = 2),
        beta0 = slider(-2, 2, step = .1, initial = 0)
    )
}


logistic_regression_ravens <- function(){
    logRegRavens <- glm(ravensData$ravenWinNum ~ ravensData$ravenScore,
                        family="binomial")
    summary(logRegRavens)
    
    # Plot the curve generated with the predictor, the curve is cut by
    # some values as we can see the plot doesn't reach zero values in 
    # y-axis and x-axis
    plot(ravensData$ravenScore,logRegRavens$fitted,pch=19,col="blue",
         xlab="Score",ylab="Prob Ravens Win")
    
    # The slope suggest that exist a 11% of probabilities of Ravens win
    # a match per each aditional point scored
    exp(logRegRavens$coeff)
    # (Intercept) ravensData$ravenScore 
    # 0.1863724             1.1124694 
    
    # Confident intervals
    exp(confint(logRegRavens))
    #                             2.5 %   97.5 %
    # (Intercept)           0.005674966 3.106384
    # ravensData$ravenScore 0.996229662 1.303304
    
    # ANOVA works as Linear Model and is really useful when we have factors
    # variables and nested models
    anova(logRegRavens,test="Chisq")
}