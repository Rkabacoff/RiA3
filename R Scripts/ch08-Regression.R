#---------------------------------------------------------------------#
# R in Action (3rd ed): Chapter 8                                     #
# Regression                                                          #
# requires the car, MASS, effects, bootstrap and leaps packages       #                 #
# install.packages(c("car", "MASS", "leaps", "effects", "bootstrap")) #               #
#---------------------------------------------------------------------#

# Listing 8.1 - Simple linear regression
fit <- lm(weight ~ height, data=women)
summary(fit)
women$weight
fitted(fit)
residuals(fit)

plot(women$height,women$weight, 
     xlab="Height (in inches)", 
     ylab="Weight (in pounds)")
abline(fit)


# Listing 8.2 - Polynomial regression
fit2 <- lm(weight ~ height + I(height^2), data=women)
summary(fit2)

plot(women$height,women$weight,
     xlab="Height (in inches)",
     ylab="Weight (in lbs)")
lines(women$height,fitted(fit2))



# Listing 8.3 - Examining bivariate relationships
states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                     "Illiteracy", "Income", "Frost")])
cor(states)
library(car)
scatterplotMatrix(states, smooth=FALSE,
                  main="Scatter Plot Matrix")


# Listing 8.4 - Multiple linear regression
states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
summary(fit)


# Listing 8.5 - Mutiple linear regression with a significant interaction term
fit <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(fit)

library(effects)
plot(effect("hp:wt", fit,, list(wt=c(2.2, 3.2, 4.2))), lines=c(1,2,3), multiline=TRUE)


# simple regression diagnostics
fit <- lm(weight ~ height, data=women)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))


# Assessing normality
library(car)
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
qqPlot(fit, simulate=TRUE, labels=row.names(states),
       id=list(method="identify"), main="Q-Q Plot")


# Independence of errors
durbinWatsonTest(fit)

# Assessing linearity
library(car)
crPlots(fit)


# Listing 8.6 - Assessing homoscedasticity
library(car)
ncvTest(fit)
spreadLevelPlot(fit)



# Listing 8.7 - Evaluating multi-collinearity
library(car)
vif(fit) 
vif(fit) > 10 # problem?

# Assessing outliers
library(car)
outlierTest(fit)

#  Identifying high leverage points
# redo as ggplot2?
hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)

# Identifying influential observations

# Cooks Distance D
# identify D values > 4/(n-k-1) 
cutoff <- 4/(nrow(states)-length(fit$coefficients)-2)
plot(fit, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")


# Added variable plots
# add id.method="identify" to interactively identify points
library(car)
avPlots(fit, ask=FALSE, id=list(method="identify"))


# Influence Plot
library(car)
influencePlot(fit, id="noteworthy", main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")


# Listing 8.10 - Box-Cox Transformation to normality
library(car)
summary(powerTransform(states$Murder))

# Box-Tidwell Transformations to linearity
library(car)
boxTidwell(Murder~Population+Illiteracy,data=states)


# Listing 8.11 - Comparing nested models using the anova function
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
           data=states)
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
anova(fit2, fit1)


# Listing 8.12 - Comparing models with the AIC
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
           data=states)
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
AIC(fit1,fit2)


# Listing 8.13 - Backward stepwise selection
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,
          data=states)
step(fit, direction="backward")


# Listing 8.14 - All subsets regression
library(leaps)
states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                     "Illiteracy", "Income", "Frost")])

leaps <-regsubsets(Murder ~ Population + Illiteracy + Income +
                     Frost, data=states, nbest=4)

subsTable <- function(obj, scale){
  x <- summary(leaps)
  m <- cbind(round(x[[scale]],3), x$which[,-1])
  colnames(m)[1] <- scale
  m[order(m[,1]), ]
}

subsTable(leaps, scale="adjr2")



# Listing 8.15 - Function for k-fold cross-validated R-square

shrinkage <- function(fit, k=10, seed=1){
  require(bootstrap)
  
  theta.fit <- function(x,y){lsfit(x,y)}                     
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}     
  
  x <- fit$model[,2:ncol(fit$model)]                         
  y <- fit$model[,1] 
  
  set.seed(seed)
  results <- crossval(x, y, theta.fit, theta.predict, ngroup=k)  
  r2    <- cor(y, fit$fitted.values)^2                         
  r2cv  <- cor(y, results$cv.fit)^2
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
}

states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Income + Illiteracy + Frost, data=states)
shrinkage(fit)

fit2 <- lm(Murder ~ Population + Illiteracy,data=states)
shrinkage(fit2)


# Listing 8.16 rlweights function for calculating relative importance of predictors
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-Square", pch=19,
           main="Relative Importance of Predictor Variables",
           sub=paste("Total R-Square=", round(rsquare, digits=3)),
           ...)
  return(import)
}

# Listing 8.17 - Applying the relweights function
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
relweights(fit, col="blue")
