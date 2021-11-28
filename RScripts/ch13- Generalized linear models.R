#---------------------------------------------------------------#
# R in Action (3rd ed): Chapter 13                              #
# Generalized linear models                                     #
# requires packages AER, robustbase, qcc, patchwork             #
# install.packages(c("AER", "robustbase", "qcc", "patchwork"))  #
#---------------------------------------------------------------#


## Logistic Regression

# get summary statistics
data(Affairs, package="AER")
summary(Affairs)
table(Affairs$affairs)

# create binary outcome variable
Affairs$ynaffair <- ifelse(Affairs$affairs > 0, 1, 0)
Affairs$ynaffair <- factor(Affairs$ynaffair, 
                           levels=c(0,1),
                           labels=c("No","Yes"))
table(Affairs$ynaffair)

# fit full model
fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children + 
                  religiousness + education + occupation +rating,
                data=Affairs,family=binomial())
summary(fit.full)

# fit reduced model
fit.reduced <- glm(ynaffair ~ age + yearsmarried + religiousness + 
                     rating, data=Affairs, family=binomial())
summary(fit.reduced)

# compare models
anova(fit.reduced, fit.full, test="Chisq")

# interpret coefficients
coef(fit.reduced)
exp(coef(fit.reduced))

# calculate probability of extramariatal affair by marital ratings
testdata <- data.frame(rating = c(1, 2, 3, 4, 5),
                       age = mean(Affairs$age),
                       yearsmarried = mean(Affairs$yearsmarried),
                       religiousness = mean(Affairs$religiousness))
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata

# calculate probabilites of extramariatal affair by age
testdata <- data.frame(rating = mean(Affairs$rating),
                       age = seq(17, 57, 10), 
                       yearsmarried = mean(Affairs$yearsmarried),
                       religiousness = mean(Affairs$religiousness))
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata

# evaluate overdispersion
fit <- glm(ynaffair ~ age + yearsmarried + religiousness +
             rating, family = binomial(), data = Affairs)
fit.od <- glm(ynaffair ~ age + yearsmarried + religiousness +
                rating, family = quasibinomial(), data = Affairs)
pchisq(summary(fit.od)$dispersion * fit$df.residual,  
       fit$df.residual, lower = F)


## Poisson Regression

# look at dataset
data(epilepsy, package="robustbase")
names(epilepsy)
summary(epilepsy[6:9])

# plot distribution of post-treatment seizure counts

library(ggplot2)
library(patchwork)
p1 <- ggplot(epilepsy, aes(x=Ysum)) +
  geom_histogram(color="black", fill="white") + 
  labs(title="Distribution of seizures", x="Seizure Count",
       y="Frequency") + theme_bw()
p2 <- ggplot(epilepsy, aes(x=Trt, y=Ysum)) +
  geom_boxplot() + 
  labs(title="Group comparisons", x="", y="") + theme_bw()

p1 + p2 

# fit regression
fit <- glm(Ysum ~ Base + Age + Trt, data=epilepsy, family=poisson())
summary(fit)

# interpret model parameters
coef(fit)
exp(coef(fit))

# evaluate overdispersion
deviance(fit)/df.residual(fit)
library(qcc)
qcc.overdispersion.test(epilepsy$Ysum, type="poisson")

# fit model with quasipoisson
fit.od <- glm(Ysum ~ Base + Age + Trt, data=epilepsy,
              family=quasipoisson())
summary(fit.od)
