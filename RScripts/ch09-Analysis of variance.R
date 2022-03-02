#-------------------------------------------------------------------------#
# R in Action (3rd ed): Chapter 9                                         #
# Analysis of variance                                                    #
# requires the multcomp, car, effects, rrcov, and mvoutlier, dplyr, and   #
#    ggplot2 packages                                                     # 
# install.packages(c("multcomp", "car", "effects", "rrcov", "mvoutlier",  #
#    "dplyr", "ggplot2"))                                                 # 
#-------------------------------------------------------------------------#

# Listing 9.1 - One-way ANOVA
data(cholesterol, package="multcomp")


library(dplyr)
plotdata <- cholesterol %>%
  group_by(trt) %>%
  summarize(n = n(),
            mean = mean(response),
            sd = sd(response),
            ci = qt(0.975, df = n - 1) * sd / sqrt(n))
plotdata

fit <- aov(response ~ trt, data=cholesterol)                                  
summary(fit)


library(ggplot2)
ggplot(plotdata, 
       aes(x = trt, y = mean, group = 1)) +
  geom_line(linetype="dashed", color="darkgrey") + 
  geom_errorbar(aes(ymin = mean - ci, 
                    ymax = mean + ci), 
                width = .2) +
  geom_point(size = 3, color="red") +
  theme_bw() + 
  labs(x="Treatment",
       y="Response",
       title="Mean Plot with 95% Confidence Interval") 

# Listing 9.2 - Tukey HSD pairwise group comparisons
pairwise <- TukeyHSD(fit)
pairwise

plotdata <- as.data.frame(pairwise[[1]])
plotdata$conditions <- row.names(plotdata)

library(ggplot2)
ggplot(data=plotdata, aes(x=conditions, y=diff)) + 
  geom_errorbar(aes(ymin=lwr, ymax=upr, width=.2)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_point(size=3, color="red") +
  theme_bw() +
  labs(y="Difference in mean levels", x="", 
       title="95% family-wise confidence level") +
   coord_flip()



# Multiple comparisons the multcomp package
library(multcomp)
tuk <- glht(fit, linfct=mcp(trt="Tukey")) 
summary(tuk)

labels1 <- cld(tuk, level=.05)$mcletters$Letters
labels2 <- paste(names(labels1), "\n", labels1)
ggplot(data=fit$model, aes(x=trt, y=response)) +
  scale_x_discrete(breaks=names(labels1), labels=labels2) +
  geom_boxplot(fill="lightgrey") +
  theme_bw() +
  labs(x="Treatment",
       title="Distribution of Response Scores by Treatment",
       subtitle="Groups without overlapping letters differ signifcantly (p < .05)")


# Assessing normality
library(car)
qqPlot(fit, simulate=TRUE, main="Q-Q Plot")

# Assessing homogeneity of variances
bartlett.test(response ~ trt, data=cholesterol)

# Assessing outliers
library(car)
outlierTest(fit)


# Listing 9.3 - One-way ANCOVA
library(multcomp)
library(dplyr)
litter %>%
  group_by(dose) %>%
  summarise(n=n(), mean=mean(gesttime), sd=sd(gesttime))
fit <- aov(weight ~ gesttime + dose, data=litter)                             
summary(fit)

# Obtaining adjusted means
library(effects)
effect("dose", fit)


#  Listing 9.4 - Multiple comparisons using user supplied contrasts
library(multcomp)
contrast <- rbind("no drug vs. drug" = c(3, -1, -1, -1))
summary(glht(fit, linfct=mcp(dose=contrast)))


# Listing 9.5 - Testing for homegeneity of regression slopes
library(multcomp)
fit2 <- aov(weight ~ gesttime*dose, data=litter)
summary(fit2)


# Visualizing a one-way ANCOVA
pred <- predict(fit)

library(ggplot2)
ggplot(data = cbind(litter, pred),
       aes(gesttime, weight)) + geom_point() +
  facet_grid(. ~ dose) + geom_line(aes(y=pred)) +
  labs(title="ANCOVA for weight by gesttime and dose") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none")


# Listing 9.6 - Two way ANOVA
data(ToothGrowth)
library(dplyr)
ToothGrowth$dose <- factor(ToothGrowth$dose)
stats <- ToothGrowth %>%
  group_by(supp, dose) %>%
  summarise(n=n(), mean=mean(len), sd=sd(len),
            ci = qt(0.975, df = n - 1) * sd / sqrt(n))
stats


fit <- aov(len ~ supp*dose, data=ToothGrowth)
summary(fit)

# plotting interactions
library(ggplot2)
pd <- position_dodge(0.2)
ggplot(stats, 
       aes(x = dose, y = mean, 
           group=supp, 
           color=supp, 
           linetype=supp)) +
  geom_point(size = 2, 
             position=pd) +
  geom_line(position=pd) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), 
                width = .1, 
                position=pd) +
  theme_bw() + 
  scale_color_manual(values=c("blue", "red")) +
  labs(x="Dose",
       y="Mean Length",
       title="Mean Plot with 95% Confidence Interval") 



#  Listing 9.7 - Repeated measures ANOVA with one between and within groups factor
data(CO2)
CO2$conc <- factor(CO2$conc)
w1b1 <- subset(CO2, Treatment=='chilled')
fit <- aov(uptake ~ (conc*Type) + Error(Plant/(conc)), w1b1)
summary(fit)

library(dplyr)
plotdata <- CO2 %>%
  group_by(conc, Type) %>%
  summarise(mean_conc = mean(uptake))

library(ggplot2)
ggplot(data=plotdata, aes(x=conc, y=mean_conc, group=Type, color=Type,
                          linetype=Type)) +
  geom_point(size=2) +
  geom_line(size=1) +
  theme_bw() + theme(legend.position="top") +
  labs(x="Concentration", y="Mean Uptake", 
       title="Interaction Plot for Plant Type and Concentration")

library(ggplot2)
ggplot(data=CO2, aes(x=conc, y=uptake, fill=Type)) +
  geom_boxplot() +
  theme_bw() + theme(legend.position="top") +
  scale_fill_manual(values=c("aliceblue", "deepskyblue"))+
  labs(x="Concentration", y="Uptake", 
       title="Chilled Quebec and Mississippi Plants")


# Listing 9.8 - One-way MANOVA
data(UScereal, package="MASS")
shelf <- factor(UScereal$shelf) 
y <- cbind(UScereal$calories, UScereal$fat, UScereal$sugars)
colnames(y) <- c("calories", "fat", "sugars")
aggregate(y, by=list(shelf=shelf), FUN=mean)
round(cov(y), 2)
fit <- manova(y ~ shelf)
summary(fit)
summary.aov(fit)


#  Listing 9.9 - Assessing multivariate normality
center <- colMeans(y)
n <- nrow(y)
p <- ncol(y)
cov <- cov(y)
d <- mahalanobis(y,center,cov)
coord <- qqplot(qchisq(ppoints(n),df=p),
                d, main="QQ Plot Assessing Multivariate Normality",
                ylab="Mahalanobis D2")
abline(a=0,b=1)
identify(coord$x, coord$y, labels=row.names(UScereal))


# multivariate outliers
library(mvoutlier)
outliers <- aq.plot(y)
outliers

# Listing 9.10 - Robust one-way MANOVA
library(rrcov)
Wilks.test(y,shelf, method="mcd")  # this can take a while


# Listing 9.11 - A regression approach to the Anova problem
fit.lm <- lm(response ~ trt, data=cholesterol)
summary(fit.lm)
contrasts(cholesterol$trt)











