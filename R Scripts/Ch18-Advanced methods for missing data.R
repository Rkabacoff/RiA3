#-----------------------------------#
# R in Action (2nd ed): Chapter 18  #
# Advanced methods for missing data #
# requires packages VIM, mice       #
# install.packages(c("VIM", mice))  #
#-----------------------------------#

# load the dataset
data(sleep, package="VIM")


# list the rows that do not have missing values
sleep[complete.cases(sleep),]


# list the rows that have one or more missing values
sleep[!complete.cases(sleep),]


# tabulate missing values patters
library(mice)
md.pattern(sleep, rotate.names=TRUE)

library(visdat)
vis_dat(sleep)
library(ggplot2)
vis_miss(sleep) + 
  theme(axis.text.x=element_text(size=8, angle=90))
vis_miss(sleep, 
         cluster=TRUE, 
         sort_miss=FALSE,
         show_perc_col=TRUE) +
  coord_flip() +
  theme(axis.text.y=element_text(size=8),
        axis.text.x=element_text(size=8, angle=0)) 

library(naniar)
gg_miss_upset(sleep)
gg_miss_var(sleep)
gg_miss_case(sleep)

# plot missing values patterns
library("VIM")
aggr(sleep)
aggr(sleep, prop=FALSE, numbers=TRUE, cex.axis=.8,
     cex.numbers=.7, cex.lab=.9, gap=2)
matrixplot(sleep)
marginplot(sleep[c("Gest","Dream")], pch=20, 
           col=c("darkgray", "red", "blue"))
aggr(sleep, prop=FALSE, numbers=TRUE)
aggr(sleep, prop=FALSE, numbers=TRUE, cex.axis=1,
     cex.numbers=1, cex.lab=.9, gap=1)
# use correlations to explore missing values
x <- as.data.frame(abs(is.na(sleep)))
head(sleep, n=5)
head(x, n=5)
y <- x[which(apply(x,2,sum)>0)]
cor(y)
cor(sleep, y, use="pairwise.complete.obs")


# complete case analysis (listwise deletion)
options(digits=1)
cor(na.omit(sleep))
fit <- lm(Dream ~ Span + Gest, data=na.omit(sleep))
summary(fit)


# multiple imputation
options(digits=3)
library(mice)
data(sleep, package="VIM")
imp <- mice(sleep, seed=1234)
fit <- with(imp, lm(Dream ~ Span + Gest))
pooled <- pool(fit)
summary(pooled)
imp

####
library(mi)
mdf <- missing_data.frame(sleep)
image(mdf)
image(mdf, grayscale=FALSE)

library(DescTools)
PlotMiss(sleep, clust=TRUE, main="Missing Values")
PlotMiss(sleep)
library(naniar)
vis_miss(sleep, cluster=TRUE, sort_miss=TRUE)
gg_miss_upset(sleep)

