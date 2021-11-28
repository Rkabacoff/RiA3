#----------------------------------------------#
# R in Action (3rd ed): Chapter 18             #
# Advanced methods for missing data            #
# requires packages VIM, mice, ggplot2,        # 
#    naniar, visdat, DescTools, mi             #
# install.packages(c("VIM", "mice", "ggplot2", #
#    "naniar", "visdat", "DescTools", "mi"))   #
#----------------------------------------------#

# load the dataset
data(sleep, package="VIM")


# list the rows that do not have missing values
sleep[complete.cases(sleep),]


# list the rows that have one or more missing values
sleep[!complete.cases(sleep),]

# number of Dream missing values
sum(is.na(sleep$Dream))

# percent of missing values on Dream
mean(is.na(sleep$Dream))

# percent of cases with missing data
mean(!complete.cases(sleep))


# Listing 18.1 Missing values patterns with md.pattern()
library(mice)
md.pattern(sleep, rotate.names=TRUE)

# plot missing values patterns using VIM
library("VIM")
aggr(sleep, prop=FALSE, numbers=TRUE)
matrixplot(sleep, sort="BodyWgt")
marginplot(sleep[c("Gest","Dream")], pch=20, 
           col=c("darkgray", "red", "blue"))


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

# available case analysis (pairwise deletion)
cor(sleep, use="pairwise.complete.obs")

# Listing 18.2 k-Nearest Neighbor Imputation for the sleep data frame
options(digits=7)
library(VIM)
head(sleep)
sleep_imp <- kNN(sleep, imp_var=FALSE)
head(sleep_imp)

# Listing 18.3 Random forest imputatoin for the sleep data frame
library(missForest)
set.seed(1234)
sleep_imp <- missForest(sleep)$ximp
head(sleep_imp)

# multiple imputation with the mice package
options(digits=8)
library(mice)
data(sleep, package="VIM")
imp <- mice(sleep, seed=1234)
fit <- with(imp, lm(Dream ~ Span + Gest))
pooled <- pool(fit)
summary(pooled)
imp$imp$Dream
dataset3 <- complete(imp, action=3)
dataset3
