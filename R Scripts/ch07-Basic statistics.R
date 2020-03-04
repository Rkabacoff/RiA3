#------------------------------------------------------------#
# R in Action (3rd ed): Chapter 7                            #
# Basic statistics                                           #
# requires the Hmisc, pastecs, psych, dplyr, carData, vcd    #
#    gmodels, ggm, and MASS packages                         #
# install.packages(c("Hmisc", "pastecs", "psych", "dplyr",   #   
#                    "carData", "vcd", "gmodels", "ggm",     #   
#                    "MASS"))                                #
#------------------------------------------------------------#

# Listing 7.1 Descriptive statistics with summary()
myvars <- c("mpg", "hp", "wt")
summary(mtcars[myvars])

# Listing 7.2 Descriptive statistics via sapply()
mystats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, stdev=s, 
           skew=skew, kurtosis=kurt))
}

myvars <- c("mpg", "hp", "wt")
sapply(mtcars[myvars], mystats)

# Listing 7.3 Descriptive statistics via describe() in the Hmisc package
library(Hmisc)
myvars <- c("mpg", "hp", "wt")
describe(mtcars[myvars])

# Listing 7.4 Descriptive statistics via stat.desc() in the pastecs package 
library(pastecs)
myvars <- c("mpg", "hp", "wt")
stat.desc(mtcars[myvars])

# Listing 7.5 Descriptive statistics via describe() in the psych package
library(psych)
myvars <- c("mpg", "hp", "wt")
describe(mtcars[myvars])

# Listing 7.6 Descriptive statistics by group using by()
dstats <- function(x)sapply(x, mystats)
myvars <- c("mpg", "hp", "wt")
by(mtcars[myvars], mtcars$am, dstats)

# Listing 7.7 Descriptive statistics for groups defined by multiple variables
dstats <- function(x)sapply(x, mystats, na.omit=TRUE)
myvars <- c("mpg", "hp", "wt")
by(mtcars[myvars], 
   list(Transmission=mtcars$am,
        Engine=mtcars$vs), 
   FUN=dstats)

# Section 7.1.4
# Summarizing data interactively with dplyr

library(dplyr)
library(carData)
Salaries %>%
  summarize(med = median(salary), 
            min = min(salary), 
            max = max(salary))

Salaries %>%
  group_by(rank, sex) %>%
  summarize(n = length(salary),
            med = median(salary), 
            min = min(salary), 
            max = max(salary))

Salaries %>%
  group_by(rank, sex) %>%
  select(yrs.service, yrs.since.phd) %>%
  summarize_all(mean)

# Section 7.2 
# Frequency tables
library(vcd)
head(Arthritis)

# one way table
mytable <- table(Arthritis$Improved)
mytable                       # counts
prop.table(mytable)           # proportions
prop.table(mytable)*100       # percents

# two way table
mytable <- xtabs(~ Treatment+Improved, data=Arthritis)
mytable  # counts

margin.table(mytable, 1)    # total counts for Treatment 
prop.table(mytable, 1)      # row proportions (rows add to 1)

margin.table(mytable, 2)    # total counts for Improved
prop.table(mytable, 2)      # column proportions (columns add to 1)

prop.table(mytable)         # cell proportions (all cells add to 1)
addmargins(mytable)         # cell counts with row and column sums
addmargins(prop.table(mytable)) # cell proportions with row and column proportions

addmargins(prop.table(mytable, 1), 2) # row proportions with row sums
addmargins(prop.table(mytable, 2), 1) # column proportions with column sums

# Listing 7.8 Two-way table using CrossTable
library(gmodels)
CrossTable(Arthritis$Treatment, Arthritis$Improved)

# Listing 7.9 Three-way contingency table
mytable <- xtabs(~ Treatment+Sex+Improved, data=Arthritis) 
mytable          
margin.table(mytable, 1)  # totals for Treatment
margin.table(mytable, 2)  # totals for Sex
margin.table(mytable, 3)  # totals for Improved
margin.table(mytable, c(1, 3)) # totals for Treatment by Improved

# Treatment by Sex for each Level of Improved
ftable(mytable)
ftable(prop.table(mytable, c(1, 2))) # proportions sum to 1 over index omitted
ftable(addmargins(prop.table(mytable, c(1, 2)), 3)) 
ftable(addmargins(prop.table(mytable, c(1, 2)), 3)) * 100

# Listing 7.10 Chi-square test of independence
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)          
chisq.test(mytable)

mytable <- xtabs(~Improved+Sex, data=Arthritis)              
chisq.test(mytable) 

# Fisher's exact test
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
fisher.test(mytable)

# Chochran-Mantel-Haenszel test
mytable <- xtabs(~Treatment+Improved+Sex, data=Arthritis)
mantelhaen.test(mytable)

# Listing 7.11 Measures of association for a two-way table
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
assocstats(mytable)

# Listing 7.12 Covariances and correlations
states<- state.x77[,1:6]
cov(states)
cor(states)
cor(states, method="spearman")

x <- states[,c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[,c("Life Exp", "Murder")]
cor(x,y)

# partial correlations
library(ggm)
colnames(states)
pcor(c(1,5,2,3,6), cov(states))

# Listing 7.13 Testing a correlation coefficient for significance
cor.test(states[,3], states[,5])

# Listing 7.14 Correlation matrix and tests of significance via corr.test()
library(psych)
corr.test(states, use="complete")

# t-tests
library(MASS)
t.test(Prob ~ So, data=UScrime)


sapply(UScrime[c("U1","U2")], function(x)(c(mean=mean(x),sd=sd(x))))
with(UScrime, t.test(U1, U2, paired=TRUE))

# Mann-Whitney U-test
with(UScrime, by(Prob, So, median))
wilcox.test(Prob ~ So, data=UScrime)
sapply(UScrime[c("U1","U2")], median)
with(UScrime, wilcox.test(U1, U2, paired=TRUE))

# Kruskal-Wallis test
states <- data.frame(state.region, state.x77)
kruskal.test(Illiteracy ~ state.region, data=states)

# Listing 7.15 Nonparametric multiple comparisons
source("http://www.statmethods.net/RiA/wmc.txt")              
states <- data.frame(state.region, state.x77)
wmc(Illiteracy ~ state.region, data=states, method="holm")



