#------------------------------------------------------------#
# R in Action (3rd ed): Chapter 1                            #
# Introduction to R                                          #
#------------------------------------------------------------#

## Chapter 01
## Introduction to R

# Listing 1.1 A sample R session
age <- c(1, 3, 5, 2, 11, 9, 3, 9, 12, 3)
weight <- c(4.4, 5.3, 7.2, 5.2, 8.5, 7.3, 6.0, 10.4, 10.2, 6.1)
mean(weight)
sd(weight)
cor(age, weight)
plot(age, weight)

# Listing 1.2 An example of commands used to manage
# the R workspace
setwd("C:/myprojects/project1")
options()
options(digits = 3)

# Listing 1.3 Working with a new package
help.start()
install.packages("vcd")
help(package = "vcd")
library(vcd)
help(Arthritis)
Arthritis
example(Arthritis)
