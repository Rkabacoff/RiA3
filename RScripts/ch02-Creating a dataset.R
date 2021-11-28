#------------------------------------------------------------#
# R in Action (3rd ed): Chapter 2                            #
# Creating a dataset                                         #
#------------------------------------------------------------#

# Types of vectors
a <- c(1, 2, 5, 3, 6, -2, 4) # numeric
b <- c("one", "two", "three") # character
c <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE) # logical

# subsetting vectors
a <- c("k", "j", "h", "a", "c", "m")
a[3]
a[c(1, 3, 5)]
a[2:6]

# Listing 2.1 Creating matrices
y <- matrix(1:20, nrow = 5, ncol = 4)
y
cells <- c(1, 26, 24, 68)
rnames <- c("R1", "R2")
cnames <- c("C1", "C2")
mymatrix <- matrix(cells,
  nrow = 2, ncol = 2, byrow = TRUE,
  dimnames = list(rnames, cnames))
mymatrix
mymatrix <- matrix(cells,
  nrow = 2, ncol = 2, byrow = FALSE,
  dimnames = list(rnames, cnames))
mymatrix

# Listing 2.2 Using matrix subscripts
x <- matrix(1:10, nrow = 2)
x
x[2, ]
x[, 2]
x[1, 4]
x[1, c(4, 5)]

# Listing 2.3 Creating an array
dim1 <- c("A1", "A2")
dim2 <- c("B1", "B2", "B3")
dim3 <- c("C1", "C2", "C3", "C4")
z <- array(1:24, c(2, 3, 4), dimnames = list(dim1, dim2, dim3))
z

# Listing 2.4 Creating a data frame
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
patientdata <- data.frame(patientID, age, diabetes, status)
patientdata

# Listing 2.5 Specifying elements of a data frame
patientdata[1:2]
patientdata$age
patientdata[c("diabetes", "status")]
patientdata$age

# Listing 2.6 Using factors
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
diabetes <- factor(diabetes)
status <- factor(status, order = TRUE)
patientdata <- data.frame(patientID, age, diabetes, status)
str(patientdata)
summary(patientdata)

# Listing 2.7 Creating a list
g <- "My First List"
h <- c(25, 26, 18, 39)
j <- matrix(1:10, nrow = 5)
k <- c("one", "two", "three")
mylist <- list(title = g, ages = h, j, k)
mylist
mylist[[2]]
mylist[["ages"]]
