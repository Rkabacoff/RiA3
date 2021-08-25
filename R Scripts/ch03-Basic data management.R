#------------------------------------------------------------#
# R in Action (3rd ed): Chapter 3                            #
# Basic data management                                      #
# requires packages the dplyr and sqldf packages             #
# install.packages(c("dplyr", "sqldf"))                      #
#------------------------------------------------------------#

# Listing 3.1 Creating the leadership data frame
leadership <- data.frame(
  manager = c(1, 2, 3, 4, 5),
  date    = c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09"),
  country = c("US", "US", "UK", "UK", "UK"),
  gender  = c("M", "F", "F", "M", "F"),
  age     = c(32, 45, 25, 39, 99),
  q1      = c(5, 3, 3, 3, 2),
  q2      = c(4, 5, 5, 3, 2),
  q3      = c(5, 2, 5, 4, 1),
  q4      = c(5, 5, 5, NA, 2),
  q5      = c(5, 5, 2, NA, 1)
)


# Listing 3.2 Creating new variables
leadership$total_score  <-  leadership$q1 + leadership$q2 + 
                            leadership$q3 + leadership$q4 + leadership$q5

leadership$mean_score <- (leadership$q1 + leadership$q2 + leadership$q3 + 
                            leadership$q4 + leadership$q5)/5

leadership <- transform(leadership,
                        total_score  =  q1 + q2 + q3 + q4 + q5,
                        mean_score = (q1 + q2 + q3 + q4 + q5)/5)

# Listing 3.3 Apply the is.na() function
is.na(leadership[,6:10])

# Listing 3.4 Using na.omit() to delete incomplete observations
leadership
newdata <- na.omit(leadership)    
newdata

# Listing 3.5 Converting from one data type to another
a <- c(1,2,3)
a
is.numeric(a)
is.vector(a)
a <- as.character(a)
a
is.numeric(a)
is.vector(a)
is.character(a)

# Listing 3.6 Selecting observations
newdata <- leadership[1:3,]

newdata <- leadership[leadership$gender=="M" &
                        leadership$age > 30,]

# Listing 3.7 Manipulating data with dplyr
leadership <- data.frame(
  manager = c(1, 2, 3, 4, 5),
  date    = c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09"),
  country = c("US", "US", "UK", "UK", "UK"),
  gender  = c("M", "F", "F", "M", "F"),
  age     = c(32, 45, 25, 39, 99),
  q1      = c(5, 3, 3, 3, 2),
  q2      = c(4, 5, 5, 3, 2),
  q3      = c(5, 2, 5, 4, 1),
  q4      = c(5, 5, 5, NA, 2),
  q5      = c(5, 5, 2, NA, 1)
)

library(dplyr)                                                  
leadership <- mutate(leadership,                                
                     total_score = q1 + q2 + q3 + q4 + q5,      
                     mean_score = total_score / 5)              
leadership$gender <- recode(leadership$gender,                  
                            "M" = "male", "F" = "female")       
leadership <- rename(leadership, ID = "manager", sex = "gender")
leadership <- arrange(leadership, sex, total_score)             
leadership_ratings <- select(leadership, ID, mean_score)        
leadership_men_high <- filter(leadership,                       
                              sex == "male" & total_score > 10)

# Listing 3.8 Using SQL statements to manipulate data frames
library(sqldf)
newdf <- sqldf("select * from mtcars where carb=1 order by mpg",
               row.names=TRUE)
newdf
sqldf("select avg(mpg) as avg_mpg, avg(disp) as avg_disp, gear 
              from mtcars where cyl in (4, 6) group by gear")


