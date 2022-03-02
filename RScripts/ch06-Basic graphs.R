#-----------------------------------------------------------------------#
# R in Action (3rd ed): Chapter 6                                       #
# Basic graphs                                                          #
# requires the ggplot2, vcd, dplyr, treemapfiy and scales packages      #
# install.packages(c("ggplot2", "vcd", "dplyr", "treemapify", "scales"))# 
#-----------------------------------------------------------------------#

# Listing 6.1 Simple bar charts
library(ggplot2)
data(Arthritis, package="vcd")
ggplot(Arthritis, aes(x=Improved, fill=Improved)) + geom_bar() +
  labs(title="Simple Bar chart",                 
       x="Improvement",                          
       y="Frequency")                            

ggplot(Arthritis, aes(x=Improved)) + geom_bar() +
  labs(title="Horizontal Bar chart",             
       x="Improvement",                          
       y="Frequency") +                          
  coord_flip()                                   

# Listing 6.2 Stacked, grouped, and filled bar charts
ggplot(Arthritis, aes(x=Treatment, fill=Improved)) +
  geom_bar(position = "stack") +  
  labs(title="Stacked Bar chart",                   
       x="Treatment",                               
       y="Frequency")                               

ggplot(Arthritis, aes(x=Treatment, fill=Improved)) +
  geom_bar(position = "dodge") + 
  labs(title="Grouped Bar chart",                   
       x="Treatment",                               
       y="Frequency")                               

ggplot(Arthritis, aes(x=Treatment, fill=Improved)) +
  geom_bar(position = "fill") +  
  labs(title="Filled Bar chart",                   
       x="Treatment",                               
       y="Proportion") 

# Listing 6.3 Bar chart for sorted mean values
states <- data.frame(state.region, state.x77)    

library(dplyr)                 
plotdata <- states %>% 
group_by(state.region) %>%
summarize(mean = mean(Illiteracy))
plotdata

ggplot(plotdata, aes(x=reorder(state.region, mean), y=mean)) + 
  geom_bar(stat="identity") +
  labs(x="Region",
  y="",
  title = "Mean Illiteracy Rate")

# Listing 6.4 Bar chart of mean values with error bars
plotdata <- states %>%
group_by(state.region) %>%
summarize(n=n(), 
          mean = mean(Illiteracy),                      
          se = sd(Illiteracy)/sqrt(n))                  

plotdata

ggplot(plotdata, aes(x=reorder(state.region, mean), y=mean)) +
geom_bar(stat="identity", fill="skyblue") +
geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2) +
labs(x="Region",
     y="",
     title = "Mean Illiteracy Rate",
     subtitle = "with standard error bars")

# Figure 6.5 Bar chart
data(Arthritis, package="vcd")
ggplot(Arthritis, aes(x=Improved)) + 
  geom_bar(fill="gold", color="black") +
  labs(title="Treatment Outcome") 

# Bar chart labels
ggplot(mpg, aes(x=model)) + 
  geom_bar() +
  labs(title="Car models in the mpg dataset", 
       y="Frequency", x="") 

ggplot(mpg, aes(x=model)) + 
  geom_bar() +
  labs(title="Car models in the mpg dataset", 
       y="Frequency", x="") +
  coord_flip() 

ggplot(mpg, aes(x=model)) + 
  geom_bar() +
  labs(title="Model names in the mpg dataset", 
       y="Frequency", x="") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8)) 


# Pie charts
if(!require(remotes)) install.packages("remotes")
   devtools::install_github("rkabacoff/ggpie")

library(ggplot2)
library(ggpie)
ggpie(mpg, class)

ggpie(mpg, class, legend=FALSE, offset=1.3, 
      title="Automobiles by Car Class")

ggpie(mpg, class, year, 
      legend=FALSE, offset=1.3, title="Car Class by Year")

# Listing 6.6 Simple Tree Map
library(ggplot2)
library(dplyr)
library(treemapify)

plotdata <- mpg %>% count(manufacturer)

ggplot(plotdata,
       aes(fill = manufacturer,
           area = n,
           label = manufacturer)) +
  geom_treemap() +
  geom_treemap_text() +
  theme(legend.position = "none")

# Listing 6.7 Tree Map with Subgrouping
plotdata <- mpg %>%  
  count(manufacturer, drv)
plotdata$drv <- factor(plotdata$drv, 
                       levels=c("4", "f", "r"),
                       labels=c("4-wheel", "front-wheel", "rear"))

ggplot(plotdata,
       aes(fill = manufacturer, 
           area = n,
           label = manufacturer,
           subgroup=drv)) +
  geom_treemap() + 
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(
    place = "middle",
    colour = "black",
    alpha = 0.5,
    grow = FALSE) +
  geom_treemap_text(colour = "white", 
                    place = "centre",
                    grow=FALSE) +
  theme(legend.position = "none")

# Listing 6.8 Histograms
library(ggplot2)
library(scales)

data(mpg)
cars2008 <- mpg[mpg$year == 2008, ]

ggplot(cars2008, aes(x=hwy)) + 
  geom_histogram() +
  labs(title="Default histogram")

ggplot(cars2008, aes(x=hwy)) + 
  geom_histogram(bins=20, color="white", fill="steelblue") +
  labs(title="Colored histogram with 20 bins",
       x="Highway Miles Per Gallon",
       y="Frequency")

ggplot(cars2008, aes(x=hwy, y=..density..)) + 
  geom_histogram(bins=20, color="white", fill="steelblue") +
  scale_y_continuous(labels=scales::percent) +
  labs(title="Histogram with percentages",
       y= "Percent",
       x="Highway Miles Per Gallon")

ggplot(cars2008, aes(x=hwy, y=..density..)) +
  geom_histogram(bins=20, color="white", fill="steelblue") + 
  scale_y_continuous(labels=scales::percent) +
  geom_density(color="red", size=1) +
  labs(title="Histogram with density curve",
       y="Percent" ,
       x="Highway Miles Per Gallon")

# Listing 6.9 Kernel density plots
data(mpg, package="ggplot2")
cars2008 <- mpg[mpg$year == 2008, ]

ggplot(cars2008, aes(x=cty)) + 
  geom_density() + 
  labs(title="Default kernel density plot") 

ggplot(cars2008, aes(x=cty)) + 
  geom_density(fill="red") + 
  labs(title="Filled kernel density plot")

bw.nrd0(cars2008$cty)

ggplot(cars2008, aes(x=cty)) + 
  geom_density(fill="red", bw=.5) +
  labs(title="Kernel density plot with bw=0.5")

# Listing 6.10 Comparative kernel density plots
data(mpg, package="ggplot2")
cars2008 <- mpg[mpg$year == 2008 & mpg$cyl != 5,]
cars2008$Cylinders <- factor(cars2008$cyl)

ggplot(cars2008, aes(x=cty, color=Cylinders, linetype=Cylinders)) +
  geom_density(size=1)  +
  labs(title="Fuel Efficiecy by Number of Cylinders",
       x = "City Miles per Gallon")

ggplot(cars2008, aes(x=cty, fill=Cylinders)) + 
  geom_density(alpha=.4) +
  labs(title="Fuel Efficiecy by Number of Cylinders",
       x = "City Miles per Gallon")

# Box plots
ggplot(mtcars, aes(x="", y=mpg)) +
  geom_boxplot() +
  labs(y = "Miles Per Gallon", x="", title="Box Plot")

cars <- mpg[mpg$cyl != 5, ]
cars$Cylinders <- factor(cars$cyl)
cars$Year <- factor(cars$year)
ggplot(cars, aes(x=Cylinders, y=cty)) + 
  geom_boxplot() +
  labs(x="Number of Cylinders", 
       y="Miles Per Gallon", 
       title="Car Mileage Data")

ggplot(cars, aes(x=Cylinders, y=cty)) + 
  geom_boxplot(notch=TRUE, 
               fill="steelblue",
               varwidth=TRUE) +
  labs(x="Number of Cylinders", 
       y="Miles Per Gallon", 
       title="Car Mileage Data")

ggplot(cars, aes(x=Cylinders, y=cty, fill=Year)) +           
  geom_boxplot() +                                           
  labs(x="Number of Cylinders",                              
       y="Miles Per Gallon",                                 
       title="City Mileage by # Cylinders and Year") +    
  scale_fill_manual(values=c("gold", "green"))      

# Listing 6.11 Violin plots
cars <- mpg[mpg$cyl != 5, ]
cars$Cylinders <- factor(cars$cyl)

ggplot(cars, aes(x=Cylinders, y=cty)) + 
  geom_boxplot(width=0.2, 
               fill="green") +
  geom_violin(fill="gold", 
              alpha=0.3) +
  labs(x="Number of Cylinders", 
       y="City Miles Per Gallon", 
       title="Violin Plots of Miles Per Gallon")

# Dot plots
plotdata <- mpg %>%
  filter(year == "2008") %>%
  group_by(model) %>%
  summarize(meanHwy=mean(hwy))
plotdata

ggplot(plotdata, aes(x=meanHwy, y=model)) + 
  geom_point() +
  labs(x="Miles Per Gallon", 
       y="", 
       title="Gas Mileage for Car Models")

ggplot(plotdata, aes(x=meanHwy, y=reorder(model, meanHwy))) + 
  geom_point() +
  labs(x="Miles Per Gallon", 
       y="", 
       title="Gas Mileage for Car Models",
       subtitle = "with standard error bars")
