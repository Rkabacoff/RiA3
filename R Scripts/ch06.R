## Chapter 06
## Basic graphs

# requires packages ggplot2, vcd, dplyr
install.packages("ggplot2")
install.packages("vcd")
install.packages("dplyr")

# Listing 6.1 Simple bar charts
library(ggplot2)
data(Arthritis, package="vcd")
ggplot(Arthritis, aes(x=Improved)) + geom_bar() +
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
  labs(title="Stacked Bar chart",                   
       x="Treatment",                               
       y="Frequency") 

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
  title = "Mean Illiteracy Rate"

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