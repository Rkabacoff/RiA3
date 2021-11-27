#------------------------------------------------------------#
# R in Action (3rd ed): Chapter 11                           # 
# Intermediate Graphs                                        #
# requires the ggplot2, GGally, scatterplot3d, car, rgl,     #
#   corrgram, and vcd packages                               #
# install.packages(c("ggplot2", "GGally", "scatterplot3d",   #
#   "car", "rgl", "corrgram", "vcd"))                        #
#------------------------------------------------------------#

# Listing 11.1 A scatter plot with best-fit lines
library(ggplot2)
data(mtcars)
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="red") +
  geom_smooth(method="loess", se=FALSE, color="blue", linetype="dashed") +
  labs(title = "Basic Scatter Plot of MPG vs. Weight",
       x = "Car Weight (lbs/1000)",
       y = "Miles Per Gallon")

# Figure 11.2 Scatter plot with subgroups and separately
# estimated fit line
library(ggplot2)
ggplot(mtcars, 
       aes(x=wt, y=mpg, 
           color=factor(cyl), 
           shape=factor(cyl))) +
  geom_point(size=2) +
  geom_smooth(method="lm", se=FALSE) +
  geom_smooth(method="loess", se=FALSE, linetype="dashed") +
  labs(title = "Scatter Plot of MPG vs. Weight",
       subtitle = "By Number of Cylinders",
       x = "Car Weight (lbs/1000)",
       y = "Miles Per Gallon",
       color = "Number of \nCylinders",
       shape = "Number of \nCylinders") +
  theme_bw()

# Figure 11.3 Scatter-plot matrix with the ggpairs function
library(GGally)
ggpairs(mtcars[c("mpg","disp","hp", "wt")])
               
# Figure 11.4 Customized scatter-plot matrix with fit lines

library(GGally)
lowerplots <- function(data, mapping) {
    ggplot(data = data, mapping = mapping) +
      geom_point(color="darkgrey") +
      geom_smooth(method = "lm", color = "steelblue", se=FALSE) +
      geom_smooth(method="loess", color="red", se=FALSE, linetype="dashed")
}

diagplots <- function(data, mapping) {
  ggplot(data = data, mapping = mapping) +
    geom_histogram(fill="lightblue", color="black")
}

upperplots <- function(data, mapping) {
    ggally_cor(data=data, mapping=mapping, 
               display_grid=FALSE, size=3.5, color="black")
}

mytheme <-  theme(strip.background = element_blank(),
                  panel.grid       = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_rect(color="grey20", fill=NA))


ggpairs(mtcars, 
        columns=c("mpg","disp", "drat", "wt"), 
        columnLabels=c("MPG", "Displacement", 
                       "R Axle Ratio", "Weight"),
        title = "Scatterplot Matrix with Linear and Loess Fits",
        lower = list(continuous = lowerplots),
        diag =  list(continuous = diagplots),
        upper = list(continuous = upperplots)) +
        mytheme



# Figure 11.5 Scatterplot with 10,000 observations
set.seed(1234)
n <- 10000
c1 <- matrix(rnorm(n, mean=0, sd=.5), ncol=2)
c2 <- matrix(rnorm(n, mean=3, sd=2), ncol=2)
mydata <- rbind(c1, c2)
mydata <- as.data.frame(mydata)
names(mydata) <- c("x", "y")

ggplot(mydata, aes(x=x, y=y)) + geom_point() +
  ggtitle("Scatter Plot with 10,000 Observations")

# Figure 11.6 Scatter plot using smoothScatter
with(mydata,
     smoothScatter(x, y, main="Scatter Plot Colored by Smoothed Densities"))

# Figure 11.7 Hexagonal Binning with 100,000 Observations
ggplot(mydata, aes(x=x, y=y)) + geom_hex(bins=50) +
  scale_fill_continuous(trans = 'reverse') +
  ggtitle("Scatter Plot with 10,000 Observations")

## Figure 11.8 3-D Scatterplots
library(scatterplot3d)
with(mtcars,
     scatterplot3d(wt, disp, mpg,
                   main="Basic 3D Scatter Plot"))

# Figure 11.9 3D scatter plot with vertical lines and shading
with(mtcars,
     scatterplot3d(wt, disp, mpg,
                   pch=16,
                   highlight.3d=TRUE,
                   type="h",
                   main="3D Scatter Plot with Vertical Lines"))

# Figure 11.10 3D scatter plot with vertical lines, shading,
# and overlaid regression plane
s3d <-with(mtcars,
           scatterplot3d(wt, disp, mpg,
                        pch=16,
                        highlight.3d=TRUE,
                        type="h",
                        main="3D Scatter Plot with Vertical Lines and Regression Plane"))
fit <- lm(mpg ~ wt+disp, data=mtcars)
s3d$plane3d(fit)

# Figure 11.11 Rotating 3D scatter plot produced by the 
# plot3d() function in the rgl package
library(rgl)
with(mtcars,
     plot3d(wt, disp, mpg, col="red", size=5))

# Figure 11.12 Spinning 3D scatter plot produced by the 
# scatter3d() function in the car package
library(car)
with(mtcars,
     scatter3d(wt, disp, mpg))

# Figure 11.13 Bubble plotBubble plot of car weight vs. mpg, 
# where point size is proportional to engine displacement
ggplot(mtcars, 
       aes(x = wt, y = mpg, size = disp)) +
       geom_point() +
       labs(title="Bubble Plot with point size proportional to displacement",
            x="Weight of Car (lbs/1000)",
            y="Miles Per Gallon")

# Figure 11.4. Enhanced bubble plot. Automobiles with more engine 
# cylinders tend to have increased weight and engine displacement, 
# and poorer fuel efficiency
ggplot(mtcars, 
       aes(x = wt, y = mpg, size = disp, fill=factor(cyl))) +
  geom_point(alpha = .5, 
             color = "black", 
             shape = 21) +
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Auto mileage by weight and horsepower",
       subtitle = "Motor Trend US Magazine (1973-74 models)",
       x = "Weight (1000 lbs)",
       y = "Miles/(US) gallon",
       size = "Engine\ndisplacement",
       fill = "Cylinders") +
  theme_minimal() 

# Figure 11.5 Comparison of a scatter plot and a line plot
library(ggplot2)
tree1 <- subset(Orange, Tree==1)
ggplot(data=tree1, 
       aes(x=age, y=circumference)) +
  geom_point(size=2) +
  labs(title="Orange Tree 1 Growth",
       x = "Age (days)",
       y = "Circumference (mm)") +
  theme_bw()

ggplot(data=tree1, 
                   aes(x=age, y=circumference)) +
   geom_line() +
  geom_point(size=2) +
  labs(title="Orange Tree 1 Growth",
       x = "Age (days)",
       y = "Circumference (mm)") +
  theme_bw()

# Figure 11.16 ggplot2 line types

lt <- c("blank", "solid", "dashed", "dotted", 
        "dotdash", "longdash", "twodash")
ltn <-paste(0:6, lt, sep="-")
d <- data.frame(lt = factor(lt, levels = lt))
ggplot() + 
  scale_x_continuous(name = "", 
                     limits = c(0, 1), breaks = NULL) + 
  scale_linetype_identity() + 
  geom_segment(data = d, 
               mapping = aes(x = 0, xend = 1, y = lt, yend = lt, linetype = lt)) + 
  scale_y_discrete(name="", labels=ltn)+
  labs(title = "ggplot2 linetypes") +
  mytheme

# Listing 11.6 Line chart displaying the growth of five orange trees
library(ggplot2)
ggplot(data=Orange,
        aes(x=age, y=circumference, linetype=Tree, color=Tree)) +
  geom_point() +
  geom_line(size=1) +
  scale_color_brewer(palette="Set1") +
  labs(title="Orange Tree Growth",
       x = "Age (days)",
       y = "Circumference (mm)") +
  guides(color = guide_legend(reverse = TRUE), 
         linetype = guide_legend(reverse = TRUE)) +
  theme_bw()

# Corrgrams
round(cor(mtcars), 2)

# Figure 11.18 Corrgram of the correlations among the 
# variables in the mtcars data frame. Rows and columns 
# have been reordered using principal components analysis.
library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of mtcars intercorrelations")

# Figure 11.19 Corrgram of the correlations among the variables in the 
# mtcars data frame. The lower triangle contains smoothed best-fit lines 
# and confidence ellipses, and the upper triangle contains scatter plots. 
# The diagonal panel contains minimum and maximum values. 
# Rows and columns have been reordered using principal components analysis.
library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax, 
         main="Corrgram of mtcars data using scatter plots and ellipses")


# Figure 11.20 Corrgram of the correlations among the variables in 
# the mtcars data frame. The lower triangle is shaded to represent 
# the magnitude and direction of the correlations. 
# Rows and columns have been reordered using principal components analysis. 
# Correlation coefficients are printed in the upper triangle.
corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.cor,
         main="Corrgram of mtcars data using shading and coefficients")

# Corrgram with an alternate color palette (not shown in book)
library(corrgram) 
cols <- colorRampPalette(c("darkgoldenrod4", "burlywood1",
                           "darkkhaki", "darkgreen"))
corrgram(mtcars, order=TRUE, col.regions=cols,
         lower.panel=panel.shade, 
         upper.panel=panel.conf, text.panel=panel.txt,
         main="A Corrgram (or Horse) of a Different Color")


# Figure 11.21Mosaic plot describing Titanic survivors by 
# class, sex, and age
library(vcd)
mosaic(Titanic, shade=TRUE, legend=TRUE, 
       labeling_args=list(gp_labels=(gpar(fontsize=10))))

