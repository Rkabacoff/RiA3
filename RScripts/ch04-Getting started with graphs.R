#------------------------------------------------------------#
# R in Action (3rd ed): Chapter 4                            #
# Getting started with graphs                                #
# requires the ggplot2 and mosaicData packages               #
# install.packages(c("ggplot2", "mosaicData"))               #
#------------------------------------------------------------#

# -- Section 4.1 Creating a graph with ggplot2
library(ggplot2)
library(mosaicData)
data(CPS85)

# Figure 4.1 Mapping worker experience and wages to the x and y axes
ggplot(data = CPS85, mapping = aes(x = exper, y = wage))

# Figure 4.2 Scatterplot of worker experience vs wages
ggplot(data = CPS85, mapping = aes(x = exper, y = wage)) +
  geom_point()

# Figure 4.3 Scatterplot of worker experience vs. wages with outlier removed
CPS85 <- CPS85[CPS85$wage < 40, ]
ggplot(data = CPS85, mapping = aes(x = exper, y = wage)) +
  geom_point()

# Figure 4.4 Scatterplot of worker experience vs. wages with outlier removed
# with modified point color, transparency, and point size
ggplot(data = CPS85, mapping = aes(x = exper, y = wage)) +
  geom_point(color = "cornflowerblue", alpha = .7, size = 1.5) +
  theme_bw()

# Figure 4.5 Scatterplot of worker experience vs. wages
# with a line of best fit
ggplot(data = CPS85, 
       mapping = aes(x = exper, y = wage)) +
  geom_point(color = "cornflowerblue", alpha = .7, size = 1.5) +
  theme_bw() +
  geom_smooth(method = "lm")

# Figure 4.6 Scatterplot of worker experience vs. wages
# with points colored by sex and separate line of best
# fit for men and women.
ggplot(data = CPS85, 
       mapping = aes(x = exper, y = wage,
                     color = sex, shape = sex, linetype = sex)) +
  geom_point(alpha = .7, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, size = 1.5) +
  theme_bw()

# Figure 4.7 Scatterplot of worker experience vs. wages
# with custom x- and y-axes and custom color mappings for sex.
ggplot(data = CPS85,
  mapping = aes(x = exper, y = wage,
                color = sex, shape = sex, linetype = sex)) +
  geom_point(alpha = .7, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, size = 1.5) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  scale_color_manual(values = c("indianred3", "cornflowerblue")) +
  theme_bw()

# Figure 4.8 Scatterplot of worker experience vs. wages with
# custom x- and y-axes and custom color mappings for sex.
# Wages are printed in dollar format.
ggplot(
  data = CPS85,
  mapping = aes(x = exper, y = wage,
                color = sex, shape = sex, linetype = sex)) +
  geom_point(alpha = .7, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, size = 1.5) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5),
                     label = scales::dollar) +
  scale_color_manual(values = c("indianred3", "cornflowerblue")) +
  theme_bw()

# Figure 4.9 Scatterplot of worker experience vs. wages with
# custom x- and y-axes and custom color mappings for sex.
# Separate graphs (facets) are provided for each of 8 job sectors.
ggplot(
  data = CPS85,
  mapping = aes(x = exper, y = wage,
                color = sex, shape = sex, linetype = sex)) +
  geom_point(alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5),
                     label = scales::dollar) +
  scale_color_manual(values = c("indianred3", "cornflowerblue")) +
  facet_wrap(~sector) +
  theme_bw()

# Figure 4.10 Scatterplot of worker experience vs. wages
# with separate graphs (facets) for each of 8 job sectors
# and custom titles and labels.
ggplot(
  data = CPS85,
  mapping = aes(x = exper, y = wage,
                color = sex, shape = sex, linetype = sex)) +
  geom_point(alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5),
                     label = scales::dollar) +
  scale_color_manual(values = c("indianred3","cornflowerblue")) +
  facet_wrap(~sector) +
  labs(title = "Relationship between wages and experience",
       subtitle = "Current Population Survey",
       caption = "source: http://mosaic-web.org/",
       x = " Years of Experience",
       y = "Hourly Wage",
       color = "Gender", shape = "Gender", linetype = "Gender") +
  theme_bw()

# Figure 4.11 Scatterplot of worker experience vs. wages
# with separate graphs (facets) for each of 8 job sectors
# and custom titles and labels, and a cleaner theme.

ggplot(data = CPS85,
       mapping = aes(x = exper, y = wage, color = sex, shape=sex,
                     linetype = sex)) +
  geom_point(alpha = .7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5),
                     label = scales::dollar) +
  scale_color_manual(values = c("indianred3", "cornflowerblue")) +
  facet_wrap(~sector) +
  labs(title = "Relationship between wages and experience",
       subtitle = "Current Population Survey",
       caption = "source: http://mosaic-web.org/",
       x = " Years of Experience",
       y = "Hourly Wage",
       color = "Gender", shape="Gender", linetype="Gender") +
  theme_minimal()

# -- Section 4.2 ggplot2 details

# Figure 4.12. Scatterplot of experience and wage by sex,
# where aes(color=sex) is placed in the ggplot() function.
# The mapping is applied to both the geom_point() and geom_smooth(),
# producing separate point colors for males and females,
# along with separate lines of best fit
ggplot(CPS85,
       mapping = aes(x = exper, y = wage, color = sex)) +
  geom_point(alpha = .7, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, size = 1)

# Figure 4.13 Scatterplot of experience and wage by sex,
# where aes(color=sex) is placed in the geom_point() function.
# The mapping is applied to point color producing separate point
# colors for men and women, but a single line of best fit
# for or all workers.
ggplot(CPS85, aes(x = exper, y = wage)) +
  geom_point(aes(color = sex), alpha = .7, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, size = 1)

# Listing 4.1 Using a ggplot2 graph as an object
data(CPS85, package = "mosaicData")
CPS85 <- CPS85[CPS85$wage < 40, ]



myplot <- ggplot(data = CPS85, aes(x = exper, y = wage)) +
  geom_point()

myplot

myplot2 <- myplot + geom_point(size = 3, color = "blue")
myplot2

myplot + geom_smooth(method = "lm") +
  labs(title = "Mildly interesting graph")

# section 4.2.4
# Listing 4.15
ggplot(CPS85, aes(x = exper, y = wage, color = "blue")) +
  geom_point() 
