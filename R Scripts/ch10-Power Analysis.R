#-------------------------------------------------------------------#
# R in Action (3rd ed): Chapter 10                                  #
# Power analysis                                                    #
# requires the pwr and ggplot2 packages                             #
# install.packages(c("pwr", "ggplot2"))                             #
#-------------------------------------------------------------------#

# t-tests
library(pwr)
pwr.t.test(d=.8, sig.level=.05, power=.9, type="two.sample",         
           alternative="two.sided")
pwr.t.test(n=20, d=.5, sig.level=.01, type="two.sample", 
           alternative="two.sided")

# ANOVA
pwr.anova.test(k=5, f=.25, sig.level=.05, power=.8)

# correlations
pwr.r.test(r=.25, sig.level=.05, power=.90, alternative="greater")

# linear models
pwr.f2.test(u=3, f2=0.0769, sig.level=0.05, power=0.90)

# tests of proportions
pwr.2p.test(h=ES.h(.65, .6), sig.level=.05, power=.9, 
            alternative="greater")

# chi-square tests
prob <- matrix(c(.42, .28, .03, .07, .10, .10), byrow=TRUE, nrow=3)
ES.w2(prob)
pwr.chisq.test(w=.1853, df=2, sig.level=.05, power=.9)

# Listing 10.1 Sample sizes for detecting significant effects in a one-way ANOVA
library(pwr)
es <- seq(.1, .5, .01)                                               
nes <- length(es)

samsize <- NULL                                                      
for (i in 1:nes){                                                    
  result <- pwr.anova.test(k=5, f=es[i], sig.level=.05, power=.9)  
  samsize[i] <- ceiling(result$n)                                  
}                                                                    

plotdata <- data.frame(es, samsize)
library(ggplot2)
ggplot(plotdata, aes(x=samsize, y=es)) +
  geom_line(color="red", size=1) +
  theme_bw() +
  labs(title="One Way ANOVA (5 groups)",
       subtitle="Power = 0.90,  Alpha = 0.05",
       x="Sample Size (per group)",
       y="Effect Size") 

# Listing 10.2 Sample size curves for detecting correlations of various sizes
library(pwr)
r <- seq(.1,.5,.01)                                         
p <- seq(.4,.9,.1)                                        

df <- expand.grid(r, p)
colnames(df) <- c("r", "p")

for (i in 1:nrow(df)){                                       
  result <- pwr.r.test(r = df$r[i],
                       sig.level = .05, power = df$p[i],
                       alternative = "two.sided")
  df$n[i] <- ceiling(result$n)
}

library(ggplot2)                                             
ggplot(data=df,
       aes(x=r, y=n, color=factor(p))) +
  geom_line(size=1) +
  theme_bw() +
  labs(title="Sample Size Estimation for Correlation Studies",
       subtitle="Sig=0.05 (Two-tailed)",
       x="Correlation Coefficient (r)",
       y="Samsple Size (n)",
       color="Power")

