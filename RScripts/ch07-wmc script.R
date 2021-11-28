# Nonparametric pairwise multiple comparisons using the Wilcoxon Signed Rank Test
# Probability values are adjusted using the p.adjust function
wmc <- function(formula, data, exact=FALSE, sort=TRUE, method="holm"){

  # setup
  df <- model.frame(formula, data)
  y <- df[[1]]
  x <- as.factor(df[[2]])
 
  
  # reorder levels of x by median y
  if(sort){
    medians <- aggregate(y, by=list(x), FUN=median)[2]
    index <- order(medians)
    x <- factor(x, levels(x)[index])
  }

  groups <- levels(x)
  k <- length(groups)
  
  # summary statistics
  stats <- function(z)(c(N = length(z), Median = median(z), MAD = mad(z)))
  sumstats <- t(aggregate(y, by=list(x), FUN=stats)[2])
  rownames(sumstats) <- c("n", "median", "mad")
  colnames(sumstats) <- groups
  cat("Descriptive Statistics\n\n")
  print(sumstats)
  
  # multiple comparisons
  mc <- data.frame(Group.1=character(0), 
                   Group.2=character(0), 
                   W=numeric(0),
                   p.unadj=numeric(0), 
                   p=numeric(0),
                   stars=character(0),
                   stringsAsFactors=FALSE)
  
  # perform Wilcoxon test
  row <- 0
  for(i in 1:k){
    for(j in 1:k){
      if (j > i){
        row <- row + 1
        y1 <- y[x==groups[i]]
        y2 <- y[x==groups[j]] 
        test <- wilcox.test(y1, y2, exact=exact)
        mc[row,1] <- groups[i]
        mc[row,2] <- groups[j]
        mc[row,3] <- test$statistic
        mc[row,4] <- test$p.value
      }
    }
  }
  mc$p <- p.adjust(mc$p.unadj, method=method)
  
  # add stars
  mc$stars <- " "
  mc$stars[mc$p <   .1] <- "."
  mc$stars[mc$p <  .05] <- "*"
  mc$stars[mc$p <  .01] <- "**"
  mc$stars[mc$p < .001] <- "***"
  names(mc)[6] <- " "
  
  cat("\nMultiple Comparisons (Wilcoxon Rank Sum Tests)\n")
  cat(paste("Probability Adjustment = ", method, "\n\n", sep=""))
  print(mc[-4], right=TRUE)
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  return(invisible(NULL))
  
}

