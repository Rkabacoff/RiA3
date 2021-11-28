#-----------------------------------------------------------------------#
# R in Action (3rd ed): Chapter 17                                      #
# Classification                                                        #
# requires packaged rpart, partykit, randomForest, e1071, DALEX, rattle #
# install.packages(c("rpart", "partykit", "randomForest",               #
#                     "e1071","rattle", "DALEX")                        #
#-----------------------------------------------------------------------#

# Listing 17.1 - Prepare the breast cancer data
loc <- "http://archive.ics.uci.edu/ml/machine-learning-databases"
ds  <- "breast-cancer-wisconsin/breast-cancer-wisconsin.data"
url <- paste(loc, ds, sep="/")

breast <- read.table(url, sep=",", header=FALSE, na.strings="?")
names(breast) <- c("ID", "clumpThickness", "sizeUniformity",
                   "shapeUniformity", "maginalAdhesion", 
                   "singleEpithelialCellSize", "bareNuclei", 
                   "blandChromatin", "normalNucleoli", "mitosis", 
                   "class")

df <- breast[-1]
df$class <- factor(df$class, levels=c(2,4), 
                   labels=c("benign", "malignant"))
df <- na.omit(df)

set.seed(1234)
index <- sample(nrow(df), 0.7*nrow(df))
train <- df[index,]
test <- df[-index,]
table(train$class)
table(test$class)

# Listing 17.2 - Logistic regression with glm()
fit.logit <- glm(class~., data=train, family=binomial())
summary(fit.logit)
prob <- predict(fit.logit, test, type="response")
logit.pred <- factor(prob > .5, levels=c(FALSE, TRUE), 
                     labels=c("benign", "malignant"))
logit.perf <- table(test$class, logit.pred,
                    dnn=c("Actual", "Predicted"))
logit.perf

# stepwise logistic regression
logit.fit.reduced <- step(fit.logit)
prob <- predict(logit.fit.reduced, test, type="response")
logit.reduced.pred <- factor(prob > .5, levels=c(FALSE, TRUE), 
                             labels=c("benign", "malignant"))
logit.reduced.perf <- table(test$class, logit.reduced.pred,
                            dnn=c("Actual", "Predicted"))
logit.reduced.perf

# Listing 17.3 - Creating a classical decision tree with rpart()
library(rpart)
dtree <- rpart(class ~ ., data=train, method="class")
dtree$cptable
plotcp(dtree)

dtree.pruned <- prune(dtree, cp=0.01705) 

library(rattle)
fancyRpartPlot(dtree.pruned,  sub="Decision Tree")

dtree.pred <- predict(dtree.pruned, test, type="class")
dtree.perf <- table(test$class, dtree.pred, 
                    dnn=c("Actual", "Predicted"))
dtree.perf


# Listing 17.4 - Creating a conditional inference tree with ctree()
library(partykit)
fit.ctree <- ctree(class~., data=train)

plot(fit.ctree, main="Conditional Inference Tree", 
     gp=gpar(fontsize=8))

ctree.pred <- predict(fit.ctree, test, type="response")
ctree.perf <- table(test$class, ctree.pred, 
                    dnn=c("Actual", "Predicted"))
ctree.perf


# Listing 17.5 - Random forest
library(randomForest)
set.seed(1234)
fit.forest <- randomForest(class~., data=train,
                          importance=TRUE)             
fit.forest
randomForest::importance(fit.forest, type=2)                          
forest.pred <- predict(fit.forest, test)         
forest.perf <- table(test$class, forest.pred, 
                     dnn=c("Actual", "Predicted"))
forest.perf


# Listing 17.6 - A support vector machine
library(e1071)
set.seed(1234)
fit.svm <- svm(class~., data=train)
fit.svm
svm.pred <- predict(fit.svm, test)
svm.perf <- table(test$class, 
                  svm.pred, dnn=c("Actual", "Predicted"))
svm.perf


# Listing 17.7 Tuning an RBF support vector machine (this can take a while)
set.seed(1234)
tuned <- tune.svm(class~., data=train,
                  gamma=10^(-6:1),
                  cost=10^(-10:10))
tuned
set.seed(1234)
fit.svm <- svm(class~., data=train, gamma=.01, cost=1)
svm.pred <- predict(fit.svm, test)
svm.perf <- table(test$class,
                  svm.pred, dnn=c("Actual", "Predicted"))
svm.perf


# Listing 17.8 Function for assessing binary classification accuracy
performance <- function(table, n=2){
  if(!all(dim(table) == c(2,2)))
    stop("Must be a 2 x 2 table")
  tn = table[1,1]
  fp = table[1,2]
  fn = table[2,1]
  tp = table[2,2]
  sensitivity = tp/(tp+fn)
  specificity = tn/(tn+fp)
  ppp = tp/(tp+fp)
  npp = tn/(tn+fn)
  hitrate = (tp+tn)/(tp+tn+fp+fn)
  result <- paste("Sensitivity = ", round(sensitivity, n) ,
                  "\nSpecificity = ", round(specificity, n),
                  "\nPositive Predictive Value = ", round(ppp, n),
                  "\nNegative Predictive Value = ", round(npp, n),
                  "\nAccuracy = ", round(hitrate, n), "\n", sep="")
  cat(result)
}


# Listing 17.9 - Performance of breast cancer data classifiers
performance(logit.perf)
performance(dtree.perf)
performance(ctree.perf)
performance(forest.perf)
performance(svm.perf)

# Understanding individual black box predictions

# Listing 17.10 Using a break down plot to understand a black box prediction
library(DALEX)

alex <- data.frame(
  clumpThickness = 6,
  sizeUniformity = 1,
  shapeUniformity = 1, 
  maginalAdhesion = 1,
  singleEpithelialCellSize = 3,
  bareNuclei = 9,
  blandChromatin = 7,
  normalNucleoli = 3,
  mitosis = 3
  )

predict(fit.forest, alex, type="prob")

set.seed(1234)
explainer_rf_malignant <- 
  explain(fit.forest, data = train,  y = train$class == "malignant",
  predict_function = function(m,x) predict(m,x, type = "prob")[,2])


rf_pparts <- predict_parts(explainer=explainer_rf_malignant,
                           new_observation = alex, 
                           type = "break_down")
rf_pparts
plot(rf_pparts)

# Plotting shapely values 
set.seed(1234)
rf_pparts = predict_parts(explainer = explainer_rf_malignant, 
                          new_observation = alex, 
                          type = "shap")
rf_pparts
plot(rf_pparts)



# variable importance (permutation based)
mp <- model_parts(explainer_rf_malignant)
mp
plot(mp)


