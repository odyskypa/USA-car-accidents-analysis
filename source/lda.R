# Importing Libraries
library(MASS)
library(car)
library(stringr)
library(caret)
library(pROC)

# Setting-up Working Environment
working_dir = "C:/Users/odyky/Desktop/UPC/1st Semester/MVA-MDS/Group project/MVA-Project-Code/USAccidents"
plot_dir = "C:/Users/odyky/Desktop/UPC/1st Semester/MVA-MDS/Group project/MVA-Project-Code/Plots/DiscriminantAnalysis"
dir.create(plot_dir, showWarnings = FALSE) # Creating the folder for saving the plots of Discriminant Analysis
setwd(working_dir)

# Loading Data and Basic Summary
df <- read.csv("../US_Accidents_DT.csv", sep=",",dec=".",stringsAsFactors = TRUE)
df <- df[-c(1)]
dim(df)
names(df)
summary(df)

# Defining Functions
calcWithinGroupsVariance <- function(variable,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # get the mean and standard deviation for each group:
  numtotal <- 0
  denomtotal <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- variable[groupvariable==leveli,]
    levelilength <- length(levelidata)
    # get the standard deviation for group i:
    sdi <- sd(levelidata)
    numi <- (levelilength - 1)*(sdi * sdi)
    denomi <- levelilength
    numtotal <- numtotal + numi
    denomtotal <- denomtotal + denomi
  }
  # calculate the within-groups variance
  Vw <- numtotal / (denomtotal - numlevels)
  return(Vw)
}

groupStandardise <- function(variables, groupvariable)
{
  # find out how many variables we have
  variables <- as.data.frame(variables)
  numvariables <- length(variables)
  # find the variable names
  variablenames <- colnames(variables)
  # calculate the group-standardised version of each variable
  for (i in 1:numvariables)
  {
    variablei <- variables[i]
    variablei_name <- variablenames[i]
    variablei_Vw <- calcWithinGroupsVariance(variablei, groupvariable)
    variablei_mean <- mean(as.matrix(variablei))  
    variablei_new <- (variablei - variablei_mean)/(sqrt(variablei_Vw))
    data_length <- nrow(variablei)
    if (i == 1) { variables_new <- data.frame(row.names=seq(1,data_length)) }
    variables_new[`variablei_name`] <- variablei_new
  }
  return(variables_new)
}

calcBetweenGroupsVariance <- function(variable,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # calculate the overall grand mean:
  grandmean <- mean(as.matrix(variable) )         
  # get the mean and standard deviation for each group:
  numtotal <- 0
  denomtotal <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- variable[groupvariable==leveli,]
    levelilength <- length(levelidata)
    # get the mean and standard deviation for group i:
    meani <- mean( as.matrix(levelidata) )
    sdi <- sd(levelidata)
    numi <- levelilength * ((meani - grandmean)^2)
    denomi <- levelilength
    numtotal <- numtotal + numi
    denomtotal <- denomtotal + denomi
  }
  # calculate the between-groups variance
  Vb <- numtotal / (numlevels - 1)
  Vb <- Vb[[1]]
  return(Vb)
}

calcSeparations <- function(variables,groupvariable)
{
  # find out how many variables we have
  variables <- as.data.frame(variables)
  numvariables <- length(variables)
  # find the variable names
  variablenames <- colnames(variables)
  # calculate the separation for each variable
  for (i in 1:numvariables)
  {
    variablei <- variables[i]
    variablename <- variablenames[i]
    Vw <- calcWithinGroupsVariance(variablei, groupvariable)
    Vb <- calcBetweenGroupsVariance(variablei, groupvariable)
    sep <- Vb/Vw
    print(paste("variable",variablename,"Vw=",Vw,"Vb=",Vb,"separation=",sep))
  }
}

calcAllocationRuleAccuracy <- function(ldavalue, groupvariable, cutoffpoints)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # calculate the number of true positives and false negatives for each group
  numlevels <- length(levels)
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- ldavalue[groupvariable==leveli]
    # see how many of the samples from this group are classified in each group
    for (j in 1:numlevels)
    {
      levelj <- levels[j]
      if (j == 1)
      {
        cutoff1 <- cutoffpoints[1]
        cutoff2 <- "NA"
        results <- summary(levelidata <= cutoff1)
      }
      else if (j == numlevels)
      {
        cutoff1 <- cutoffpoints[(numlevels-1)]
        cutoff2 <- "NA"
        results <- summary(levelidata > cutoff1)
      }
      else
      {
        cutoff1 <- cutoffpoints[(j-1)]
        cutoff2 <- cutoffpoints[(j)]
        results <- summary(levelidata > cutoff1 & levelidata <= cutoff2)
      }
      trues <- results["TRUE"]
      trues <- trues[[1]]
      print(paste("Number of samples of group",leveli,"classified as group",levelj," : ",
                  trues,"(cutoffs:",cutoff1,",",cutoff2,")"))
    }
  }
}

printMeanAndSdByGroup <- function(variables,groupvariable)
{
  # find the names of the variables
  variablenames <- c(names(groupvariable),names(as.data.frame(variables)))
  # within each group, find the mean of each variable
  groupvariable <- groupvariable[,1] # ensures groupvariable is not a list
  means <- aggregate(as.matrix(variables) ~ groupvariable, FUN = mean)
  names(means) <- variablenames
  print(paste("Means:"))
  print(means)
  # within each group, find the standard deviation of each variable:
  sds <- aggregate(as.matrix(variables) ~ groupvariable, FUN = sd)
  names(sds) <- variablenames
  print(paste("Standard deviations:"))
  print(sds)
  # within each group, find the number of samples:
  samplesizes <- aggregate(as.matrix(variables) ~ groupvariable, FUN = length)
  names(samplesizes) <- variablenames
  print(paste("Sample sizes:"))
  print(samplesizes)
}

# Applying Linear Discriminant Analysis

# Splitting data to training and test sets.
# 1/3 will be used for training and 2/3 for testing the model.
set.seed(123)
NUMBERROWS <- nrow(df)
learn <- sample(1:NUMBERROWS, round(0.67*NUMBERROWS))

training_full <- df[learn,]
test_full = df[-learn,]
training <-cbind(training_full[,c(6:11)], training_full[13])
test <-cbind(test_full[,c(6:11)], test_full[13])


# TRAINING STEP

# Using only the numerical values of the data set.
# If the variables are standardized, the results obtained
# are usually easier to interpret.
training.Standard <- groupStandardise(training[,c(1:6)], training[7])
training.Standard$Severity <- training$Severity
test.Standard <- groupStandardise(test[,c(1:6)], test[7])
test.Standard$Severity <- test$Severity


summary(training.Standard)
summary(test.Standard)

training.lda <- lda(Severity ~., data = training.Standard)

training.lda


# Coefficients of the Discriminant Functions
training.lda$scaling[,1:3]

# Values of each case for the first discriminant function
training.lda.values <- predict(training.lda, training.Standard[,c(1:6)])

LDA1<- training.lda.values$x[,1]
LDA2<- training.lda.values$x[,2]
LDA3<- training.lda.values$x[,3]

# Separation given by the discriminant functions.
calcSeparations(training.lda.values$x,training.Standard$Severity)
# Total Separation -> 752.2910036642373
(training.lda$svd)^2 # This command returns the separation of the groups
# Same results with calcSeparations function


# Stacked Histograms of the LDA Values
ldahist(data = LDA1, g=training.Standard$Severity, main="Stacked Histogram of the 1st Discriminant Function’s Values")

ldahist(data = LDA2, g=training.Standard$Severity, main="Stacked Histogram of the 2nd Discriminant Function’s Values")

ldahist(data = LDA3, g=training.Standard$Severity, main="Stacked Histogram of the 3rd Discriminant Function’s Values")

#plot  of LDA axis and observations
SeverityNum = str_sub(training.Standard$Severity,-1)
plot(LDA1,LDA2, main="LDA1 and LDA2 Projection of Training Data",
     xlab="LDA1", ylab="LDA2")
text(LDA1,LDA2, SeverityNum, cex=0.5, pos=4, col=SeverityNum)

# We can glue together the posterior-probabilities and the predicted class
# vector
training_posterior <- training.lda.values$posterior
training_pred <- training.lda.values$class
head(cbind(round(training_posterior, 4), class=training_pred),10)

# plot(training.lda, col=as.numeric(SeverityNum)) # assign color code based on factor code --> APPENDIX

# Confusion matrix (training error)

training_conf_with_lib <- confusionMatrix(data=training_pred, reference = training.Standard$Severity, mode="everything")
training_conf_with_lib
training_conf <- table(list(predicted=training_pred, observed=training.Standard$Severity))
training_conf

# Training Accuracy
training_accuracy<-sum(diag(training_conf))/dim(training.Standard)[1]; training_accuracy

# Training Precision (Positive predicted value)
training_precision <- diag(training_conf) / rowSums(training_conf); training_precision

# Compute Training Missclassification Rate
training_mr <- 1-training_accuracy; training_mr

# ROC for Training Set
train_prob = predict(training.lda, training.Standard[1:6], type = "response")

result = multiclass.roc(training.Standard$Severity, train_prob$posterior)

pdf(paste(plot_dir, "AUC_train", ".pdf", sep=""))
par(mfrow=c(2,3))
for (contrast in names(result$rocs)) {
  plot(result$rocs[[contrast]][[1]], col = "#006666", main = contrast)
  plot(result$rocs[[contrast]][[2]], col = "#666666", add = TRUE)
  line1<-str_split(contrast,"/",simplify = TRUE)[1]
  line2<-str_split(contrast,"/",simplify = TRUE)[2]
  auc1<-auc(result$rocs[[contrast]][[1]])
  auc1<- round(auc1, digits = 3)
  auc2<-auc(result$rocs[[contrast]][[2]])
  auc2<- round(auc2, digits = 3)
  text<- " with AUC: "
  legend(x="topleft",legend=c(paste(line1,text, as.character(auc1)),paste(line2,text, as.character(auc2))),
         col=c("#006666", "#666666"), lwd=5, cex=0.8)
  
}
dev.off()
par(mfrow=c(1,1))

# TESTING STEP

# Values of each case for the first discriminant function
test.lda.values <- predict(training.lda, newdata = test.Standard[1:6])
test_predictions <- test.lda.values$class

# We can glue together the posterior-probabilities and the predicted class
# vector
test_posterior <- test.lda.values$posterior
head(cbind(round(test_posterior, 4), class=test_predictions),10)

# Confusion matrix (test error)

test_conf_with_lib <- confusionMatrix(data=test_predictions, reference = test.Standard$Severity, mode="everything")
test_conf_with_lib
test_conf <- table(list(predicted=test_predictions, observed=test.Standard$Severity))
test_conf

# test Accuracy
test_accuracy<-sum(diag(test_conf))/dim(test.Standard)[1]; test_accuracy

# test Precision (Positive predicted value)
test_precision <- diag(test_conf) / rowSums(test_conf); test_precision

# Compute test Missclassification Rate
test_mr <- 1-test_accuracy; test_mr

# ROC for test Set
test_prob = predict(training.lda, test.Standard[1:6], type = "response")
result = multiclass.roc(test.Standard$Severity,test_prob$posterior)

pdf(paste(plot_dir, "AUC_test", ".pdf", sep=""))
par(mfrow=c(2,3))
for (contrast in names(result$rocs)) {
  plot(result$rocs[[contrast]][[1]], col = "#006666", main = contrast)
  plot(result$rocs[[contrast]][[2]], col = "#666666", add = TRUE)
  line1<-str_split(contrast,"/",simplify = TRUE)[1]
  line2<-str_split(contrast,"/",simplify = TRUE)[2]
  auc1<-auc(result$rocs[[contrast]][[1]])
  auc1<- round(auc1, digits = 3)
  auc2<-auc(result$rocs[[contrast]][[2]])
  auc2<- round(auc2, digits = 3)
  text<- " with AUC: "
  legend(x="topleft",legend=c(paste(line1,text, as.character(auc1)),paste(line2,text, as.character(auc2))),
         col=c("#006666", "#666666"), lwd=5, cex=0.8)
  
}
dev.off()
par(mfrow=c(1,1))
