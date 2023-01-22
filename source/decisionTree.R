library(rpart)
library(rpart.plot)

setwd(dir = "/Users/Gerard/Desktop/MVA/Project/")
df <- read.csv("clustered_data_CA_2020.csv")[,-1]

df <- df[,-which(colnames(df) %in% c("cluster","ID","Start_Time","End_Time"))]

district1 <- c("Del Norte", "Humboldt", "Lake", "Mendocino")
district2 <- c("Lassen", "Modoc", "Plumas", "Shasta", "Siskiyou", "Tehama", "Trinity")
district3 <- c("Butte", "Colusa", "El Dorado", "Glenn", "Nevada", "Placer", "Sacramento", "Sierra", "Sutter", "Yolo","Yuba")
district4 <- c("Alameda", "Contra Costa", "Marin", "Napa", "Santa Clara", "San Francisco","San Mateo", "Solano","Sonoma" )
district5 <- c("Monterey", "Santa Barbara", "San Benito","Santa Cruz","San Luis Obispo" )
district6 <- c("Fresno", "Kern", "Kings", "Madera", "Tulare")
district7 <- c("Los Angeles", "Ventura")
district8 <- c("Riverside", "San Bernardino")
district9 <- c("Inyo", "Mono")
district10 <- c("Alpine", "Amador", "Calaveras", "Merced", "Mariposa", "San Joaquin", "Stanislaus", "Tuolumne")
district11 <- c("Imperial", "San Diego")
district12 <- c("Orange")

df$district = NA
for (i in 1:nrow(df)){
  if (df$County[i] %in% district1){
    df$district[i] <- "district1"
  }
  else if (df$County[i] %in% district2){
    df$district[i] <- "district2"
  } 
  else if (df$County[i] %in% district3){
    df$district[i] <- "district3"
  } 
  else if (df$County[i] %in% district4){
    df$district[i] <- "district4"
  } 
  else if (df$County[i] %in% district5){
    df$district[i] <- "district5"
  } 
  else if (df$County[i] %in% district6){
    df$district[i] <- "district6"
  } 
  else if (df$County[i] %in% district7){
    df$district[i] <- "district7"
  }
  else if (df$County[i] %in% district8){
    df$district[i] <- "district8"
  } 
  else if (df$County[i] %in% district9){
    df$district[i] <- "district9"
  } 
  else if (df$County[i] %in% district10){
    df$district[i] <- "district10"
  } 
  else if (df$County[i] %in% district11){
    df$district[i] <- "district11"
  } 
  else{
    df$district[i] <- "district12"
  } 
}

#Fist we set the data types:
str(df)
df$Weather_Condition <- as.factor(df$Weather_Condition)
df$Season <- as.factor(df$Season)
df$Severity <- as.factor(df$Severity)
df$City <- as.factor(df$City)
df$County <- as.factor(df$County)
df$Month <- as.factor(df$Month)
df$Year <- as.factor(df$Year)
df$Crossing <- as.factor(df$Crossing)
df$Bump <- as.factor(df$Bump)
df$Traffic_Signal <- as.factor(df$Traffic_Signal)
df$Stop <- as.factor(df$Stop)
df$district <- as.factor(df$district)

set.seed(123)
#We separe the data in two differnt parts 1/3 will be used for validation and the other 2/3 will be used to train the model

NUMBERROWS <- nrow(df)
learn <- sample(1:NUMBERROWS, round(0.67*NUMBERROWS))

#We will build our decision tree to predict the severity of our accident
training <- df[learn,]
tree1 <- rpart(Severity ~ Weather_Condition + Crossing + Bump + Stop + Traffic_Signal + Visibility.mi. 
                        + Precipitation.in. + Humidity... + Temperature.F. + Distance.mi.  
                        + Pressure.in. + Season + district, 
                          data = training)
rpart.plot(tree1)

tree2 <- rpart(Severity ~ Weather_Condition + Crossing + Bump + Stop + Traffic_Signal + Visibility.mi. 
               + Precipitation.in. + Humidity... + Temperature.F. + Distance.mi.  
               + Pressure.in. + Season + district, data = training, control=rpart.control(cp=0.005,minsplit = 200,maxdepth = 20))
rpart.plot(tree2)

tree3 <- rpart(Severity ~ Weather_Condition + Crossing + Bump + Stop + Traffic_Signal + Visibility.mi. 
               + Precipitation.in. + Humidity... + Temperature.F. + Distance.mi.  
               + Pressure.in. + Season + district, data = training, control=rpart.control(cp=0.005,minsplit = 1000,maxdepth = 20))
rpart.plot(tree3)

tree4 <- rpart(Severity ~ Weather_Condition + Crossing + Bump + Stop + Traffic_Signal + Visibility.mi. 
               + Precipitation.in. + Humidity... + Temperature.F. + Distance.mi.  
               + Pressure.in. + Season + district, data = training, control=rpart.control(cp=0.0006,minsplit = 1000,maxdepth = 20))
rpart.plot(tree4)

tree5 <- rpart(Severity ~ Weather_Condition + Crossing + Bump + Stop + Traffic_Signal + Visibility.mi. 
               + Precipitation.in. + Humidity... + Temperature.F. + Distance.mi.  
               + Pressure.in. + Season + district, data = training, control=rpart.control(cp=0.05,minsplit = 200,maxdepth = 20))
rpart.plot(tree5)

tree6 <- rpart(Severity ~ Weather_Condition + Crossing + Bump + Stop + Traffic_Signal + Visibility.mi. 
               + Precipitation.in. + Humidity... + Temperature.F. + Distance.mi.  
               + Pressure.in. + Season + district, data = training, control=rpart.control(cp=0.5,minsplit = 200,maxdepth = 20))
rpart.plot(tree6,box.palette="Blue")

tree7 <- rpart(Severity ~ Weather_Condition + Crossing + Bump + Stop + Traffic_Signal + Visibility.mi. 
               + Precipitation.in. + Humidity... + Temperature.F. + Distance.mi.  
               + Pressure.in. + Season + district, data = training, control=rpart.control(cp=0.001,minsplit = 1000,maxdepth = 20))
rpart.plot(tree7)
# Predictions

prediction1 <- predict(tree1, newdata = training)
prediction2 <- predict(tree2, newdata = training)
prediction3 <- predict(tree3, newdata = training)
prediction4 <- predict(tree4, newdata = training)
prediction5 <- predict(tree5, newdata = training)
prediction6 <- predict(tree6, newdata = training)
prediction7 <- predict(tree7, newdata = training)
# Calculate the error rate in the learning sample
nlearn <- length(learn)

# Tree 1
predClass1 <- apply(prediction1,1,which.max)
confusionMatrix1 <- table(training$Severity,predClass1)
confusionMatrix1
error_rate1.learn <- 100*(1-(confusionMatrix1[1,1]+confusionMatrix1[2,2]+confusionMatrix1[3,3])/nlearn)
error_rate1.learn

# Tree 2
predClass2 <- apply(prediction2,1,which.max)
confusionMatrix2 <- table(training$Severity,predClass2)
confusionMatrix2
error_rate2.learn <- 100*(1-(confusionMatrix2[1,1]+confusionMatrix2[2,2]+confusionMatrix2[3,3]+confusionMatrix2[4,4])/nlearn) 
error_rate2.learn

# Tree 3
predClass3 <- apply(prediction3,1,which.max)
confusionMatrix3 <- table(training$Severity,predClass3)
confusionMatrix3
error_rate3.learn <- 100*(1-(confusionMatrix3[1,1]+confusionMatrix3[2,2]+confusionMatrix3[3,3])/nlearn) 
error_rate3.learn

# Tree 4
predClass4 <- apply(prediction4,1,which.max)
confusionMatrix4 <- table(training$Severity,predClass4)
confusionMatrix4
error_rate4.learn <- 100*(1-(confusionMatrix4[1,1]+confusionMatrix4[2,2]+confusionMatrix4[3,3])/nlearn) 
error_rate4.learn

# Tree 5
predClass5 <- apply(prediction5,1,which.max)
confusionMatrix5 <- table(training$Severity,predClass5)
confusionMatrix5
error_rate5.learn <- 100*(1-(confusionMatrix5[1,1]+confusionMatrix5[2,2])/nlearn) 
error_rate5.learn

# Tree 6
predClass6 <- apply(prediction6,1,which.max)
confusionMatrix6 <- table(training$Severity,predClass6)
confusionMatrix6
error_rate6.learn <- 100*(1-(confusionMatrix6[1,1])/nlearn)
error_rate6.learn

# Tree 7
predClass7 <- apply(prediction7,1,which.max)
confusionMatrix7 <- table(training$Severity,predClass7)
confusionMatrix7
error_rate7.learn <- 100*(1-(confusionMatrix7[1,1]+confusionMatrix7[2,2]+confusionMatrix7[3,3])/nlearn) 
error_rate7.learn

errorvec <- c(error_rate1.learn,error_rate2.learn,error_rate3.learn,error_rate4.learn,error_rate5.learn,error_rate6.learn,error_rate7.learn)
errorvec[which.min(errorvec)]
#The best is tree 4

# ROC tree 4
library(ROCit)
test = df[-learn,]
testPredict4 <- predict(tree4,newdata = test)
predTest4 <- apply(testPredict4,1,which.max)
par(mfrow = c(2,2))
# Severity 1
severity1.roc <- as.data.frame(cbind(ifelse(test$Severity == "Severity1",1,0),
                                     ifelse(predTest4 == 1,1,0)))

ROCit_obj.1 <- rocit(score=severity1.roc[,2],class=severity1.roc[,1])
plot(ROCit_obj.1)

# Severity 2
severity2.roc <- as.data.frame(cbind(ifelse(test$Severity == "Severity2",1,0),
                                     ifelse(predTest4 == 2,1,0)))

ROCit_obj.2 <- rocit(score=severity2.roc[,2],class=severity2.roc[,1])
plot(ROCit_obj.2)

# Severity 3
severity3.roc <- as.data.frame(cbind(ifelse(test$Severity == "Severity3",1,0),
                                     ifelse(predTest4 == 3,1,0)))

ROCit_obj.3 <- rocit(score=severity3.roc[,2],class=severity3.roc[,1])
roc3  = plot(ROCit_obj.3)

# Severity 4
severity4.roc <- as.data.frame(cbind(ifelse(test$Severity == "Severity4",1,0),
                                     ifelse(predTest4 == 4,1,0)))

ROCit_obj.4 <- rocit(score=severity4.roc[,2],class=severity4.roc[,1])
plot(ROCit_obj.4)

#ROC tree 7
testPredict7 <- predict(tree7,newdata = test)
predTest7 <- apply(testPredict7,1,which.max)
# Severity 1
severity1.roc <- as.data.frame(cbind(ifelse(test$Severity == "Severity1",1,0),
                                     ifelse(predTest7 == 1,1,0)))

ROCit_obj.1 <- rocit(score=severity1.roc[,2],class=severity1.roc[,1])
plot(ROCit_obj.1)

# Severity 2
severity2.roc <- as.data.frame(cbind(ifelse(test$Severity == "Severity2",1,0),
                                     ifelse(predTest7 == 2,1,0)))

ROCit_obj.2 <- rocit(score=severity2.roc[,2],class=severity2.roc[,1])
plot(ROCit_obj.2)

# Severity 3
severity3.roc <- as.data.frame(cbind(ifelse(test$Severity == "Severity3",1,0),
                                     ifelse(predTest7 == 3,1,0)))

ROCit_obj.3 <- rocit(score=severity3.roc[,2],class=severity3.roc[,1])
roc3  = plot(ROCit_obj.3)

# Severity 4
severity4.roc <- as.data.frame(cbind(ifelse(test$Severity == "Severity4",1,0),
                                     ifelse(predTest7 == 4,1,0)))

ROCit_obj.4 <- rocit(score=severity4.roc[,2],class=severity4.roc[,1])
plot(ROCit_obj.4)

#ROC tree 2
testPredict2 <- predict(tree2,newdata = test)
predTest2 <- apply(testPredict2,1,which.max)
# Severity 1
severity1.roc <- as.data.frame(cbind(ifelse(test$Severity == "Severity1",1,0),
                                     ifelse(predTest2 == 1,1,0)))

ROCit_obj.1 <- rocit(score=severity1.roc[,2],class=severity1.roc[,1])
plot(ROCit_obj.1)

# Severity 2
severity2.roc <- as.data.frame(cbind(ifelse(test$Severity == "Severity2",1,0),
                                     ifelse(predTest2 == 2,1,0)))

ROCit_obj.2 <- rocit(score=severity2.roc[,2],class=severity2.roc[,1])
plot(ROCit_obj.2)

# Severity 3
severity3.roc <- as.data.frame(cbind(ifelse(test$Severity == "Severity3",1,0),
                                     ifelse(predTest2 == 3,1,0)))

ROCit_obj.3 <- rocit(score=severity3.roc[,2],class=severity3.roc[,1])
roc3  = plot(ROCit_obj.3)

# Severity 4
severity4.roc <- as.data.frame(cbind(ifelse(test$Severity == "Severity4",1,0),
                                     ifelse(predTest2 == 4,1,0)))

ROCit_obj.4 <- rocit(score=severity4.roc[,2],class=severity4.roc[,1])
plot(ROCit_obj.4)

# Error rate in the test sample

predTest <- apply(testPredict2,1,which.max)
confusionMatrixTest <- table(test$Severity,predTest)
confusionMatrixTest
error_rate.test <- 100*(1-(confusionMatrixTest[1,1]+confusionMatrixTest[2,2]+confusionMatrixTest[3,3]+confusionMatrixTest[4,4])/nlearn) 
error_rate.test

predTest <- apply(testPredict7,1,which.max)
confusionMatrixTest <- table(test$Severity,predTest)
confusionMatrixTest
error_rate.test <- 100*(1-(confusionMatrixTest[1,1]+confusionMatrixTest[2,2]+confusionMatrixTest[3,3])/nlearn) 
error_rate.test

testPredict5 <- predict(tree5,newdata = test)
predTest <- apply(testPredict5,1,which.max)
confusionMatrixTest <- table(test$Severity,predTest)
confusionMatrixTest
error_rate.test <- 100*(1-(confusionMatrixTest[1,1]+confusionMatrixTest[2,2])/nlearn) 
error_rate.test
