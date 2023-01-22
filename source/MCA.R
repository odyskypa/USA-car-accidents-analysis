library("FactoMineR")
library("factoextra")
library("corrplot")
library(ggplot2)
library(dplyr)
library(FactoClass)
library(ade4)
library(knitr)


#Load data
setwd("~/Documents/MDS/1st_Semester/MVA/Project/")
data<-read.csv("USAccidents_final_dataset.csv")

#Remove columns that are not categorical data
data<-data[,-c(1,3,6,12,13,14,15,16,17,18,19,20)]

#Put Severity at the end of the dataset in order to use it as qualitative
data<-data %>% relocate(Severity, .after = last_col())

#Change all variables into factor
data$State <- as.factor(data$State)
data$Year <- as.factor(data$Year)
data$Weather_Condition <- as.factor(data$Weather_Condition)
data$Crossing = as.factor(data$Crossing)
data$Bump = as.factor(data$Bump)
data$Stop = as.factor(data$Stop)
data$Traffic_Signal = as.factor(data$Traffic_Signal)
data$Season = as.factor(data$Season)
data$Severity = as.factor(data$Severity)


#MCA Analysis
mca_us<-MCA(data, ncp=8, graph = FALSE, quali.sup = 9)


#Dimension selection using Logic Table (1/p)
p<-ncol(data[,1:8]) ###number of active categorical features
logic_table<-1/p
number_selected_dimensions<-length(which(mca_us$eig[,1]>logic_table)) 
number_selected_dimensions ###number of selected dimensions. This indicates to keep the first 12 components.
###The analysis shown in plots will be performed with Dim 1 and Dim 2.


#Scree plot and percentage of variance of dimensions
##Scree Plot
fviz_screeplot(mca_us, addlabels = TRUE, ylim = c(0, 10))

##Dimension variance
eig.val <-get_eigenvalue(mca_us)
eig.val
cum_var <- eig.val[,3]
bp<-barplot(cum_var, xlab = "Components", ylab = "Percentage of Variance", main = "Cumulative Sum of Variance")
text(bp, 0, round(cum_var, 1),cex=1,pos=3)


#Correlation between variables and principal dimensions
##Correlation between variables and MCA principal dimensions
fviz_mca_var(mca_us, choice = "mca.cor",repel = TRUE,ggtheme = theme_minimal())

##Coordinates of modalities
fviz_mca_var(mca_us, repel = TRUE, ggtheme = theme_minimal())


#Contribution of each modality to the dimensions

##Contribution to Dim1
fviz_contrib(mca_us, choice = "var", axes = 1, top = 20)

##Contribution to Dim2
fviz_contrib(mca_us, choice = "var", axes = 2, top = 20)

##Contribution for both dimensions
fviz_contrib(mca_us, choice = "var", axes = 1:2, top = 20)

##Contribution of each modality
fviz_mca_var(mca_us, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal(), title="", labelsize=4)


#Biplot of correlation of modalities with individuals represented
fviz_mca_biplot(mca_us, select.ind = list(contrib = 10605),invisible=c("quali.sup"), repel=TRUE, ggtheme = theme_minimal(), title=" ")

