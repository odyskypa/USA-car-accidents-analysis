library(ggplot2)
library(lsr)
library(dplyr)
library(corrplot)
library(ggpubr)
library(naniar)
library(stringi)
library(class)
library(ltm)
library(dlookr)
library(chemometrics)
library(psych)

setwd("~/Desktop/MDS/MVA/D3/")

data <- read.csv("USAccidents_imputed.csv")
data <- data[2:23]
USAccidents<-read.csv("final_US_Accidents.csv")
USAccidents <- USAccidents[,2:21]
USAccidents_preprocessed <- read.csv("USAccidents_final_dataset.csv")



str(USAccidents)
# Changing vars to factors
USAccidents$State <- as.factor(USAccidents$State)
USAccidents$City <- as.factor(USAccidents$City)
USAccidents$Weather_Condition <- as.factor(USAccidents$Weather_Condition)
USAccidents$Year <- as.factor(USAccidents$Year)
USAccidents$County <- as.factor(USAccidents$County)
USAccidents$Severity <- as.factor(USAccidents$Severity)
USAccidents$Humidity... <- as.numeric(USAccidents$Humidity...)

str(USAccidents_preprocessed)
# Changing vars to factors
USAccidents_preprocessed$State <- as.factor(USAccidents_preprocessed$State)
USAccidents_preprocessed$City <- as.factor(USAccidents_preprocessed$City)
USAccidents_preprocessed$Weather_Condition <- as.factor(USAccidents_preprocessed$Weather_Condition)
USAccidents_preprocessed$Year <- as.factor(USAccidents_preprocessed$Year)
USAccidents_preprocessed$County <- as.factor(USAccidents_preprocessed$County)
USAccidents_preprocessed$Severity <- as.factor(USAccidents_preprocessed$Severity)
USAccidents_preprocessed$Month <- as.factor(USAccidents_preprocessed$Month)
USAccidents_preprocessed$Season <- as.factor(USAccidents_preprocessed$Season)

### Univariate Analysis ###

data %>% 
  correlate(method = "spearman") %>% 
  plot()


# summary - comparison after preprocessing
summary(USAccidents)
summary(USAccidents_preprocessed)

summary(USAccidents$Temperature.F.)
summary(USAccidents_preprocessed$Temperature.F.)

summary(USAccidents$Distance.mi.)
summary(USAccidents_preprocessed$Distance.mi.)

summary(USAccidents$Pressure.in.)
summary(USAccidents_preprocessed$Pressure.in.)


#comparing number of NAs after inputation
USAccidents %>% summarise_all(~ sum(is.na(.)))
USAccidents_preprocessed %>% summarise_all(~ sum(is.na(.)))

# Descriptive Analysis for Numerical Variables

quantiCols<-colnames(select_if(USAccidents_preprocessed, is.numeric))
quantiData = USAccidents_preprocessed[quantiCols]
quantiCols

# standard deviation
lapply(quantiData, sd)

Visibility<-USAccidents_preprocessed$Visibility.mi.
Percipitation<-USAccidents_preprocessed$Precipitation.in.
Humidity<-USAccidents_preprocessed$Humidity...
Temperature<-USAccidents_preprocessed$Temperature.F.
Distance<-USAccidents_preprocessed$Distance.mi.
Pressure<-USAccidents_preprocessed$Pressure.in.

# histograms
summary(Visibility)
hVis <- ggplot(USAccidents_preprocessed, aes(x=Visibility)) + 
  geom_histogram(color="darkred", fill="pink")


summary(Percipitation)
hPrec <-ggplot(USAccidents_preprocessed, aes(x=Percipitation)) + 
  geom_histogram(color="darkblue", fill="lightblue")
# consequence of inputing 0 for NAs

summary(Humidity)
hHumid <- ggplot(USAccidents_preprocessed, aes(x=Humidity)) + 
  geom_histogram(color="darkgreen", fill="lightgreen")

summary(Temperature)
hTemp<-ggplot(USAccidents_preprocessed, aes(x=Temperature)) + 
  geom_histogram(color="gold4", fill="gold")

summary(Distance)
hDist<- ggplot(USAccidents_preprocessed, aes(x=Distance)) + 
  geom_histogram(color="darkorchid4", fill="darkorchid1")

summary(Pressure)
hPres<-ggplot(USAccidents_preprocessed, aes(x=Pressure)) + 
  geom_histogram(color="darkblue", fill="lightblue")

ggarrange(hVis, hPrec, hHumid, hTemp, hDist, hPres, 
          ncol = 3, nrow = 2)

# boxplots
boxplot(Visibility, Percipitation, Humidity, Temperature, Distance, Pressure,
        las ="2",names = c("Visibility","Percipit.","Humidity","Temp.","Distance","Pressure"))

#Visibility outlier detection
boxplot(Visibility)

out <- boxplot.stats(Visibility)$out
out_ind <- which(Visibility %in% c(out))
length(out_ind)
USAccidents_preprocessed[out_ind,]

#Precipitation outlier detection
boxplot(Percipitation)

out <- boxplot.stats(Percipitation)$out
out_ind <- which(Percipitation %in% c(out))
length(out_ind)
USAccidents_preprocessed[out_ind,]

#Humidity outlier detection
boxplot(Humidity)

out <- boxplot.stats(Humidity)$out
out_ind <- which(Humidity %in% c(out))
length(out_ind)
USAccidents_preprocessed[out_ind,]

#Temperature outlier detection
boxplot(Temperature)

out <- boxplot.stats(Temperature)$out
out_ind <- which(Temperature %in% c(out))
length(out_ind)
USAccidents_preprocessed[out_ind,]

#Distance outlier detection
boxplot(Distance)

out <- boxplot.stats(Distance)$out
out_ind <- which(Distance%in% c(out))
length(out_ind)
USAccidents_preprocessed[out_ind,]

#Pressure outlier detection
boxplot(Pressure)

out <- boxplot.stats(Pressure)$out
out_ind <- which(Pressure %in% c(out))
length(out_ind)
USAccidents_preprocessed[out_ind,]



# Descriptive Analysis for Qualitative Variables
qualiCols= colnames(select_if(USAccidents_preprocessed, is.factor))
qualiData = USAccidents_preprocessed[qualiCols]
qualiCols

table(USAccidents_preprocessed['Weather_Condition'])

ggplot(USAccidents_preprocessed, aes(x = Weather_Condition)) + 
  geom_bar(fill = "cornflowerblue", 
           color="black") +
  labs(x = "Weather Condition", 
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(USAccidents_preprocessed, aes(x = Season)) + 
  geom_bar(fill = "lightblue", 
           color="darkblue") +
  labs(x = "Season", 
       y = "Frequency")


#pie charts for those with few levels
par(mfrow=c(1,3))

years = table(USAccidents_preprocessed$Year)
pie(years,  main= "Year", labels = paste(round(prop.table(years)*100), "%", sep = ""), col = rainbow(length(years))) 
legend("topright", c("2016","2017","2018","2019", "2020", "2021"), cex = 0.8,
       fill = rainbow(length(years)))

states = table(USAccidents_preprocessed$State)
pie(states,  main= "State", labels = paste(round(prop.table(states)*100), "%", sep = ""), col = rainbow(length(states))) 
legend("topright", c("CA","CT","PA","SC", "TN"), cex = 0.8,
       fill = rainbow(length(states)))

severity = table(USAccidents_preprocessed$Severity)
pie(severity,  main= "Severity", labels = paste(round(prop.table(severity)*100), "%", sep = ""), col = rainbow(length(severity))) 
legend("topright", c("1","2","3","4"), cex = 0.8,
       fill = rainbow(length(severity)))


#since country and city have many levels, most frequent 5 are considered for visualization
top5Cities = head(sort(table(USAccidents_preprocessed$City), decreasing = TRUE), 5)
top5Counties = head(sort(table(USAccidents_preprocessed$County), decreasing = TRUE), 5)

par(mfrow=c(1,2))
barplot(top5Cities,
        xlab = "Cities",
        ylab = "Count",
        axes = TRUE)
pie(top5Cities,  main= "Top 5 Cities", labels = paste(round(prop.table(top5Cities)*100), "%", sep = ""), col = rainbow(length(top5Cities))) 
legend("topright", c("Los Angeles","Sacramento","San Diego","Nashville", "Riverside"), cex = 0.8,
       fill = rainbow(length(top5Cities)))


par(mfrow=c(1,2))
barplot(top5Counties,
        xlab = "Counties",
        ylab = "Count",
        axes = TRUE)
pie(top5Counties,  main= "Top 5 Counties", labels = paste(round(prop.table(top5Counties)*100), "%", sep = ""), col = rainbow(length(top5Counties))) 
legend("topright", c("Los Angeles","San Bernardino","Orange","San Diego", "Sacramento"), cex = 0.8,
       fill = rainbow(length(top5Counties)))



### Bivariate Analysis ###
par(mfrow=c(1,1))

## 1 categorical and 1 numerical
ggplot(USAccidents_preprocessed, 
       aes(x = Weather_Condition, 
           y = Visibility)) +
  geom_boxplot() +
  labs(title = "Visibility distribution by Weather Condition")


## 2 numerical variables
ggplot(USAccidents_preprocessed, 
       aes(x = Distance, 
           y = Visibility)) +
  geom_point()


## 2 categorical variables

##Information about the relation between Severity and the binary variables (Crossing, Bump, Stop and Traffic_Signal)

###Looking at the the number of accidents with each type of severity and looking at how many there was a crossing present
ggplot(USAccidents_preprocessed, aes(x = Severity, fill = Crossing)) +
  geom_bar()

###Looking at the the number of accidents with each type of severity and looking at how many there was a bump present
ggplot(USAccidents_preprocessed, aes(x = Severity, fill = Bump)) +
  geom_bar()

###Looking at the the number of accidents with each type of severity and looking at how many there was a stop present
ggplot(USAccidents_preprocessed, aes(x = Severity, fill = Stop)) +
  geom_bar()

###Looking at the the number of accidents with each type of severity and looking at how many there was a traffic signal present
ggplot(USAccidents_preprocessed, aes(x = Severity, fill = Traffic_Signal)) +
  geom_bar()


##Information regarding the number of accidents of each Severity related with every weather condition

###Looking at the the number of accidents with each type of severity and looking at the weather condition at the moment of the accident
ggplot(USAccidents_preprocessed, aes(x = Severity, fill = Weather_Condition)) +
  geom_bar()


##Correlations for the bivariate analysis
correlate(USAccidents_preprocessed, Severity, method = "spearman") %>% 
  plot()


##Multivariate Outlier Detection####
outlier(quantiData)

mdi = mahalanobis(quantiData,center=colMeans(quantiData),cov=cov(quantiData))
cutoff <- qchisq(p = 0.99 , ncol(quantiData))
##Display observation whose distance greater than cutoff value
USAccidents_preprocessed[mdi>cutoff,]
nrow(USAccidents_preprocessed[mdi>cutoff,])










