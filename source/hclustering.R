# Loading Libraries
library(StatMatch)
library(cluster)
library(NbClust)
library(vegan)
library(supc)
library(caret)
library(factoextra)


# Setting Working Environment 
# Clear plots
if(!is.null(dev.list())) dev.off()

# Clean workspace
rm(list=ls())

#setwd("/Users/ander.barrio/Desktop/MVA-Project")
setwd("C:/Users/odyky/Desktop/UPC/1st Semester/MVA-MDS/Group project/MVA-Project-Code")

# Reading Data and Summary
dd <- read.csv("USAccidents_final_dataset.csv", sep=",",dec=".",stringsAsFactors = TRUE)
names(dd)
dim(dd)
summary(dd)

# Creating factors for specific variables ("Year", "Severity", "Month)
dd$Year <- as.factor(dd$Year)

# Severity
severity_abb <- c("Severity1","Severity2","Severity3", "Severity4")
dd$Severity <- severity_abb[dd$Severity]
dd$Severity <- as.factor(dd$Severity)

# Month
months_abb <- c("Jan","Feb","Mar",
                "Apr","May","Jun",
                "Jul","Aug","Sep",
                "Oct","Nov","Dec")
dd$Month <- months_abb[dd$Month]
dd$Month <- as.factor(dd$Month)

# Scalling Numerical Variables before Clustering
cols <- sapply(dd, is.numeric)
dd.scaled <-dd
process <- preProcess(dd.scaled[cols], method=c("range"))
dd.scaled[cols] <- predict(process, dd.scaled[cols])
summary(dd.scaled)

# Checking Associations and Variance of Severity (target variable), on different
# groups of categorical variables (State, Season, Year)
par(mfrow=c(1,1))
boxplot(Severity~State,data=dd.scaled, id=list(n=Inf,labels=row.names(dd.scaled)),
        main = "Association of Severity and State")
boxplot(Severity~Season,data=dd.scaled, id=list(n=Inf,labels=row.names(dd.scaled)),
        main = "Association of Severity and Season")
boxplot(Severity~Year,data=dd.scaled, id=list(n=Inf,labels=row.names(dd.scaled)),
        main = "Association of Severity and Year")


# Feature Selection before Clustering
id <- dd.scaled$ID
city <- dd.scaled$City
county <- dd.scaled$County
severity <- dd.scaled$Severity
start <- dd.scaled$Start_Time
end <- dd.scaled$End_Time
month <- dd.scaled$Month
state <- dd.scaled$State
year <- dd.scaled$Year

drop <- names(dd.scaled) %in% c("ID", "City", "County", "Start_Time", "End_Time", "Month") #we discard this variables
dd.scaled <- dd.scaled[,!drop]
dim(dd.scaled)


table(dd.scaled$Year)
table(dd.scaled$State)
table(dd.scaled$Severity)
# From the following table we can see that for California in 2020
# there are several accidents characterized by all different levels 
# of severity. For that reason our analysis for the rest of the project
# will only focus on Accidents that took place in California in 2020, and
# we will sample randomly 20k observations from all 4 different levels of
# Severity. (20k due to our RAM memory limitation for the calcuation of the
# distance matrices for the clustering.) 
table(dd.scaled$Severity,dd.scaled$Year,dd.scaled$State)

# Filter data frame based on State and Year, for dimensionality reduction
CA_2020 <- dd.scaled[dd.scaled$State == 'CA' & dd.scaled$Year == "2020",]
nrow(CA_2020)
severity_2_sample <-CA_2020[ sample( which( CA_2020$Severity == "Severity2" ) , 10000 ) , ]
other_severities <- CA_2020[CA_2020$Severity %in% c("Severity1","Severity3","Severity4"),]
filtered_data = rbind(severity_2_sample,other_severities)
nrow(filtered_data)

drop_final <- names(filtered_data) %in% c("State", "Year", "Severity") #we discard those variables
dd_final <- filtered_data[,!drop_final]

# HIERARCHICAL CLUSTERING
daisy <- daisy(dd_final, metric = "gower", type =list(asymm=c(2:5),
                              factor=c(1, 12),
                              numeric=c(6:11)))
daisy <- daisy^2

h1 <- hclust(daisy,method="ward.D") #ward
plot(h1)

###### Selection of K with quality clustering KPIs by using NbClust
indices <- c("cindex","silhouette","dunn", "frey", "mcclain")

particiones <- vector()
for(i in 1:length(indices)){
  print(indices[i])
  sel_k<-NbClust(data = NULL,diss=daisy,distance=NULL,min.nc=2,max.nc=6,method="ward.D2",index=indices[i])
  particiones[i]<-max(sel_k$Best.partition)
}

names(particiones) <- indices
particiones #we select k = 5


sil_width <- c(NA)
for(i in 2:10){
  
  pam_fit <- pam(daisy,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}
# Plot sihouette width (higher is better)
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

# Labelling of Clustering Results
# Decision on k=4 (just for final-user requirements)
plot(h1)
rect.hclust(h1, k=4, border=6)
k <- 4
cut_k <- cutree(h1,k)

###### Selection of K by using silhoutte
sil = silhouette(cut_k,daisy)
pdf('silhouttes.pdf')
plot(sil)
dev.off()

df <- cbind(dd_final, cluster = factor(unname(cut_k), labels = c("cluster-1", "cluster-2", "cluster-3", "cluster-4")))

ind <- as.numeric(rownames(df))


df$Severity <- severity[ind]
df$ID <- id[ind]
df$State <- state[ind]
df$City <- city[ind]
df$County <- county[ind]
df$Start_Time <- start[ind]
df$End_Time <- end[ind]
df$Month <- month[ind]
df$Year <- year[ind]

df$Severity <- as.factor(df$Severity)
df$State <- as.factor(df$State)
df$City <- as.factor(df$City)
df$County <- as.factor(df$County)
df$Start_Time <- as.factor(df$Start_Time)
df$End_Time <- as.factor(df$End_Time)
df$Month <- as.factor(df$Month)
df$Year <- as.factor(df$Year)


table(cut_k)

boxplot(Severity~cluster,data=df, id=list(n=Inf,labels=row.names(df)),
        main = "Association of Severity and Clusters")

write.csv(df, file = "./clustered_data_CA_2020.csv")

