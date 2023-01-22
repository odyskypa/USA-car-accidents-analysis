# Loading Libraries
library("FactoMineR")
library("factoextra")
library("arules")
library("RColorBrewer")
library("arulesViz")
library("colorspace")


######### PLEASE CHANGE this directories when running the code ######### 
working_dir = "C:/Users/odyky/Desktop/MVA-Project/USAccidents"
plot_dir = "C:/Users/odyky/Desktop/MVA-Project/Plots/AssociationRules/"
dir.create(plot_dir, showWarnings = FALSE) # Creating the folder for saving the plots of Association Rules
setwd(working_dir)

######### Reading data from the csv generated after preprocessing and univariate-bivariate analysis
USAccidents <- read.csv("../USAccidents_final_dataset.csv", stringsAsFactors = TRUE)
summary(USAccidents)

######### Creating factors for specific variables ("Year", "Severity", "Month)
# Year
USAccidents$Year <- as.factor(USAccidents$Year)

# Severity
# Severity vector
severity_abb <- c("Severity1","Severity2","Severity3", "Severity4")
# Add severity name to data set
USAccidents$Severity <- severity_abb[ USAccidents$Severity ]
USAccidents$Severity <- as.factor(USAccidents$Severity)

# Month
# Months abbreviation vector assuming 1st month is Jan.
months_abb <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
# Add abbreviated month name to data set

USAccidents$Month <- months_abb[ USAccidents$Month ]
USAccidents$Month <- as.factor(USAccidents$Month)


######### Discarding columns "ID", "Start_Time" and "End_Time" for the assocation rules analysis.
df_arules <- USAccidents[,!names(USAccidents) %in% c("ID", "Start_Time", "End_Time")]

######### Finding the quantitative variables of the data set
sapply(df_arules,class) # Checking the class of every variable of the data set
numeriques <- which(sapply(df_arules,is.numeric)) # Creating a vector containing the column index of the data set's numeric variables
numeriques # 6 numerical variables for discretization before applying association rules algorithms

# Picking only the numerical variables in order to discretize them into categorical ones
df_discret <- df_arules[,numeriques]


######### Discretization of numerical variables

######### Visibility Discretization
######### From the National Weather Service (https://www.weather.gov/safety/fog-boating):
######### Visibility is generally described as Very Poor, Poor, Moderate, Good
######### Very Poor: Less than 0.5 nautical miles (inf, 0.57539)
######### Poor: 0.5 to less than 2 nautical miles [0.57539,2.30156)
######### Moderate: 2 to 5 nautical miles [2.30156, 5.7539]
######### Good: Greater than 5 nautical miles [5.7539, inf)

### user-specified (with labels)
table(discretize(df_discret$Visibility.mi, method = "fixed", breaks = c(0, 0.57539, 2.30156, 5.7539, 140), 
                 labels = c("Very Poor", "Poor", "Moderate", "Good")))
pdf(paste(plot_dir, "visibility_hist_disc", ".pdf", sep=""))
hist(df_discret$Visibility.mi, breaks = 20, main = "Fixed")
abline(v = discretize(df_discret$Visibility.mi, method = "fixed", breaks = c(0, 0.57539, 2.30156, 5.7539, 140), 
                      onlycuts = TRUE), col = "red")
dev.off()
df_discret$Visibility.mi.<- ordered(cut(df_discret$Visibility.mi., c(0, 0.57539, 2.30156, 5.7539, 140)),
                             labels = c("Very Poor", "Poor", "Moderate", "Good"))



######### Precipitation Discretization
######### From https://www.baranidesign.com/:
######### https://www.baranidesign.com/faq-articles/2020/1/19/rain-rate-intensity-classification
######### Rainfall rate is generally described as none, light, moderate or heavy.
######### No rain - 0 mm/h (0"/hr)
######### Light rain - less than 2.5 mm/h (<0.1”/hr)
######### Moderate rain - rain rate of fall is 2.6 to 7.5 mm/h (0.1 to 0.3”/hr)
######### Heavy rain - rain rate is greater than 7.6 to 50 mm/h (0.3 to 2”/hr)

### user-specified (with labels)
table(discretize(df_discret$Precipitation.in., method = "fixed", breaks = c(-1, 0.01, 0.1, 0.3, 24), 
                 labels = c("None","Light", "Moderate", "Heavy")))
pdf(paste(plot_dir, "precipitation_hist_disc", ".pdf", sep=""))
hist(df_discret$Precipitation.in., breaks = 20, main = "Fixed")
abline(v = discretize(df_discret$Precipitation.in., method = "fixed", breaks = c(-1, 0.01, 0.1, 0.3, 24), 
                      onlycuts = TRUE), col = "red")
dev.off()
df_discret$Precipitation.in.<- ordered(cut(df_discret$Precipitation.in., breaks = c(-1, 0.01, 0.1, 0.3, 24)),
                                      labels = c("None", "Light", "Moderate", "Heavy"))


######### Humidity Discretization
######### From the National Weather Service (https://www.weather.gov/arx/why_dewpoint_vs_humidity):
######### Humidity is generally described as dry and comfortable(low), "sticky" with muggy evenings(medium) or oppressive(high).
######### less than or equal to 55: dry and comfortable -> low
######### between 55 and 65: becoming "sticky" with muggy evenings -> medium
######### greater than or equal to 65: lots of moisture in the air, becoming oppressive -> high

### user-specified (with labels)
table(discretize(df_discret$Humidity..., method = "fixed", breaks = c(-1, 55.01, 65.01, 101), 
                 labels = c("Low","Medium", "High")))
pdf(paste(plot_dir, "humidity_hist_disc", ".pdf", sep=""))
hist(df_discret$Humidity..., breaks = 20, main = "Fixed")
abline(v = discretize(df_discret$Humidity..., method = "fixed", breaks = c(-1, 55.01, 65.01, 101), 
                      onlycuts = TRUE), col = "red")
df_discret$Humidity...<- ordered(cut(df_discret$Humidity..., breaks = c(-1, 55.01, 65.01, 101)),
                                         labels = c("Low","Medium", "High"))
dev.off()

######### Temperature Discretization
######### From https://thinkmetric.uk/basics/temperature/:
######### Temperature is generally described as very cold, cold, cool, warm, warm to hot, hot, very hot.

### user-specified (with labels)
table(discretize(df_discret$Temperature.F., method = "fixed", breaks = c(-19, 32.01, 50.01, 59.01, 68.01, 77.01, 86.01, 120), 
                 labels = c("Very Cold","Cold", "Cool", "Warm", "Warm to Hot", "Hot", "Very Hot")))
pdf(paste(plot_dir, "temperature_hist_disc", ".pdf", sep=""))
hist(df_discret$Temperature.F., breaks = 20, main = "Fixed")
abline(v = discretize(df_discret$Temperature.F., method = "fixed", breaks = c(-19, 32.01, 50.01, 59.01, 68.01, 77.01, 86.01, 120), 
                      onlycuts = TRUE), col = "red")
df_discret$Temperature.F.<- ordered(cut(df_discret$Temperature.F., breaks = c(-19, 32.01, 50.01, 59.01, 68.01, 77.01, 86.01, 120)),
                                   labels = c("Very Cold","Cold", "Cool", "Warm", "Warm to Hot", "Hot", "Very Hot"))
dev.off()

######### Pressure Discretization
######### From https://www.thoughtco.com/how-to-read-a-barometer-3444043:
######### Temperature is generally described as Low, Normal, High.

### user-specified (with labels)
table(discretize(df_discret$Pressure.in., method = "fixed", breaks = c(0, 29.81, 30.21, 59), 
                 labels = c("Low","Normal", "High")))
pdf(paste(plot_dir, "pressure_hist_disc", ".pdf", sep=""))
hist(df_discret$Pressure.in., breaks = 20, main = "Fixed")
abline(v = discretize(df_discret$Pressure.in., method = "fixed", breaks = c(0, 29.81, 30.21, 59), 
                      onlycuts = TRUE), col = "red")
dev.off()
df_discret$Pressure.in.<- ordered(cut(df_discret$Pressure.in., breaks = c(0, 29.81, 30.21, 59)),
                                      labels = c("Low","Normal", "High"))

######### Distance Discretization
######### Distance (mi.) will be described as Small, Medium, High.

### user-specified (with labels)
table(discretize(df_discret$Distance.mi., method = "cluster", breaks = 3, 
                 labels = c("Small","Medium", "High")))
pdf(paste(plot_dir, "distance_hist_disc", ".pdf", sep=""))
hist(df_discret$Distance.mi., breaks = 20, main = "k-Means")
abline(v = discretize(df_discret$Distance.mi., method = "cluster", breaks = 3, 
                      onlycuts = TRUE), col = "red")
df_discret$Distance.mi.<- discretize(df_discret$Distance.mi., method = "cluster", breaks = 3, 
                                       labels = c("Small","Medium", "High"))
dev.off()


######### Combining categorical variables with the df_discret after discretization.
######### From the location variables only County is chosen, so the analysis is gonna
######### be county-wise and season-wise concerning place and time dimensions of data.
df_arules <- cbind(df_arules[,
                      c("Weather_Condition", "Year", "County", "Crossing", "Bump", "Stop", "Traffic_Signal", "Severity", "Season")
                      ],df_discret)

######### Association Rules

transactionsTotal <- transactions(df_arules)
summary(transactionsTotal)

######## Item Frequency Histogram tells how many times an item has occurred
######## in our data set as compared to the others.

######## Frequency Histogram for ALL cases.
pdf(paste(plot_dir, "item_frequency_plot_minsupport_0.1_ALL", ".pdf", sep=""))
itemFrequencyPlot(transactionsTotal,
                          support=0.1,
                          cex.names=0.8,
                          col=brewer.pal(8,'Pastel2'),
                          main='Relative Item Frequency Plot',
                          type="relative",
                          ylab="Item Frequency (Relative)")
dev.off()

pdf(paste(plot_dir, "item_frequency_plot_top20_ALL", ".pdf", sep=""))
itemFrequencyPlot(transactionsTotal,
                  topN=20,
                  cex.names=0.8,
                  col=brewer.pal(8,'Pastel2'),
                  main='Relative Item Frequency Plot',
                  type="relative",
                  ylab="Item Frequency (Relative)")
dev.off()

######### Rules
######### Frequent Itemsets with ECLAT
Totalfreq.itemsets <- eclat(df_arules, parameter=list(supp=0.1, minlen =3))
# Inspecting top 30 frequent itemsets based on support value.
inspect(sort(Totalfreq.itemsets, by ="support")[1:30])
write(Totalfreq.itemsets, file = "../total_frequent_itemsets.csv", sep = ",", col.names = NA)

######### Rules Generation with APRIORI
Totalrules <- apriori(df_arules, parameter = list(support = 0.1, confidence = 0.25, minlen = 3))
summary(Totalrules)
write(Totalrules, file = "../total_rules.csv", sep = ",", col.names = NA)

######### Creating 4 subset of rules, taking into account for the rhs
######### the 4 different levels of severity

######### Subset for Severity1 in rhs is empty
severity1_subset <- subset(Totalrules, subset = rhs %in% "Severity=Severity1")
sorted_rules_severity1 <- sort(severity1_subset, by ="lift")
inspect(sorted_rules_severity1)

######### Subset for Severity2
severity2_subset <- subset(Totalrules, subset = rhs %in% "Severity=Severity2")
sorted_rules_severity2 <- sort(severity2_subset, by ="lift")
write(sorted_rules_severity2, file = "../total_rules_severity2.csv", sep = ",", col.names = NA)
inspect(sorted_rules_severity2[1:10])

######### Subset for Severity3 in rhs is empty
severity3_subset <- subset(Totalrules, subset = rhs %in% "Severity=Severity3")
sorted_rules_severity3 <- sort(severity3_subset, by ="lift")
inspect(sorted_rules_severity3)

######### Subset for Severity4 in rhs is empty
severity4_subset <- subset(Totalrules, subset = rhs %in% "Severity=Severity4")
sorted_rules_severity4 <- sort(severity4_subset, by ="lift")
inspect(sorted_rules_severity4)

######### Inspecting top 20 rules sorted by decreasing confidence Total
top20rules <- head(sort(Totalrules, by ="lift"),20)
inspect((sort(top20rules,by ="confidence")))
write(top20rules, file = "../top20_rules_sortedby_lift_and_confidence.csv", sep = ",", col.names = NA)

pdf(paste(plot_dir, "total_rules_confidence_support", ".pdf", sep=""))
plot(Totalrules)
dev.off()

pdf(paste(plot_dir, "total_rules_support_lift", ".pdf", sep=""))
plot(Totalrules, measure = c("support", "lift"), shading = "confidence")
dev.off()

pdf(paste(plot_dir, "top20_rules_by_lift_graph", ".pdf", sep=""))
plot(top20rules, method ="graph")
dev.off()

pdf(paste(plot_dir, "top20_rules_by_lift_paracoord", ".pdf", sep=""))
plot(top20rules, method = "paracoord", control = list(reorder = TRUE))
dev.off()

pdf(paste(plot_dir, "top20_rules_by_lift_matrix", ".pdf", sep=""))
plot(top20rules, method="matrix", measure=c("support","confidence"), control=list(reorder="measure", col=sequential_hcl(200)))
dev.off()

######### Inspecting top 20 rules containing Severity2 in RHS
top20_severity2_rules <- sorted_rules_severity2[1:20]
top20_severity2_rules_sorted_by_confidence <- (sort(top20_severity2_rules,by ="confidence"))

pdf(paste(plot_dir, "severity2_rules_confidence_support", ".pdf", sep=""))
plot(top20_severity2_rules_sorted_by_confidence)
dev.off()

pdf(paste(plot_dir, "severity2_rules_support_lift", ".pdf", sep=""))
plot(top20_severity2_rules_sorted_by_confidence, measure = c("support", "lift"), shading = "confidence")
dev.off()

pdf(paste(plot_dir, "severity2_rules_by_lift_graph", ".pdf", sep=""))
plot(top20_severity2_rules_sorted_by_confidence, method ="graph")
dev.off()

pdf(paste(plot_dir, "severity2_rules_by_lift_paracoord", ".pdf", sep=""))
plot(top20_severity2_rules_sorted_by_confidence, method = "paracoord", control = list(reorder = TRUE))
dev.off()

pdf(paste(plot_dir, "severity2_rules_by_lift_matrix", ".pdf", sep=""))
plot(top20_severity2_rules_sorted_by_confidence, method="matrix", measure=c("support","confidence"), control=list(reorder="measure", col=sequential_hcl(200)))
dev.off()

