library(FactoMineR)
library(dplyr)
library(factoextra)
library(ggplot2)

setwd("/Users/Gerard/Desktop/MVA/Project/")
USAccidents_imputed <- read.csv("USAccidents_final_dataset.csv")

USAccidents_imputed$Weather_Condition <- as.factor(USAccidents_imputed$Weather_Condition)
levels(USAccidents_imputed$Weather_Condition)

colnames(USAccidents_imputed)

geographic <- USAccidents_imputed[,2]
time_df <- USAccidents_imputed[,21]
weather_condition <- USAccidents_imputed[,4]
atmospheric_condition <- USAccidents_imputed[,c(12:15,19)]
distance <- USAccidents_imputed[,18]
scenario <- USAccidents_imputed[,7:10]
severity <- USAccidents_imputed[,11]

DFtoMFA <- cbind(geographic, time_df, weather_condition, atmospheric_condition, distance, scenario, severity)

set.seed(123)
stratified <- as.data.frame(DFtoMFA %>% group_by(geographic) %>% slice_sample(prop = 0.5))
stratified$geographic <- as.factor(stratified$geographic)
stratified$time_df <- as.factor(stratified$time_df)
stratified$weather_condition <- as.factor(stratified$weather_condition)

res.mfa <- MFA(stratified[,-14], 
               group = c(1, 1, 1, 5, 1, 4), 
               type = c("n", "n", "n", "s", "s", "n"),
               name.group = c("geographic","time_df","weather_condition",
                              "atmospheric_condition", "distance","scenario"),
               graph = FALSE)

ind <- get_mfa_ind(res.mfa)
eig.val <- get_eigenvalue(res.mfa)
head(eig.val)

set.seed(123)
sampel_indiv_ploting <- sample(seq(from = 1, to = nrow(stratified), by = 1),size = nrow(stratified) * 0.01) 
  
indiv_sample <- res.mfa$ind$coord[sampel_indiv_ploting,c(1,2)]
indiv_sample <- as.data.frame(indiv_sample)
severity_sample <- as.factor(stratified[sampel_indiv_ploting,14])
  
ggplot(indiv_sample, aes(Dim.1,Dim.2,color = severity_sample)) + 
  geom_point() +
  theme_minimal() + 
  theme(axis.line = element_line(),
        panel.grid = element_blank()) +
  scale_color_discrete(name = "Severity") +
  labs(x = "Dim 1 (7.64%)", y = "Dim 2 (5.35%)")
#We can see that getting the three first dimension we can explain 82 percent of the data:

fviz_screeplot(res.mfa)
fviz_mfa_var(res.mfa, "group",axes = c(1,2))

fviz_contrib(res.mfa, "group", axes = 1)
fviz_contrib(res.mfa, "group", axes = 2)
fviz_contrib(res.mfa, "group", axes = 3)
fviz_contrib(res.mfa, "group", axes = 4)
fviz_contrib(res.mfa, "group", axes = 5)

fviz_mfa_var(res.mfa, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE,  axes = c(1,2))
fviz_mfa_var(res.mfa, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE,  axes = c(1,3))
fviz_mfa_var(res.mfa, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE,  axes = c(2,3))

fviz_mfa_var(res.mfa, "quali.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE, axes = c(1,2)) 
fviz_mfa_var(res.mfa, "quali.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE, axes = c(1,3))
fviz_mfa_var(res.mfa, "quali.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE, axes = c(2,3))

summary(DFtoMFA)
