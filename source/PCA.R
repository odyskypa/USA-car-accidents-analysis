# Loading Libraries
library(naniar)
library(stringi)
library(class)
library("FactoMineR")
library("factoextra")

################### PLEASE CHANGE this directories when running the code ###################
working_dir = "/Users/anderbarriocampos/Desktop"
plot_dir = "C:/Users/odyky/Desktop/MVA-Project/Plots/"
setwd(working_dir)

# Reading data from the csv generated after preprocessing and univariate-bivariate analysis
USAccidents <- read.csv("USAccidents_final_dataset.csv", stringsAsFactors = TRUE)
summary(USAccidents)

# Converting Year, Severity and Month variables into factors
USAccidents$Year <- as.factor(USAccidents$Year)
USAccidents$Severity <- as.factor(USAccidents$Severity)
USAccidents$Month <- as.factor(USAccidents$Month)


sapply(USAccidents,class) # Checking the class of every variable of the dataset
numeriques <- which(sapply(USAccidents,is.numeric)) # Creating a vector containing the column index of the dataset's numeric variables
numeriques # 6 numerical variables for performing PCA


# Applying PCA to the numeric variables of the data set
# Using the qualitative variables as supplementary information for the generation of graphs

df_num = USAccidents[,numeriques] # Taking the numerical values of the data set.
res.pca <- PCA(df_num, scale.unit = TRUE, ncp = 6, graph = FALSE)
summary(res.pca)

# Creating a folder for saving the plots
dir.create(plot_dir, showWarnings = FALSE)

# Scree plot for the percentage of explained variance for the Principal Components.
pdf(paste(plot_dir, "percentage_of_explained_variance", ".pdf", sep=""))
fviz_eig(res.pca, main = "Variance Scree Plot", addlabels = TRUE, ylim = c(0, 50))
dev.off()

# Barplot depicting the cumulative sum of variance for the Principal Components.
pdf(paste(plot_dir, "cumulative_sum_of_variance", ".pdf", sep=""))
eig.val <-get_eigenvalue(res.pca)
eig.val
cum_var <- eig.val[,3]
bp<-barplot(cum_var, xlab = "Principal Components", ylab = "Percentage of Variance", main = "Cumulative Sum of Variance")
text(bp, 0, round(cum_var, 1),cex=1,pos=3)
dev.off()


############Dimension description
##### The dimdesc() function calculates the correlation coefficient between
##### a variable and a dimension and performs a significance test.
res.desc <- dimdesc(res.pca, axes = c(1,2,3), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1
# Description of dimension 2
res.desc$Dim.2
# Description of dimension 3
res.desc$Dim.3


#############Variables simple individual projection
##### Axes 1 and 2
pdf(paste(plot_dir, "simple_ind_proj_1_2", ".pdf", sep=""))
fviz_pca_ind(res.pca, label="none", axes = c(1,2))
dev.off()
##### Axes 1 and 3
pdf(paste(plot_dir, "simple_ind_proj_1_3", ".pdf", sep=""))
fviz_pca_ind(res.pca, label="none", axes = c(1,3))
dev.off()
##### Axes 2 and 3
pdf(paste(plot_dir, "simple_ind_proj_2_3", ".pdf", sep=""))
fviz_pca_ind(res.pca, label="none", axes = c(2,3))
dev.off()


#############Variables factor graph
##### Axes 1 and 2
pdf(paste(plot_dir, "variables_factor_map_1_2", ".pdf", sep=""))
fviz_pca_var(res.pca, col.var = "contrib", repel = TRUE, axes=c(1,2),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
)
dev.off()

##### Axes 1 and 3
pdf(paste(plot_dir, "variables_factor_map_1_3", ".pdf", sep=""))
fviz_pca_var(res.pca, col.var = "contrib", repel = TRUE, axes=c(1,3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
dev.off()

pdf(paste(plot_dir, "variables_factor_map_2_3", ".pdf", sep=""))
fviz_pca_var(res.pca, col.var = "contrib", repel = TRUE, axes=c(2,3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
dev.off()


#############Individuals projections with coloring based on Severity
#############Taking a sample of 100000 units due to large number of observations
##### Axes 1 and 2
pdf(paste(plot_dir, "individuals_projection_axes_1_2", ".pdf", sep=""))
fviz_pca_ind(res.pca, col.ind = USAccidents$Severity, pointsize = 0.5, 
             select.ind = list(contrib = 100000), label="none", axes = c(1,2),
             legend.title = "Severity Levels")
dev.off()

##### Axes 1 and 3
pdf(paste(plot_dir, "individuals_projection_axes_1_3", ".pdf", sep=""))
fviz_pca_ind(res.pca, col.ind = USAccidents$Severity, palette = c("#00AFBB", "#E7B800", "#FC4E07", "#753236"),
             geom.ind = "point", pointsize = 1.5, label="none", axes = c(1,3),
             select.ind = list(contrib = 100000),
             legend.title = "Severity Levels")
dev.off()

##### Axes 2 and 3
pdf(paste(plot_dir, "individuals_projection_axes_2_3", ".pdf", sep=""))
fviz_pca_ind(res.pca, col.ind = USAccidents$Severity, palette = c("#00AFBB", "#E7B800", "#FC4E07", "#753236"),
             geom.ind = "point", pointsize = 1.5, label="none", axes = c(2,3),
             select.ind = list(contrib = 100000),
             legend.title = "Severity Levels")
dev.off()

#############Biplots Containing Individuals and Variable Factor Maps
#############colored and grouped based on Severity levels
##### Axes 1 and 2
pdf(paste(plot_dir, "biplot_axes_1_2", ".pdf", sep=""))
fviz_pca_biplot(res.pca, select.ind = list(contrib = 100000), axes=c(1,2),
                habillage = USAccidents$Severity, addEllipses = TRUE, elipse.level = 0.2,
                label = "var") +
  scale_color_brewer(palette="Dark2")+
  theme_minimal()
dev.off()

##### Axes 1 and 3
pdf(paste(plot_dir, "biplot_axes_1_3", ".pdf", sep=""))
fviz_pca_biplot(res.pca, select.ind = list(contrib = 100000), axes=c(1,3),
                habillage = USAccidents$Severity, addEllipses = TRUE, elipse.level = 0.2,
                label = "var") +
  scale_color_brewer(palette="Dark2")+
  theme_minimal()
dev.off()

##### Axes 2 and 3
pdf(paste(plot_dir, "biplot_axes_axes_2_3", ".pdf", sep=""))
fviz_pca_biplot(res.pca, select.ind = list(contrib = 100000), axes=c(2,3),
                habillage = USAccidents$Severity, addEllipses = TRUE, elipse.level = 0.2,
                label = "var") +
  scale_color_brewer(palette="Dark2")+
  theme_minimal()
dev.off()


###NOTES:
#    Positively correlated variables are grouped together.
#    Negatively correlated variables are positioned on opposite sides of the plot origin (opposed quadrants).
#    The distance between variables and the origin measures the quality of the variables on the factor map. 
#    Variables that are away from the origin are well represented on the factor map.
