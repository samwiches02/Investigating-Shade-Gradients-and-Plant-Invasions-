####4.1.3 Correlation Analaysis Shell####
setwd("")

library(corrplot)


#Read Data
#summary dataset format
#species	site	la.avg	ldmc.avg	sla.avg	percentcover.avg
#Agro_capi	1	169.5765714	30.2	19.1	15.6
#Agro_capi	2	110.2742857	34.72857143	9.471428571	2

data <- read.csv("SUMMARYDATASET.csv")

# Remove rows with -9999 values
data_clean <- data[!apply(data[,c("la.avg", "ldmc.avg", "sla.avg", "percentcover.avg")], 1, function(x) any(x == -9999)),]

# Create separate dataframes for shaded (2) and unshaded (1) sites
unshaded <- data_clean[data_clean$site == 1, c("la.avg", "ldmc.avg", "sla.avg", "percentcover.avg")]
shaded <- data_clean[data_clean$site == 2, c("la.avg", "ldmc.avg", "sla.avg", "percentcover.avg")]

# Calculate correlation matrices with spearman
cor_unshaded <- cor(unshaded, method = "spearman") #site 1
cor_shaded <- cor(shaded, method = "spearman") #site 2

# Adjust plot margins & move title down (for some reason base par shot the titles out of the plot graphic)
par(mar = c(5, 4, 6, 2) + 0.1)  # Bottom, left, top, right margins
par(family = "Times New Roman") #force font to be times

#Create correlation plots with adjusted margins
#& also Modify correlation plot appearance for better readability
corrplot(cor_unshaded, method = "color", 
         type = "upper", 
         title = "Unshaded (Site 1) Correlations",
         addCoef.col = "black", 
         tl.col = "black",
         tl.family = "Times New Roman",
         cl.family = "Times New Roman",
         mar = c(0,0,4,0),
         col = colorRampPalette(c("cornsilk", "darkgoldenrod1"))(100),  # Light to dark blue gradient
         number.cex = 0.7,  # Adjust coefficient text size
         tl.cex = 1)     # Adjust label text size

corrplot(cor_shaded, method = "color", 
         type = "upper", 
         title = "Shaded (Site 2) Correlations",
         addCoef.col = "black", 
         tl.col = "black",
         tl.family = "Times New Roman",
         cl.family = "Times New Roman",
         mar = c(0,0,4,0),
         col = colorRampPalette(c("azure2", "deepskyblue"))(100),  # Light to dark blue gradient
         number.cex = 0.7,  # Adjust coefficient text size
         tl.cex = 1)     # Adjust label text size











