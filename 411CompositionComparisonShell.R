####4.1.1 composition comparisons shell script####
#200036135
library(vegan)
library(ggplot2)
library(tidyverse)
library(patchwork)


####data read in####
setwd("")
#domin data should look like this 
#site	sp1
#1	    0
#1      2
#1      3
#1      1
#2      7
#2      5
#2      3
#2      5
domindata <- read.csv("yourdataset.csv")

####seperate by site####
site1 <- domindata[domindata$site == 1, -1]  # Remove site column
site2 <- domindata[domindata$site == 2, -1]  # Remove quadrat column

####Calculate Diversity Indices for Each Row####
diversity_indices <- domindata %>%
  select(-site) %>%
  group_by(domindata$site) %>%
  summarise(
    Shannon = diversity(., index = "shannon"), #shannon calclates spec diversity in a community
    Simpson = diversity(., index = "simpson"), #also another metric for diversity 
    Evenness = Shannon/log(specnumber(.)) #similarity of abundances of each species in an environment
  )

####BOXPLOTS FOR INDICES####
#shannon
Shannon <- ggplot(diversity_indices, aes(x = as.factor(`domindata$site`), y = Shannon, fill = as.factor(`domindata$site`))) + #fill in your datasets here
  geom_boxplot() +#replace y value as necessary to do the other 2 indices
  scale_fill_manual(values = c("darkgoldenrod1", "deepskyblue1")) + #S1 = unshaded (yellow=sun) , #S2 = shaded (blue = not sun)
  labs(x = "Site", y = "Shannon Index", fill = "Site", title = "a. Shannon Index") +
  theme_minimal()


####Rank Abundance Plot####
#Function to Calculate Rank Abundance 
rank_abundance <- function(site_data) {
  abundances <- colSums(site_data)
  sorted_abundances <- sort(abundances[abundances > 0], decreasing = TRUE)
  data.frame(
    rank = 1:length(sorted_abundances),
    abundance = sorted_abundances,
    species = names(sorted_abundances)
  )
}

#Calculate RA using function for each site
site1_ra <- rank_abundance(site1)
site2_ra <- rank_abundance(site2)

#Add site information
site1_ra$site <- "Site 1"
site2_ra$site <- "Site 2"

#Combine data
ra_combined <- rbind(site1_ra, site2_ra)

#Plot out RA for each site on the same graph
RankAbund <- ggplot(ra_combined, aes(x = rank, y = abundance, color = site)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("darkgoldenrod1", "deepskyblue1")) +
  labs(x = "Species Rank", y = "Abundance", title = "d. Rank/Abundance") +
  theme_minimal()

####BETA DIVERSITY OF MEADOW (BRAY CURTIS)####
#BC = Most sensitive metric - might exaggerate results, p value hack it

#Combine Sites
sites_combined <- rbind(
  colMeans(site1),
  colMeans(site2)
)

#Beta diversity
beta_div <- vegdist(domindata, method = "bray")

#Beta diversity value
print(beta_div)

#NMDS for visualization
nmds <- metaMDS(domindata, distance = "bray")

#NMDS plot
nmds_points <- data.frame(
  NMDS1 = nmds$points[,1],
  NMDS2 = nmds$points[,2],
  Site = c("Site 1", "Site 2")
)

#BetaDiv plot using NMDS
BetaDiv <- ggplot(nmds_points, aes(x = NMDS1, y = NMDS2, color = Site)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("darkgoldenrod1", "deepskyblue1")) +
  stat_ellipse(type = "t", level = 0.95, linewidth = 1.2) +  # Add 95% confidence ellipses
  theme_minimal() +
  labs(title = "e. NMDS Plot of Beta Diversity")


####Formatting Graphs####
#custom theme (using times font to match with the essay)
custom_theme <- theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(family = "Times New Roman", size = 12),
    axis.title = element_text(family = "Times New Roman", size = 15),
    plot.title = element_text(family = "Times New Roman", size = 20),
    legend.text = element_text(family = "Times New Roman", size = 15)
  )

#Custom theme for individual plots
Shannon <- Shannon + custom_theme
Smpson <- Smpson + custom_theme
Evenness <- Evenness + custom_theme
RankAbund <- RankAbund + custom_theme
BetaDiv <- BetaDiv + custom_theme

#Combined plot with all themed graphs
combined_plots <- (Shannon + Smpson + Evenness) / (RankAbund + BetaDiv)

#Combined plot with final theming
combined_plots + 
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "Meadow Diversity Analysis",
    theme = custom_theme
  )

# Perform PERMANOVA test
permanova_test <- adonis2(meadow_data ~ site, data = meadow_data, method = "bray")

# Print PERMANOVA results
print(permanova_test)


#Save as PNG
ggsave("__ADD INFO__diversity_analysis.png", 
       combined_plots, 
       width = 12, 
       height = 8, 
       dpi = 300, 
       units = "in")





