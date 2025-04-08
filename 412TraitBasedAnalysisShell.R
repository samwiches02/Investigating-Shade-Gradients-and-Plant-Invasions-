####4.1.2 TRAIT BASED ANALYSIS#######
####Community Weighted Means####
#calculates CWM based on a summary dataset
#####Data Read in####
setwd("")
summary <- read.csv("yourdata.csv")

#summary dataset example structure:
#species	site	la.avg	ldmc.avg	sla.avg	percentcover.avg
#Agro_capi	1	169.5765714	30.2	19.1	15.6
#Agro_capi	2	110.2742857	34.72857143	9.471428571	2
#0/NA values in the excel shld be -9999

####required packages####
library(vegan)
library(dendextend)
library(labdsv)
library(tidyverse)
library(dplyr)
library(FD)
library(ggplot2)
library(factoextra)
library(tidyverse)
library(ggfortify)


####Calculating Cwm Per Site####

#Remove any -9999 values
summary_clean <- summary[summary$la.avg != -9999, ] #repeat as necessary for ldmc.avg, and sla.avg


#Convert site to factors
summary_clean$site <- as.factor(summary_clean$site)

#Create trait matrix (just the trait columns)
trait_matrix <- as.matrix(summary_clean[, c("la.avg", "ldmc.avg", "sla.avg")])

#Create abundance vector
abundance <- summary_clean$percentcover.avg

#Calculate CWM
cwm <- trait_matrix * abundance                        
  
#creating a summary table
cwm_summary <- aggregate(trait_matrix * abundance,
                         by = list(Site = m_summary_clean$site),
                         FUN = sum, na.rm = TRUE)

#Divide by total abundance per site to get weighted means
site_abundance <- aggregate(abundance,
                            by = list(Site = m_summary_clean$site),
                            FUN = sum, na.rm = TRUE)
cwm_final <- cwm_summary[, -1] / site_abundance$x
                              

#making a nice little table
cwm_table <- data.frame(
  Site = cwm_summary$Site,
  LA_CWM = cwm_final$la.avg,
  LDMC_CWM = cwm_final$ldmc.avg,
  SLA_CWM = cwm_final$sla.avg
)

print(cwm_table)

####CWM Per Species Per Site (Composite Graph)####
#1 Prep the datasets: 

# Calculate CWM grouped by species and site
cwm_by_species <- aggregate(trait_matrix * summary_clean$percentcover.avg,
                            by = list(
                              Species = summary_clean$species,
                              Site = summary_clean$site
                            ),
                            FUN = sum, na.rm = TRUE)


# Get abundance totals by species and site
abundance_by_species <- aggregate(summary_clean$percentcover.avg,
                                  by = list(
                                    Species = summary_clean$species,
                                    Site = summary_clean$site
                                  ),
                                  FUN = sum, na.rm = TRUE)                                
                                
#Calculate final CWM values
cwm_species_final <- data.frame(
  Species = cwm_by_species$Species,
  Site = cwm_by_species$Site,
  LA_CWM = cwm_by_species$la.avg / abundance_by_species$x,
  LDMC_CWM = cwm_by_species$ldmc.avg / abundance_by_species$x,
  SLA_CWM = cwm_by_species$sla.avg / abundance_by_species$x
)

print(cwm_species_final)


####Graphing CWM Comparisons####
#prepare data
cwm_plot_data <- cwm_species_final%>%
  gather(key = "Trait", value = "Value", LA_CWM:SLA_CWM)


#add custom theme (new times roman again to match with paper font)
custom_theme <- theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(family = "Times New Roman", size = 12),
    axis.title = element_text(family = "Times New Roman", size = 15),
    plot.title = element_text(family = "Times New Roman", size = 20),
    legend.text = element_text(family = "Times New Roman", size = 15)
  )

#Make the Plot Using ggplot 
cwm_grid_plot <- cwm_species_final %>%
  pivot_longer(cols = c(LA_CWM, LDMC_CWM, SLA_CWM),
               names_to = "Trait",
               values_to = "Value")


meadowspeciestraits <- ggplot(cwm_grid_plot, aes(x = Site, y = Value, color = Site)) +
  geom_point(size = 3) +
  geom_line(aes(group = Site)) +
  facet_grid(Trait ~ Species, scales = "free_y") + 
  scale_color_manual(values = c("1" = "darkgoldenrod1", "2" = "deepskyblue1")) +
  theme_bw() +
  labs(
    x = "Site",
    y = "Trait Value",
    title = "_____ADD INFO____ Species Traits"
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(family = "Times New Roman", size = 12),
    axis.title = element_text(family = "Times New Roman", size = 15),
    plot.title = element_text(family = "Times New Roman", size = 20),
    legend.text = element_text(family = "Times New Roman", size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = 90, family = "Times New Roman"),  # Horizontal trait labels
    strip.text.x = element_text(angle = 90, family = "Times New Roman"),  # Vertical species labels
    strip.background = element_rect(fill = "grey90"),  # Gray background for labels
    panel.spacing = unit(0.5, "cm"),  # More space between panels
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Panel borders
    strip.placement = "outside"  # Place strip labels outside the panels
  )

#Save as PNG
ggsave("____ADDINFO____SpeciesTraits.png", 
       meadowspeciestraits, 
       width = 12, 
       height = 8, 
       dpi = 300, 
       units = "in")


#####Functional Diversity Indices####
#The data formatting here is essential 
#For the species traits, the species should be rows, and the traits in columns (mean values)
#For the Abundance Matrix, the species should be in columns and the site abundance (absent presence, or percent, or domin, do whatever) should be in the rows

#Species_traits format: 
#species	la	ldmc	sla
#Agro_capi	139.9254286	32.46428572	14.28571429
#Bell_pere	185.8408334	23.975	28.45833333

#Abundance_Matrix Format
#species	Agro_capi	Bell_pere
#site1	15.6	2
#site2	2	3

#Read the Data
species_traits <- read.csv("___SPECIES_TRAITS___.csv")
abundance_matrix <- read.csv("____ABUNDANCE_MATRIX____.csv")


#Prepare the trait matrix
trait_matrix <- species_traits[, c("la", "ldmc", "sla")]
rownames(trait_matrix) <- species_traits$species

#Prepare the abundance matrix
abund_matrix <- abundance_matrix[, -1]  # Remove species column
rownames(abund_matrix) <- abundance_matrix$species

#Calculate FD indices using the 'FD' Package
fd_indices <- dbFD(
  trait_matrix,           # Trait matrix
  abund_matrix,          # Abundance matrix
  w.abun = TRUE,         # Weight by abundance
  stand.x = TRUE,        # Standardize traits
  corr = "cailliez"      # Correction method for distance matrix
)

#View results
print("Functional Richness (FRic):")
print(fd_indices$FRic) #amount of Niche space occupied with species within a community

print("Functional Evenness (FEve):")
print(fd_indices$FEve) #unifromity of abundance of trait values in trait space 

print("Functional Divergence (FDiv):")
print(fd_indices$FDiv) #measuring variety of roles species play in env based on traits


#Create a summary dataframe
fd_summary <- data.frame(
  Site = c("site1", "site2"),
  FRic = fd_indices$FRic,
  FEve = fd_indices$FEve,
  FDiv = fd_indices$FDiv
)
print(fd_summary)


#Convert FD summary to long format for plotting (Pivot)
fd_long <- fd_summary %>%
  pivot_longer(cols = c(FRic, FEve, FDiv),
               names_to = "Index",
               values_to = "Value")



#Create simple plots with custom colors for both lines and points, differentiating sites and emphasizing their difference with a dashed line
ggplot(fd_long, aes(x = Site, y = Value, group = Index)) +
  geom_line(linetype = "dashed", color = "black") +
  geom_point(aes(fill = Site), size = 4, shape = 21, stroke = 1) +
  facet_wrap(~Index, scales = "free_y") +
  theme_bw() +
  labs(title = "Functional Diversity Indices Comparison",
       y = "Index Value") +
  scale_fill_manual(values = c("site1" = "darkgoldenrod1", 
                               "site2" = "deepskyblue1")) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 20),
    legend.text = element_text(size = 15)
  )

#####Principal Component Analysis###
#identifies principal compoents 

#leaf trait data should look like this: (each leaf replicate represented per species per site)
species	site	leaf.id	LA	LDMC	SLA
Horc_lana	1	1	685.31	41.21621622	11.23459016
Horc_lana	1	2	1352.6	25.46296296	24.59272727

leaf_traits <- read.csv("__YOURDATA_leaf_traits.csv")


#Prepare data - using actual column names from the table
trait_data <- data.frame(
  species = as.factor(leaf_traits$species),
  site = as.factor(leaf_traits$site),
  la = leaf_traits$LA,
  ldmc =  leaf_traits$LDMC,
  sla = leaf_traits$SLA
)

#Calculate means for each species-site combination
species_means <- trait_data %>%
  group_by(species, site) %>%
  summarise(
    mean_la = mean(la, na.rm = TRUE),
    mean_ldmc = mean(ldmc, na.rm = TRUE),
    mean_sla = mean(sla, na.rm = TRUE)
  )

#Scale the data for PCA (standardizing range of independent variables so there is less variance)
#Update scaled_data to include mean_ldmc
scaled_data <- species_means %>%
  ungroup() %>%
  select(mean_la, mean_sla, mean_ldmc) %>%
  scale()

#Perform PCA 
pca_result <- prcomp(scaled_data)

#create biplot with confidence elipses
biplot_with_ellipses <- ggplot2::autoplot(pca_result,
                                          data = species_means,
                                          colour = 'site',
                                          size = 3) +
  geom_segment(data = as.data.frame(pca_result$rotation),
               aes(x = 0, y = 0,
                   xend = PC1*0.5,
                   yend = PC2*0.5),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black") +
  geom_text(data = as.data.frame(pca_result$rotation),
            aes(x = PC1*0.6,
                y = PC2*0.6,
                label = rownames(pca_result$rotation)),
            color = "black",
            family = "Times New Roman") +
  stat_ellipse(aes(color = site),
               level = 0.95,  #.95 confidence level
               type = "t",
               linetype = 2) +
  theme_minimal() +
  scale_color_manual(values = c("1" = "darkgoldenrod1", "2" = "deepskyblue1")) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(family = "Times New Roman", size = 12), #force times new roman as text
    axis.title = element_text(family = "Times New Roman", size = 15),
    plot.title = element_text(family = "Times New Roman", size = 20),
    legend.text = element_text(family = "Times New Roman", size = 15),
    legend.title = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) +
  labs(title = "PCA Biplot with Trait Arrows",
       x = paste0("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "%)"),
       y = paste0("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "%)"))

print(biplot_with_ellipses)

#save biplot as a png
ggsave("___ADDINFO_pca_biplot.png", 
       biplot_with_ellipses + 
         theme(
           panel.background = element_rect(fill = "white"),
           plot.background = element_rect(fill = "white")
         ),
       width = 8, 
       height = 6, 
       dpi = 300)





















                                
                                