####4.1.2. CSR analysis####
# Load required libraries
library(ggplot2)
library(ggtern)
library(readr)
library(dplyr)
library(ggrepel) #this stupid thing doesnt work anymore for some reason 
#so manually inputting the labels might be necessary
library(patchwork)
install.packages("remotes")
remotes::install_version("ggtern", version = "3.3.0")
install.packages("devtools")
devtools::install_github("slowkow/ggrepel")

#for the darn labels to stop overlapping
nv = 0.05  #Vertical Adjustment
pn = position_nudge_tern(y=nv,x=-nv/.5,z=-nv/1)


#splitting up CSR analysis by site 

df1 <- read_excel('YOURDATASET.xlsx')
#example dataset format
#notes = site - using stratefy to calculate CSR values
#split up the two spreadsheets by site (I had 2)

#species_binomial family 	habit	notes	LA         leaf area (mm2)	LDMC         leaf dry matter content                  (%)	SLA specific leaf area                                   (mm2 mg-1)	C	S	R	CSR	S (%)	C (%)	R (%)	Red	Green	Blue	Color
#Agro_capi	Poaceae	graminoid	1	169.5765714	30.2	19.1	8.598565586	59.55836099	31.84307343	9 : 60 : 32 %	59.55836099	8.598565586	31.84307343	22	152	81	#165251
#Bell_pere	asteraceae	forb	1	206.325	12.2	40.28333333	13.1115538	0	86.8884462	13 : 0 : 87 %	0	13.1115538	86.8884462	33	0	222	#2100DE
#Card_prat	brassicaceae	forb	1	8.54	23.3	12.2	0	76.02102802	23.97897198	0 : 76 : 24 %	76.02102802	0	23.97897198	0	194	61	#00C23D
#I guess you could change the species binomial to the full taxonomic names but I did not do that 



#mutate colors (had to look this up)
df1 <- df1 %>%
  mutate(color = sprintf("#%02X%02X%02X", Red, Green, Blue))


#ternary plot 1
t1 <- ggtern(data = df1,
             aes(x = R, y = C, z = S,
                 color = color,
                 shape = habit,
                 label = species_binomial)) +
  theme_bw() +
  theme_nomask() +
  theme_showarrows() +
  labs(title = "___ADDINFOHERE___ Strategies Site 1") +
  scale_shape_manual(values = c(
    tree = 15,        # square
    forb = 16,        # circle
    graminoid = 17,   # triangle
    small_shrub = 18  # diamond
  )) +
  geom_point(size = 3, color = df1$color) +
  geom_text(position=pn,aes(label=species_binomial),check_overlap=T, size=3, color = "black", family = "Times") +
  guides(shape = guide_legend(title = "Growth Forms")) +
  theme(text = element_text(family = "Times New Roman")) +
  theme(legend.position = "none") 

print(t1)


df2 <- read_excel('YOURDATASET2.xlsx')
df2 <- df2 %>%
  mutate(color = sprintf("#%02X%02X%02X", Red, Green, Blue))

t2 <- ggtern(data = df2,
             aes(x = R, y = C, z = S,
                 color = color,
                 shape = habit,
                 label = species_binomial)) +
  theme_bw() +
  theme_nomask() +
  theme_showarrows() +
  labs(title = "Meadow Ecological Strategies Site 2") +
  scale_shape_manual(values = c(
    tree = 15,        # square
    forb = 16,        # circle
    graminoid = 17,   # triangle
    small_shrub = 18  # diamond
  )) +
  geom_point(size = 3, color = df2$color) +
  geom_text(position=pn,aes(label=species_binomial),check_overlap=T, size=3, color = "black", family = "Times") + #you can remove this for putting the names in manually???
  guides(shape = guide_legend(title = "Growth Forms")) +
  theme(text = element_text(family = "Times New Roman")) +
  theme(legend.position = "none") 
print(t2)


#add common theme with times new roman font to match with paper
common_theme <- theme_bw() +
  theme_nomask() +
  theme_showarrows() +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "right",
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        tern.axis.text = element_text(size = 10),  # Control axis text size
        tern.axis.title = element_text(size = 12)) # Control axis title size


# Or save plots with ggsave individually
ggsave("ternplot1.png", t1, width = 8, height = 8)
ggsave("ternplot2.png", t2, width = 8, height = 8)
#i had to go back and annotate each full species name


# Then combine saved plots using magick package
library(magick) #can read in images but you can also do this outside of R...
img1 <- image_read("ternplot1.png")
img2 <- image_read("ternplot2.png")
combined <- image_append(c(img1, img2))
image_write(combined, "combined_tern_plots.png")









