library(tidyverse)

CL_All <- read_csv("Data/Cover_Lifeform_All.csv")
View(CL_All)

#Create a new column
CL_All$YearMacroPlot <- paste(CL_All$Year,"-", CL_All$MacroPlot)

#Sum up individual species counts to get total species counts for each plot in a given year
sum <- CL_All %>%
  group_by(Species, Year, Treatment) %>%
  summarize(total_count = sum(Count))

# Pivot the data wider 
wide_data <- sum %>%
  pivot_wider(names_from = Species, values_from = total_count)

#Replace NAs with 0
wide_data[is.na.data.frame(wide_data)] <- 0
#view(wide_data)

#why are there 28 rows of data missing? why did they do 5 sets of plots instead of 4?

#Make sure all rows >0
wide_data.test <- wide_data %>%
  rowwise() %>% 
  mutate(TOTAL = rowSums(across(where(is.numeric)))) %>%
  select(TOTAL)

#Create a new dataframe with plot info
data.plot <- wide_data %>%
  select(Year,                 
         Treatment
         )
# view(data.plot)

#BB create new covariate to plot just year and treatment.  should add location?
data.plot$yr_trt <- paste(data.plot$Year, "_", data.plot$Treatment)

#Create a new dataframe with plant info
data.spp1 <- CL_All %>%
  select(Species, Lifecycle, Preferred_LF, Default_LF, Native, Invasive)
  
data.spp <- data.spp1[!duplicated(data.spp1),]
# view(data.spp)

#BB commented out
# Enable the r-universe repo
# options(repos = c(
#   fawda123 = 'https://fawda123.r-universe.dev',
#   CRAN = 'https://cloud.r-project.org'))

# Load packages
library(ggord)
library(vegan)


#BB add remove first columns to get all numeric data for nms
wide_data.nms <- wide_data[,-c(1:4)]


#run the NMS
set.seed(10) # for repeatability
nms <- metaMDS(wide_data.nms, trymax = 25) ## BB changed from wide_data2 -> wide_data.nms
plot(nms)

#plot by Year and Treatment
p.nmds <- ggord(nms, 
                grp_in = data.plot$yr_trt, #<-- BB edited to new variable
                pbslab = FALSE,
                arrow = NULL, 
                #size = 3,
                alpha_el = 0.2, 
                ptslab = TRUE,           # darkness of polygon
                poly = TRUE,
                labcol = "black",
                ellipse = F,
                ellipse_pro = 0.80, # confidence intervals for ellipse
                grp_title = "Treatment and Year",  
                repel = TRUE,            # make text not overlap
                txt = 2,                 # size of text
                #cols = c('purple', 'orange', 'blue', 'green', 'red', 'yellow'), # you want to select colors
                facet = F, #make each grp_in on its own panel
                nfac = 0) +   # number of facet columns
  theme_gray(base_size = 18) # can add ggplot commands !

p.nmds

#Creating group 1 which is lifecycle - default_lf - native
CL_All$Group1 <- paste(CL_All$Lifecycle, "-", 
                       CL_All$Default_LF, "-",
                       CL_All$Native)

sumG1 <- CL_All %>%
  group_by(Year, Treatment, Group1) %>%
  summarize(total_count = sum(Count))

wide_data_G1 <- sumG1 %>%
  pivot_wider(names_from = Group1, values_from = total_count)

#Replace NAs with 0 and remove non-numeric data
wide_data_G1[is.na.data.frame(wide_data_G1)] <- 0
wide_data_G1.nms <- wide_data_G1[,-c(1:2)]

#Create plot data sheet
data.plot_G1 <- wide_data_G1 %>%
  select(Year, 
         Treatment)

data.plot_G1$yr_trt <- paste(data.plot_G1$Year, "_", data.plot_G1$Treatment)

set.seed(10) # for repeatability
nms_G1 <- metaMDS(wide_data_G1.nms, trymax = 25) ## BB changed from wide_data2 -> wide_data.nms
#plot(nms_G1)

#plot by Year and Treatment
p.nmds_G1 <- ggord(nms_G1, 
                grp_in = data.plot_G1$yr_trt, #<-- BB edited to new variable
                pbslab = FALSE,
                arrow = NULL, 
                size = 3,
                alpha_el = 0.2, 
                ptslab = TRUE,           # darkness of polygon
                poly = TRUE,
                labcol = "black",
                ellipse = F,
                ellipse_pro = 0.80, # confidence intervals for ellipse
                grp_title = "Treatment and Year",  
                repel = T,            # make text not overlap
                txt = 2,                 # size of text
                facet = F, #make each grp_in on its own panel
                nfac = 0) +   # number of facet columns
  theme_gray(base_size = 18) # can add ggplot commands !

p.nmds_G1

adonis2(wide_data_G1.nms ~ Year * Treatment,
        data = wide_data_G1,
        permutations = 1000)

adonis2(wide_data_G1.nms ~ MacroPlot,
        data = wide_data_G1,
        permutations = 1000)
