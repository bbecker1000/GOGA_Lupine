# Load necessary packages
library(tidyverse)
library(ggord)
library(vegan)

# Enable the r-universe repo
# options(repos = c(
#   fawda123 = 'https://fawda123.r-universe.dev',
#   CRAN = 'https://cloud.r-project.org'))

# Upload data
CL_All <- read_csv("Data/Cover_Lifeform_All.csv")
# View(CL_All)

#Sum up individual species counts to get total species counts for each treatment in a given year
sum_yr_tr <- CL_All %>%
  group_by(Species, 
           Year, 
           Treatment) %>%
  summarize(total_count = sum(Count))
# View(sum_yr_tr)

# Pivot the data wider 
wide_data_yr_tr <- sum_yr_tr %>%
  pivot_wider(names_from = Species, values_from = total_count)
# View(wide_data_yr_tr)

# Replace NAs with 0
wide_data_yr_tr[is.na.data.frame(wide_data_yr_tr)] <- 0
# View(wide_data_yr_tr)

# Make sure all rows >0
wide_data_yr_tr.test <- wide_data_yr_tr %>%
  rowwise() %>% 
  mutate(TOTAL = rowSums(across(where(is.numeric)))) %>%
  select(TOTAL)

# Remove first columns to get all numeric data for nms
wide_data_yr_tr.nms <- wide_data[,-c(1:4)]

# Create a new dataframe with plot info
data.plot_yr_tr <- wide_data_yr_tr %>%
  select(Year, 
         Treatment)
# View(data.plot_yr_tr)

# Create new covariate to plot just year and treatment.
data.plot_yr_tr$yr_trt <- paste(data.plot_yr_tr$Year, 
                                "_", 
                                data.plot_yr_tr$Treatment)

#Create a new dataframe with plant info
data.spp1 <- CL_All %>%
  select(Species, 
         Lifecycle, 
         Preferred_LF, 
         Default_LF, 
         Native, 
         Invasive)

data.spp <- data.spp1[!duplicated(data.spp1),]
# View(data.spp)

#run the NMS
set.seed(10) # for repeatability
nms_yr_tr <- metaMDS(wide_data_yr_tr.nms, trymax = 25)
# plot(nms_yr_tr)

#plot by Year and Treatment
p.nmds_yr_tr <- ggord(nms_yr_tr, 
                grp_in = data.plot_yr_tr$yr_trt, #<-- BB edited to new variable
                #pbslab = FALSE,
                arrow = NULL, 
                #size = 3,
                alpha_el = 0.2, 
                #ptslab = TRUE,           # darkness of polygon
                #poly = TRUE,
                #labcol = "black",
                ellipse = F,
                ellipse_pro = 0.80, # confidence intervals for ellipse
                grp_title = "Treatment and Year",  
                repel = TRUE,            # make text not overlap
                txt = NULL,                 # size of text
                #cols = c('purple', 'orange', 'blue', 'green', 'red', 'yellow'), # you want to select colors
                facet = F, #make each grp_in on its own panel
                nfac = 0) +   # number of facet columns
  theme_gray(base_size = 18) # can add ggplot commands !

p.nmds_yr_tr
