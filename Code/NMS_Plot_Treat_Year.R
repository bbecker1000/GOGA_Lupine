# Load packages
library(ggord)
library(vegan)
library(tidyverse)

#Enable the r-universe repo
# options(repos = c(
#   fawda123 = 'https://fawda123.r-universe.dev',
#   CRAN = 'https://cloud.r-project.org'))

# Upload data
CL_All <- read_csv("Data/Cover_Lifeform_All.csv")
# View(CL_All)
# why are there 28 rows of data missing? why did they do 5 sets of plots instead of 4?

# unique(CL_All$Year)

# Sum up individual species counts to get total species counts for each plot in a given year
sum <- aggregate.data.frame(CL_All$Count, 
                            by=list(CL_All$MacroPlot, 
                                    CL_All$Year, 
                                    CL_All$Treatment,
                                    CL_All$Species), 
                            FUN=sum)

names(sum) <- c("MacroPlot", "Year", "Treatment", "Species", "total_count")

# Pivot the data wider 
wide_data <- sum %>%
  pivot_wider(names_from = Species, 
              values_from = total_count)

# Replace NAs with 0
wide_data[is.na.data.frame(wide_data)] <- 0
# View(wide_data)

#Make sure all rows >0
wide_data.test <- wide_data %>%
  rowwise() %>% 
  mutate(TOTAL = rowSums(across(where(is.numeric)))) %>%
  select(TOTAL)

# Remove first columns to get all numeric data for nms
wide_data.nms <- wide_data[,-c(1:3)]

# Create a new dataframe with plot info
data.plot <- wide_data %>%
  select(Year, 
         Treatment,
         MacroPlot)
# View(data.plot)

# Create new covariate to plot just year and treatment.
data.plot$yr_trt <- paste(data.plot$Year, "_", data.plot$Treatment)

#Create a new dataframe with plant info
data.spp1 <- CL_All %>%
  select(Species, Lifecycle, Preferred_LF, Default_LF, Native, Invasive)

data.spp <- data.spp1[!duplicated(data.spp1),]
# View(data.spp)


# Run the NMS
set.seed(10) # for repeatability
nms <- metaMDS(wide_data.nms, trymax = 25) ## BB changed from wide_data2 -> wide_data.nms
plot(nms)

# Plot by Year and Treatment
p.nmds <- ggord(nms, 
                grp_in = data.plot$yr_trt, #<-- BB edited to new variable
                pbslab = FALSE,
                arrow = NULL, 
                #size = 3,
                alpha_el = 0.2, 
                ptslab = TRUE,           # darkness of polygon
                poly = TRUE,
                labcol = "black",
                ellipse = T,
                ellipse_pro = 0.80, # confidence intervals for ellipse
                grp_title = "Treatment and Year",  
                repel = TRUE,            # make text not overlap
                txt = NULL,                 # size of text
                #cols = c('purple', 'orange', 'blue', 'green', 'red', 'yellow'), # you want to select colors
                facet = F, #make each grp_in on its own panel
                nfac = 0) +   # number of facet columns
  theme_gray(base_size = 18) # can add ggplot commands !

p.nmds

# Run an adonis
adonis2(wide_data.nms ~ Year + Treatment,
        data = data.plot,
        permutations = 1000)

adonis2(wide_data.nms ~ MacroPlot,
        data = wide_data,
        permutations = 1000)
