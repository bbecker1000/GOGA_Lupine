# Based on the poster published by GGNRA they only used these years 
# 2009 maybe be pretreatment data 

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

# Filter to only get 2010, 2011, and 2013 data
CL_limited <- CL_All[CL_All$Year %in% c(2010, 2011, 2013), ]

CL_limited <- CL_limited %>%
  mutate(Plot = str_extract(MacroPlot, "\\d+"))

CL_limited <- CL_limited %>%
  mutate(Site = str_extract(MacroPlot, "[^_]+$"))
view(CL_limited)

CL_limited2 <- CL_limited %>%
  select(-"MacroPlot")

sum_limited <- aggregate.data.frame(CL_limited$Count, 
                            by=list(CL_limited$Plot, 
                                    CL_limited$Year, 
                                    CL_limited$Treatment,
                                    CL_limited$Site,
                                    CL_limited$MacroPlot,
                                    CL_limited$Species), 
                            FUN=sum)

names(sum_limited) <- c("Plot", 
                        "Year", 
                        "Treatment", 
                        "Site", 
                        "MacroPlot",
                        "Species", 
                        "count_sum")

# Pivot the data wider 
wide_data_limited <- sum_limited %>%
  pivot_wider(names_from = Species, 
              values_from = count_sum)

# Replace NAs with 0
wide_data_limited[is.na.data.frame(wide_data_limited)] <- 0

wide_data_limited.nms <- wide_data_limited[,-c(1:5)]
# view(wide_data_limited.nms)

data.plot.limited <- wide_data_limited %>%
  select(Year, 
         Treatment,
         Plot,
         MacroPlot,
         Site)

# Create new covariate to plot just year and treatment.
data.plot.limited$yr_trt <- paste(data.plot.limited$Year, 
                                  "_", 
                                  data.plot.limited$Treatment)

#Create a new dataframe with plant info
data.spp1 <- CL_All %>%
  select(Species, Lifecycle, Preferred_LF, Default_LF, Native, Invasive)

data.spp <- data.spp1[!duplicated(data.spp1),]

# Run the NMS
set.seed(10) # for repeatability
nms.limited <- metaMDS(wide_data_limited.nms, trymax = 25) ## BB changed from wide_data2 -> wide_data.nms
# plot(nms.limited)

# Plot by Year and Treatment
p.nmds.limited <- ggord(nms.limited, 
                grp_in = data.plot.limited$yr_trt, #<-- BB edited to new variable
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

p.nmds.limited

h <- how(within = Within(type = "series"),
                       plots = Plots(strata = data.plot.limited$MacroPlot),
                       blocks = data.plot.limited$Plot,
                       nperm = 499)

adonis2(wide_data_limited.nms ~ Year + Treatment,
        data = data.plot.limited,
        dist = "bray",
        perm = h,
        by = "margin")

str(CL_All)

