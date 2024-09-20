# Load packages
library(ggord)
library(vegan)
library(tidyverse)

# Enable the r-universe repo
# options(repos = c(
#   fawda123 = 'https://fawda123.r-universe.dev',
#   CRAN = 'https://cloud.r-project.org'))

# Upload cover data
CL_All <- read_csv("Data/Cover_Lifeform_All.csv")
# View(CL_All)

# Alter the data frame to separate the components of MacroPlot
CL_All2 <- CL_All %>%
  mutate(Plot = str_extract(MacroPlot, "\\d+"))

CL_All2 <- CL_All %>%
  mutate(Site = str_extract(MacroPlot, "[^_]+$"))

# Upload precipitation data
Precip_cm <- read_csv("Data/cm_yearly_rain.csv")
# View(Precip_cm)

# Change the column name I want to join by so they match in both dataframes
colnames(Precip_cm) [1] <- "Year"

# Join the two data frames by Year
CL_Precip <- merge(x = CL_All2, y = Precip_cm,
                   by = "Year", all.x = TRUE)
# View(CL_Precip)

