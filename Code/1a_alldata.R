# Load Packages
library(ggordiplots)
library(vegan)
library(tidyverse)

# Upload data
CL_All <- read_csv("Data/Cover_Lifeform_All.csv")
Precip_cm <- read_csv("Data/cm_yearly_rain.csv")

# Separate our MacroPLot so you have columns with the Plot and Site
CL_All$Plot <- str_extract(CL_All$MacroPlot, "\\d+")
CL_All$Site = str_extract(CL_All$MacroPlot, "[^_]+$")

CL_All$yr_trt <- paste(CL_All$Year, 
                 "_", 
                 CL_All$Treatment)

# Make sure Year and Plot are being treated as characters not numeric
CL_All$Plot <- as.character(CL_All$Plot)
CL_All$Year <- as.character(CL_All$Year)

# Change the column name I want to join by so they match in both dataframes
colnames(Precip_cm) [1] <- "Year"

# Join the two data frames by Year
CL_Complete <- merge(x = CL_All, y = Precip_cm,
                     by = "Year", all.x = TRUE)

CL_Complete <- CL_Complete %>%
  mutate(Trt_Status = case_when(
    Year %in% c(2009, 2010) ~ "before",
    Year %in% c(2011, 2012, 2013) ~ "after"))


# set Control as base level
CL_Complete$Treatment <- factor(CL_Complete$Treatment, 
                                     levels = c("CONTROL", "BURN", "MECHANICAL"))

# set Pre-treatment as base level
CL_Complete$Trt_Status <- factor(CL_Complete$Trt_Status, 
                                           levels = c("before", "after"))

# create a new column with 
CL_Complete <- CL_Complete %>%
  mutate(Time_Since_Trt = as.numeric(Year) - 2010)

# saveRDS(CL_Complete, file = "CL_Complete")
