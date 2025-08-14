# Load Packages
library(ggordiplots)
library(vegan)
library(tidyverse)

# Upload data
CL_All <- read_csv("Data/Cover_Lifeform_All.csv")
Precip_cm <- read_csv("Data/cm_yearly_rain.csv")
CL_All_Updated <- read_csv("Data/MBB_Transect_AllYears.csv")


# Step 1: Get unique combinations from CL_All
unique_CL_All <- CL_All %>%
  select(Species, Lifecycle, Preferred_LF, Default_LF, Native, Invasive) %>%
  distinct()

# Step 2: Join with CL_All_2015 by Species
CL_All_2015 <- CL_All_Updated %>%
  left_join(unique_CL_All, by = "Species")


# Create a plot column from MacroPlot
CL_All_2015$Plot <- str_extract(CL_All_2015$MacroPlot, "\\d+")
CL_All_2015 <- CL_All_2015 %>% mutate(Treatment = word(MacroPlot, 2, sep = "_"))

CL_All_2015$yr_trt <- paste(CL_All_2015$Year, 
                       "_", 
                       CL_All_2015$Treatment)

# Make sure Year and Plot are being treated as characters not numeric
CL_All_2015$Plot <- as.character(CL_All_2015$Plot)
CL_All_2015$Year <- as.character(CL_All_2015$Year)

# Change the column name I want to join by so they match in both dataframes
colnames(Precip_cm) [1] <- "Year"

# Join the two data frames by Year
CL_Complete_2015 <- merge(x = CL_All_2015, y = Precip_cm,
                     by = "Year", all.x = TRUE)

CL_Complete_2015 <- CL_Complete_2015 %>%
  mutate(Trt_Status = case_when(
    Year %in% c(2009, 2010) ~ "before",
    Year %in% c(2011, 2012, 2013, 2015) ~ "after"))


# set Control as base level
CL_Complete_2015$Treatment <- factor(CL_Complete_2015$Treatment, 
                                levels = c("Control", "Burn", "Mechanical"))


# set Pre-treatment as base level
CL_Complete_2015$Trt_Status <- factor(CL_Complete_2015$Trt_Status, 
                                 levels = c("before", "after"))

# create a new column with 
CL_Complete_2015 <- CL_Complete_2015 %>%
  mutate(Time_Since_Trt = as.numeric(Year) - 2010)


# Replace NAs in the Lifecycle, Preferred_LF, Default_LF, Native, Invasive for Species POGL
CL_Complete_2015[CL_Complete_2015$Species == "POGL", "Lifecycle"] <- "Perennial"
CL_Complete_2015[CL_Complete_2015$Species == "POGL", "Preferred_LF"] <- "Forb/Herb"
CL_Complete_2015[CL_Complete_2015$Species == "POGL", "Default_LF"] <- "Forb"
CL_Complete_2015[CL_Complete_2015$Species == "POGL", "Native"] <- TRUE
CL_Complete_2015[CL_Complete_2015$Species == "POGL", "Invasive"] <- FALSE

# Replace NAs in the Lifecycle, Preferred_LF, Default_LF, Native, Invasive for LICH
CL_Complete_2015[CL_Complete_2015$Species == "LICH", "Lifecycle"] <- "Perennial"
CL_Complete_2015[CL_Complete_2015$Species == "LICH", "Preferred_LF"] <- "Lichen"
CL_Complete_2015[CL_Complete_2015$Species == "LICH", "Default_LF"] <- "Lichen"
CL_Complete_2015[CL_Complete_2015$Species == "LICH", "Native"] <- TRUE
CL_Complete_2015[CL_Complete_2015$Species == "LICH", "Invasive"] <- FALSE

# Replace NAs in the Lifecycle, Preferred_LF, Default_LF, Native, Invasive for Species OXEP1
CL_Complete_2015[CL_Complete_2015$Species == "OXPE1", "Lifecycle"] <- "Perennial"
CL_Complete_2015[CL_Complete_2015$Species == "OXPE1", "Preferred_LF"] <- "Forb/Herb"
CL_Complete_2015[CL_Complete_2015$Species == "OXPE1", "Default_LF"] <- "Forb"
CL_Complete_2015[CL_Complete_2015$Species == "OXPE1", "Native"] <- FALSE
CL_Complete_2015[CL_Complete_2015$Species == "OXPE1", "Invasive"] <- TRUE


# Replace NAs in the Lifecycle, Preferred_LF, Default_LF for substrates
CL_Complete_2015[CL_Complete_2015$Species == "LITT", "Lifecycle"] <- "substrate"
CL_Complete_2015[CL_Complete_2015$Species == "LITT", "Preferred_LF"] <- "substrate"
CL_Complete_2015[CL_Complete_2015$Species == "LITT", "Default_LF"] <- "substrate"

CL_Complete_2015[CL_Complete_2015$Species == "BARE", "Lifecycle"] <- "substrate"
CL_Complete_2015[CL_Complete_2015$Species == "BARE", "Preferred_LF"] <- "substrate"
CL_Complete_2015[CL_Complete_2015$Species == "BARE", "Default_LF"] <- "substrate"

CL_Complete_2015[CL_Complete_2015$Species == "ROCK", "Lifecycle"] <- "substrate"
CL_Complete_2015[CL_Complete_2015$Species == "ROCK", "Preferred_LF"] <- "substrate"
CL_Complete_2015[CL_Complete_2015$Species == "ROCK", "Default_LF"] <- "substrate"

CL_Complete_2015[CL_Complete_2015$Species == "WOOD", "Lifecycle"] <- "substrate"
CL_Complete_2015[CL_Complete_2015$Species == "WOOD", "Preferred_LF"] <- "substrate"
CL_Complete_2015[CL_Complete_2015$Species == "WOOD", "Default_LF"] <- "substrate"

CL_Complete_2015[CL_Complete_2015$Species == "MOSS", "Lifecycle"] <- "substrate"
CL_Complete_2015[CL_Complete_2015$Species == "MOSS", "Preferred_LF"] <- "substrate"
CL_Complete_2015[CL_Complete_2015$Species == "MOSS", "Default_LF"] <- "substrate"

CL_Complete_2015[CL_Complete_2015$Species == "GOPH", "Lifecycle"] <- "substrate"
CL_Complete_2015[CL_Complete_2015$Species == "GOPH", "Preferred_LF"] <- "substrate"
CL_Complete_2015[CL_Complete_2015$Species == "GOPH", "Default_LF"] <- "substrate"

CL_Complete_2015$Lifecycle[is.na.data.frame(CL_Complete_2015$Lifecycle)] <- "Unknown"
CL_Complete_2015$Lifecycle[CL_Complete_2015$Lifecycle == "Not Defined"] <- "Unknown"


# saveRDS(CL_Complete_2015, file = "CL_Complete_2015")
 







