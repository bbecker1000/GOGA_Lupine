library(tidyverse)

# Import data
Lupine_Density_2009_2015 <- read_csv("Data/MBB_Lupine Density_2009_2015.csv")

# View(Lupine_Density_2009_2015)

Lupine_Density_2009_2015$Plot <- str_extract(Lupine_Density_2009_2015$Plot, "\\d+$")

# Make a pre/post-treatment cateogry
Lupine_Density_2009_2015 <- Lupine_Density_2009_2015 %>%
  mutate(Trt_Status = case_when(
    Year %in% c(2010) ~ "before",
    Year %in% c(2011, 2012, 2013, 2015) ~ "after"))


# Set control as base level
Lupine_Density_2009_2015$Treatment <- factor(Lupine_Density_2009_2015$Treatment, 
                                 levels = c("Control", "Burn", "Mechanical"))

# Set pre-treatment as base level
Lupine_Density_2009_2015$Trt_Status <- factor(Lupine_Density_2009_2015$Trt_Status, 
                                  levels = c("before", "after"))

Lupin_Density_2009_2015$Year <- factor(Lupin_Density_2009_2015$Year, 
                                     levels = c("2010", "2009", "2011", "2012", "2013", "2015"))


# Sum up counts so that all lupine species are grouped
Lupine_Density_2009_2015_grouped_live <- Lupine_Density_2009_2015 %>%
  filter(Status == "L") %>%
  group_by(MacroPlot, Site, Plot, Treatment, Year, Trt_Status, Status, UV1) %>%
  summarise(Count = sum(Count), .groups = "keep") %>%
  ungroup()


# Plot all data in a histogram to determine distribution
hist(Lupine_Density_2009_2015_grouped_live$Count)

# Create a dataframe with the ratio of young to mature lupine
Lupin_Ratio_2009_2015 <- Lupine_Density_2009_2015_grouped_live %>%
  group_by(MacroPlot, Year, Site, Plot, Trt_Status, Treatment) %>%
  summarise(
    Count_I = sum(Count[UV1 == "I"], na.rm = TRUE),
    Count_M = sum(Count[UV1 == "M"], na.rm = TRUE),
    Total_Count = sum(Count, na.rm = TRUE),
    Ratio_I_M = (Count_I / Total_Count),
    .groups = "keep") %>%
  ungroup() 

Lupin_Ratio_2009_2015 <- as.data.frame(Lupin_Ratio_2009_2015)


# Plot all data in a histogram to determine distribution
hist(Lupin_Ratio_2009_2015$Ratio_I_M)

