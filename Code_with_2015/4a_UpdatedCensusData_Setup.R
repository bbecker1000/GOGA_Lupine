library(tidyverse)

# Import data
Lupine_Density_2009_2015_NoRain <- read_csv("Data/MBB_Lupine Density_2009_2015.csv")

# Add the rainfall data
Lupine_Density_2009_2015 <- merge(x = Lupine_Density_2009_2015_NoRain, y = Precip_cm,
         by = "Year", all.x = TRUE)


# Remove the text MBB from the Plot column
Lupine_Density_2009_2015$Plot <- str_extract(Lupine_Density_2009_2015$Plot, "\\d+$")


# Make a pre/post-treatment category
Lupine_Density_2009_2015 <- Lupine_Density_2009_2015 %>%
  mutate(Trt_Status = case_when(
    Year %in% c(2009, 2010) ~ "before",
    Year %in% c(2011, 2012, 2013, 2015) ~ "after"))


# Set control as baseline
Lupine_Density_2009_2015$Treatment <- factor(Lupine_Density_2009_2015$Treatment, 
                                 levels = c("Control", "Burn", "Mechanical"))

# Set pre-treatment as baseline
Lupine_Density_2009_2015$Trt_Status <- factor(Lupine_Density_2009_2015$Trt_Status, 
                                  levels = c("before", "after"))


# Sum up counts so that all lupine species are grouped
Lupine_Density_2009_2015_grouped_live <- Lupine_Density_2009_2015 %>%
  filter(Status == "L") %>%
  group_by(MacroPlot, Site, Plot, Treatment, Trt_Status, Year, yearly_rain, UV1) %>%
  summarise(Count = sum(Count), .groups = "keep") %>%
  ungroup()

# Set 2010 as the baseline
Lupine_Density_2009_2015_grouped_live$Year <- factor(Lupine_Density_2009_2015_grouped_live$Year, 
                                levels = c("2010", "2009", "2011", "2012", "2013", "2015"))



# Create a dataframe with the ratio of young to mature lupine
Lupin_Ratio_2009_2015 <- Lupine_Density_2009_2015_grouped_live %>%
  group_by(MacroPlot, Site, Plot, Treatment, Trt_Status, Year, yearly_rain) %>%
  summarise(
    Count_I = sum(Count[UV1 == "I"], na.rm = TRUE),
    Count_M = sum(Count[UV1 == "M"], na.rm = TRUE),
    Total_Count = sum(Count, na.rm = TRUE),
    Ratio_I_M = (Count_I / Total_Count),
    .groups = "keep") %>%
  ungroup() 

# Make sure its a dataframe
Lupin_Ratio_2009_2015 <- as.data.frame(Lupin_Ratio_2009_2015)


# Set 2010 as the baseline
Lupin_Ratio_2009_2015$Year <- factor(Lupin_Ratio_2009_2015$Year, 
                              levels = c("2010", "2009", "2011", "2012", "2013", "2015"))


