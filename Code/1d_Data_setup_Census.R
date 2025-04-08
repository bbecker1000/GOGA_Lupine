#Load Packages
library(tidyverse)
library(lme4)
library(sjPlot)

#Upload data
Lupin_Census <- read_csv("Data/Lupine_Combined_AllYears.csv")

# Separate our MacroPlot so you have columns with the Plot and Site
Lupin_Census$Plot <- str_extract(Lupin_Census$MacroPlot, "\\d+")
Lupin_Census$Site = str_extract(Lupin_Census$MacroPlot, "[^_]+$")

# Make year a factor
Lupin_Census$Year = as.factor(Lupin_Census$Year)

# Make a pre/post-treatment cateogry
Lupin_Census <- Lupin_Census %>%
  mutate(Trt_Status = case_when(
    Year %in% c(2010) ~ "before",
    Year %in% c(2011, 2012, 2013) ~ "after"))

# Set control as base level
Lupin_Census$Treatment <- factor(Lupin_Census$Treatment, 
                                 levels = c("C", "B", "M"))

# set Pre-treatment as base level
Lupin_Census$Trt_Status <- factor(Lupin_Census$Trt_Status, 
                                  levels = c("before", "after"))


# Take a preliminary look at the data
ggplot(Lupin_Census, aes(x=as.factor(Year), y = RowCount, color = Treatment)) +
  geom_boxplot() +
  facet_wrap(~Status) +
  geom_point(aes(color = Treatment),
             alpha = 0.5,
             position = position_dodge(width = .75)) +
  labs(x = "Year",
       y = "Lupine Count",
       title = "Actual Lupine Census Data")

# Plot all data in a histogram to determine distribution
hist(Lupin_Census$RowCount)

# Remove dead lupine from the data set
Lupin_Live <- Lupin_Census %>%
  filter(Status == "L")

# Create a new dataframe that has counts for Mature and Immature lupine
Lupin_UV1 <- Lupin_Live %>%
  group_by(MacroPlot, Year, Site, Plot, Trt_Status, Treatment, UV1) %>%
  summarise(Count = sum(RowCount), .groups = "keep") %>%
  ungroup()

Lupin_Ratio <- Lupin_UV1 %>%
  group_by(MacroPlot, Year, Site, Plot, Trt_Status, Treatment) %>%
  summarise(
    Count_I = sum(Count[UV1 == "I"], na.rm = TRUE),
    Count_M = sum(Count[UV1 == "M"], na.rm = TRUE),
    Total_Count = sum(Count, na.rm = TRUE),
    Ratio_I_M = Count_I / Count_M,
    .groups = "keep"
  ) %>%
  ungroup() 

Lupin_Ratio <- as.data.frame(Lupin_Ratio)

Lupin_Ratio$Year <- as.factor(Lupin_Ratio$Year)

# Compare count of immature to mature lupines 
ggplot(Lupin_UV1, aes(x=as.factor(Year), y = Count, color = Treatment)) +
  geom_boxplot() +
  facet_wrap(~UV1) +
  geom_point(aes(color = Treatment),
             alpha = 0.5,
             position = position_dodge(width = .75)) +
  labs(x = "Year",
       y = "Lupine Count",
       title = "Actual Lupine Census Data")

  
