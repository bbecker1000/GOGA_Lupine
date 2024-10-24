source("Code/1a_alldata.R")

# Setting up dataframes to run GLMER

# Create a table that has the macroplot, the year, and the total detections
total_detections <- CL_Complete %>%
  select(MacroPlot, Year, Count) %>%
  group_by(MacroPlot, Year) %>%
  summarise(Total_Detections = sum(Count), .groups = "keep")

# Set up a dataframe that contains data for a model on Lupin abundance
# THIS DATAFRAME INCLUDES ZEROS AND TOTAL DETECTIONS

# Need to make implicitly missing data explicitly missing ie count = 0
sum_allspp_zeros <- sum_allspp %>%
  ungroup() %>%
  complete(nesting(Year, Site, Plot, Treatment, MacroPlot, yr_trt, yearly_rain), Species,
           fill = list(Total_Count = 0))

# now we can get lupin counts even including macroplots with the count of zero
Lupin_data_zeros <- sum_allspp_zeros %>%
  filter(Species %in% c("LUAL", "LUFO", "LUVA")) %>%
  group_by(Year,
           Site,
           Plot,
           Treatment,
           MacroPlot,
           yr_trt,
           yearly_rain) %>%
  summarise(Total_Count_Lupin = sum(Total_Count), .groups = "keep")

# set Control as base level
Lupin_data_zeros$Treatment <- factor(Lupin_data_zeros$Treatment, 
                                     levels = c("CONTROL", "BURN", "MECHANICAL"))
#make year numeric
Lupin_data_zeros$Year.numeric <- as.numeric(Lupin_data_zeros$Year)
Lupin_data_zeros$Year_Time_since_trt <- Lupin_data_zeros$Year.numeric-2011
#we think that treatment didn't happen until after 2010 sampling, so start with 2011 data
Lupin_data_2011_2013_zeros <- Lupin_data_zeros %>% filter(Year >  2010)

# we want a dataset that includes total detections for each macroplot in each year
Lupin_merge_zeros <- merge(x = Lupin_data_zeros, y = total_detections, 
                              by.x = c("MacroPlot", "Year"),
                              by.y = c("MacroPlot", "Year"), 
                              all = TRUE)

#we think that treatment didn't happen until after 2010 sampling, so start with 2011 data
Lupin_merge_2011_2013_zeros <- Lupin_merge_zeros %>% filter(Year >  2010)

# THIS DATAFRAME ONLY INCLUDES NON-ZERO LUPINE COUNTS

# now we can get lupin counts even including macroplots with the count of zero
Lupin_data<- sum_allspp %>%
  filter(Species %in% c("LUAL", "LUFO", "LUVA")) %>%
  group_by(Year,
           Site,
           Plot,
           Treatment,
           MacroPlot,
           yr_trt,
           yearly_rain) %>%
  summarise(Total_Count_Lupin = sum(Total_Count), .groups = "keep")

# set Control as base level
Lupin_data$Treatment <- factor(Lupin_data$Treatment, 
                                     levels = c("CONTROL", "BURN", "MECHANICAL"))
#make year numeric
Lupin_data$Year.numeric <- as.numeric(Lupin_data$Year)
Lupin_data$Year_Time_since_trt <- Lupin_data$Year.numeric-2011
#we think that treatment didn't happen until after 2010 sampling, so start with 2011 data
Lupin_data_2011_2013 <- Lupin_data %>% filter(Year >  2010)




# Set up a dataframe that contains data for a model on nativity
Nativity <- CL_Complete %>%
  filter(Native == TRUE) %>%
  group_by(Year,
           Site,
           Plot,
           Treatment,
           MacroPlot,
           yearly_rain,
           Native) %>%
  summarise(Total_Count = sum(Count), .groups = "keep")

# set Control as base level
Nativity$Treatment <- factor(Nativity$Treatment, 
                                  levels = c("CONTROL", "BURN", "MECHANICAL"))
#make year numeric
Nativity$Year.numeric <- as.numeric(Nativity$Year)
Nativity$Year_Time_since_trt <- Nativity$Year.numeric-2011

Lupin_merge <- merge(x = Lupin_data, y = total_detections, 
                     by.x = c("MacroPlot", "Year"),
                     by.y = c("MacroPlot", "Year"), 
                     all = TRUE)

#we think that treatment didn't happen until after 2010 sampling, so start with 2011 data
Nativity_data_2011_2013 <- Nativity %>% filter(Year >  2010)

# Set up a dataframe that contains data for a model on invasives
Invasive <- CL_Complete %>%
  filter(Invasive == TRUE) %>%
  group_by(Year,
           Site,
           Plot,
           Treatment,
           MacroPlot,
           yearly_rain, 
           Invasive) %>%
  summarise(Total_Count = sum(Count), .groups = "keep")

# set Control as base level
Invasive$Treatment <- factor(Invasive$Treatment, 
                                  levels = c("CONTROL", "BURN", "MECHANICAL"))
#make year numeric
Invasive$Year.numeric <- as.numeric(Invasive$Year)
Invasive$Year_Time_since_trt <- Invasive$Year.numeric-2011
#we think that treatment didn't happen until after 2010 sampling, so start with 2011 data
Invasive_data_2011_2013 <- Invasive %>% filter(Year >  2010)

