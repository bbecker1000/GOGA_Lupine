source("Code/1a_alldata.R")
source("Code/1b_Data_setup_NMS.R")

# Setting up dataframes to run GLMER

# Create a table that has the macroplot, the year, and the total detections
total_detections <- CL_Complete %>%
  select(MacroPlot, Year, Count) %>%
  group_by(MacroPlot, Year) %>%
  summarise(Total_Count = sum(Count), .groups = "keep") 


# FOR LUPIN DATA #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #

# Set up a dataframe that contains data for a model on Lupin abundance
# THIS DATAFRAME INCLUDES ZEROS AND TOTAL DETECTIONS

# Need to make implicitly missing data explicitly missing ie count = 0
sum_allspp_zeros <- sum_allspp %>%
  ungroup() %>%
  complete(nesting(Year, Trt_Status, Site, Plot, Treatment, MacroPlot, yr_trt, yearly_rain), 
           Species,
           fill = list(Total_Count = 0))

# now we can get lupin counts even including macroplots with the count of zero
Lupin <- sum_allspp_zeros %>%
  filter(Species %in% c("LUAL", "LUFO", "LUVA")) %>%
  group_by(Year,
           Trt_Status,
           Site,
           Plot,
           Treatment,
           MacroPlot,
           yr_trt,
           yearly_rain) %>%
  summarise(Total_Lupin = sum(Total_Count), .groups = "keep")

# set Control as base level
Lupin$Treatment <- factor(Lupin$Treatment, 
                                levels = c("CONTROL", "BURN", "MECHANICAL"))

# set Pre-treatment as base level
Lupin$Trt_Status <- factor(Lupin$Trt_Status, levels = c("before", "after"))

#make year numeric
Lupin$Year.numeric <- as.numeric(Lupin$Year)
Lupin$Year_Time_since_trt <- Lupin$Year.numeric-2011

# we want a dataset that includes total detections for each macroplot in each year
Lupin_data <- merge(x = Lupin, y = total_detections, 
                              by.x = c("MacroPlot", "Year"),
                              by.y = c("MacroPlot", "Year"), 
                              all = TRUE)

#we think that treatment didn't happen until after 2010 sampling, so start with 2011 data
Lupin_data_2011_2013 <- Lupin_data %>% filter(Year >  2010)


# FOR NATIVE DATA #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #

# Set up a dataframe that contains data for a model on nativity
Nativity <- CL_Complete %>%
  filter(Native == TRUE) %>%
  group_by(Year,
           Trt_Status,
           Site,
           Plot,
           Treatment,
           MacroPlot,
           yearly_rain,
           Native) %>%
  summarise(Total_Native = sum(Count), .groups = "keep")

# set Control as base level
Nativity$Treatment <- factor(Nativity$Treatment, 
                                  levels = c("CONTROL", "BURN", "MECHANICAL"))

# set Pre-treatment as base level
Nativity$Trt_Status <- factor(Nativity$Trt_Status, 
                             levels = c("before", "after"))

#make year numeric
Nativity$Year.numeric <- as.numeric(Nativity$Year)
Nativity$Year_Time_since_trt <- Nativity$Year.numeric-2011

Nativity_data <- merge(x = Nativity, y = total_detections, 
                     by.x = c("MacroPlot", "Year"),
                     by.y = c("MacroPlot", "Year"), 
                     all = TRUE)

#we think that treatment didn't happen until after 2010 sampling, so start with 2011 data
Nativity_data_2011_2013 <- Nativity_data %>% filter(Year >  2010)

# FOR INVASIVE DATA #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #

# Set up a dataframe that contains data for a model on invasives
Invasive <- CL_Complete %>%
  filter(Invasive == TRUE) %>%
  group_by(Year,
           Trt_Status,
           Site,
           Plot,
           Treatment,
           MacroPlot,
           yearly_rain, 
           Invasive) %>%
  summarise(Total_Invasive = sum(Count), .groups = "keep")

# set Control as base level
Invasive$Treatment <- factor(Invasive$Treatment, 
                                  levels = c("CONTROL", "BURN", "MECHANICAL"))

# set Pre-treatment as base level
Invasive$Trt_Status <- factor(Invasive$Trt_Status, 
                              levels = c("before", "after"))

#make year numeric
Invasive$Year.numeric <- as.numeric(Invasive$Year)
Invasive$Year_Time_since_trt <- Invasive$Year.numeric-2011

# we want a dataset that includes total detections for each macroplot in each year
Invasive_data <- merge(x = Invasive, y = total_detections, 
                           by.x = c("MacroPlot", "Year"),
                           by.y = c("MacroPlot", "Year"), 
                           all = TRUE)

#we think that treatment didn't happen until after 2010 sampling, so start with 2011 data
Invasive_data_2011_2013 <- Invasive_data %>% filter(Year >  2010)

# FOR SHRUB DATA #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #

# Set up a dataframe that contains data for a model on shrubs
Shrubs <- CL_Complete %>%
  filter(Default_LF == "Shrub") %>%
  group_by(Year,
           Trt_Status,
           Site,
           Plot,
           Treatment,
           MacroPlot,
           yearly_rain, 
           Default_LF) %>%
  summarise(Total_Shrubs = sum(Count), .groups = "keep")

# set Control as base level
Shrubs$Treatment <- factor(Shrubs$Treatment, 
                             levels = c("CONTROL", "BURN", "MECHANICAL"))

# set Pre-treatment as base level
Shrubs$Trt_Status <- factor(Shrubs$Trt_Status, 
                              levels = c("before", "after"))

#make year numeric
Shrubs$Year.numeric <- as.numeric(Shrubs$Year)
Shrubs$Year_Time_since_trt <- Shrubs$Year.numeric-2011

# we want a dataset that includes total detections for each macroplot in each year
Shrubs_data <- merge(x = Shrubs, y = total_detections, 
                       by.x = c("MacroPlot", "Year"),
                       by.y = c("MacroPlot", "Year"), 
                       all = TRUE)

Shrubs_data <- drop_na(Shrubs_data)

