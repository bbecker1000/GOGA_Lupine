source("Code/1a_alldata.R")

# Setting up dataframe to run GLMER

# Set up a dataframe that contains data for a model on Lupin abundance
Lupin_data <- sum_allspp %>%
  filter(Species %in% c("LUAL", "LUFO", "LUVA"))

# set Control as base level
Lupin_data$Treatment <- factor(Lupin_data$Treatment, levels = c("CONTROL", "BURN", "MECHANICAL"))
#make year numeric
Lupin_data$Year.numeric <- as.numeric(Lupin_data$Year)
Lupin_data$Year_Time_since_trt <- Lupin_data$Year.numeric-2011
#we think that treatment didn't happen until after 2010 sampling, so start with 2011 data
Lupin_data_2011_2013 <- Lupin_data %>% filter(Year >  2010)

# Set up a dataframe that contains data for a model on nativity
Nativity <- CL_Complete %>%
  group_by(Year,
           Site,
           Plot,
           Treatment,
           MacroPlot,
           yearly_rain, 
           Native) %>%
  summarise(Total_Count = sum(Count), .groups = "keep")

# Filter to only have the native species
Nativity_data <- Nativity %>%
  filter(Native == TRUE)

# set Control as base level
Nativity_data$Treatment <- factor(Nativity_data$Treatment, 
                                  levels = c("CONTROL", "BURN", "MECHANICAL"))
#make year numeric
Nativity_data$Year.numeric <- as.numeric(Nativity_data$Year)
Nativity_data$Year_Time_since_trt <- Nativity_data$Year.numeric-2011
#we think that treatment didn't happen until after 2010 sampling, so start with 2011 data
Nativity_data_2011_2013 <- Nativity_data %>% filter(Year >  2010)

# Set up a dataframe that contains data for a model on invasives
Invasive <- CL_Complete %>%
  group_by(Year,
           Site,
           Plot,
           Treatment,
           MacroPlot,
           yearly_rain, 
           Invasive) %>%
  summarise(Total_Count = sum(Count), .groups = "keep")

# Filter to only have the native species
Invasive_data <- Invasive %>%
  filter(Invasive == TRUE)

# set Control as base level
Invasive_data$Treatment <- factor(Invasive_data$Treatment, 
                                  levels = c("CONTROL", "BURN", "MECHANICAL"))
#make year numeric
Invasive_data$Year.numeric <- as.numeric(Invasive_data$Year)
Invasive_data$Year_Time_since_trt <- Invasive_data$Year.numeric-2011
#we think that treatment didn't happen until after 2010 sampling, so start with 2011 data
Invasive_data_2011_2013 <- Invasive_data %>% filter(Year >  2010)

