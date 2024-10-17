source("Code/1b_Data_setup_NMS.R")

# Setting up dataframe to run GLMER

# Set up a dataframe that contains data for a model on Lupin abundance
Lupin_data <- sum_allspp %>%
  filter(Species %in% c("LUAL", "LUFO", "LUVA"))

# Set up a dataframe that contains data for a model on nativity
Nativity_data <- CL_Complete %>%
  group_by(Year,
           Site,
           Plot,
           Treatment,
           MacroPlot,
           yearly_rain, 
           Native) %>%
  summarise(Total_Count = sum(Count), .groups = "keep")


