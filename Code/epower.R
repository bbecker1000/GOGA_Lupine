#### Set up data for epower

# Upload the data we will need
source("Code/1a_alldata.R")
source("Code/1c_Data_setup_GLMER.R")

Lupin_power <- Lupin_data %>%
  access_power()


Native_power <- Nativity_data %>%
  access_power()

  
Invasive_power <- Invasive_data %>%
  access_power()


