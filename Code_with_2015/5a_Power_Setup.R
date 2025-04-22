library(tidyverse)
library(simr)

# Upload power data
power_LPI_2015 <- read_csv("Data/power_LPI_2015.csv") #GLMER LPI by Year
power_LPI_Status_2015 #GLMER LPI by before/after
power_census_2015 <- read_csv("Data/power_census_2015.csv") #GLMER Census by Year
power_census_Status_2015 #GLMER Census by before/after


# Merge them into one dataset
power_2015 <- rbind(power_LPI_2015, power_census_2015)

# Save the data
write.csv(power_2015, "power_2015.csv", row.names = FALSE)
