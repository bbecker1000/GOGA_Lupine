library(tidyverse)
library(simr)

# Upload power data
power_LPI_2015 <- read_csv("Data/power_LPI_2015.csv") #GLMER LPI by Year
power_LPI_Status_2015 <- read_csv("Data/power_LPI_Status_2015.csv") #GLMER LPI by before/after
power_census_2015 <- read_csv("Data/power_census_2015.csv") #GLMER Census by Year
power_census_Status_2015 <- read_csv("Data/power_census_Status_2015.csv") #GLMER Census by before/after

saveRDS(power_LPI_2015, "power_LPI_2015.rds")
saveRDS(power_LPI_Status_2015, "power_LPI_Status_2015.rds")
saveRDS(power_census_2015, "power_census_2015.rds")
saveRDS(power_census_Status_2015, "power_census_Status_2015.rds")

# Make sure the column Type ie Year is treated as a factor and not numeric
power_LPI_2015$Type <- as.factor(power_LPI_2015$Type)
power_census_2015$Type <- as.factor(power_census_2015$Type)

# Merge them into one dataset
power_2015 <- bind_rows(
  power_LPI_2015,
  power_LPI_Status_2015,
  power_census_2015,
  power_census_Status_2015
)

# Save the combined data frames
write.csv(power_2015,
          file = file.path("Data", "power_2015.csv"),
          row.names = FALSE)
