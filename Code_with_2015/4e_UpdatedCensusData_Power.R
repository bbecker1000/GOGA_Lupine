source("Code_with_2015/4b_UpdatedCensusData_GLMER.R")

# Install packages
library(lme4)
library(lmerTest) 
library(MuMIn) 
library(pwr)
library(simr) 
library(dplyr)
library(stringr)


# Extracting the fixed effects from the yearly model
fixef(m_Lupin_census_allyears_nb)["TreatmentBurn:Year2009"] <- 0.5
fixef(m_Lupin_census_allyears_nb)["TreatmentMechanical:Year2009"] <- 0.5
fixef(m_Lupin_census_allyears_nb)["TreatmentBurn:Year2011"] <- 0.5
fixef(m_Lupin_census_allyears_nb)["TreatmentMechanical:Year2011"] <- 0.5
fixef(m_Lupin_census_allyears_nb)["TreatmentBurn:Year2012"] <- 0.5
fixef(m_Lupin_census_allyears_nb)["TreatmentMechanical:Year2012"] <- 0.5
fixef(m_Lupin_census_allyears_nb)["TreatmentBurn:Year2013"] <- 0.5
fixef(m_Lupin_census_allyears_nb)["TreatmentMechanical:Year2013"] <- 0.5
fixef(m_Lupin_census_allyears_nb)["TreatmentBurn:Year2015"] <- 0.5
fixef(m_Lupin_census_allyears_nb)["TreatmentMechanical:Year2015"] <- 0.5

# Extracting the fixed effects from the immature model
fixef(m_lupin_immature_count)["TreatmentBurn:Year2009"] <- 0.5
fixef(m_lupin_immature_count)["TreatmentMechanical:Year2009"] <- 0.5
fixef(m_lupin_immature_count)["TreatmentBurn:Year2011"] <- 0.5
fixef(m_lupin_immature_count)["TreatmentMechanical:Year2011"] <- 0.5
fixef(m_lupin_immature_count)["TreatmentBurn:Year2012"] <- 0.5
fixef(m_lupin_immature_count)["TreatmentMechanical:Year2012"] <- 0.5
fixef(m_lupin_immature_count)["TreatmentBurn:Year2013"] <- 0.5
fixef(m_lupin_immature_count)["TreatmentMechanical:Year2013"] <- 0.5
fixef(m_lupin_immature_count)["TreatmentBurn:Year2015"] <- 0.5
fixef(m_lupin_immature_count)["TreatmentMechanical:Year2015"] <- 0.5


# Set the number of simulations
Nsim <- 100

# Run power analysis
power_result_census2015 <- list(
  P2_Census_2009_BurnBefore = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentBurn:Year2009", "z"))),
  P2_Census_2009_MechBefore = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentMechanical:Year2009", "z"))),
  P2_Census_2011_BurnAfter = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentBurn:Year2011", "z"))),
  P2_Census_2011_MechAfter = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentMechanical:Year2011", "z"))),
  P2_Census_2012_BurnAfter = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentBurn:Year2012", "z"))),
  P2_Census_2012_MechAfter = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentMechanical:Year2012", "z"))),
  P2_Census_2013_BurnAfter = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentBurn:Year2013", "z"))),
  P2_Census_2013_MechAfter = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentMechanical:Year2013", "z"))),
  P2_Census_2015_BurnAfter = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentBurn:Year2015", "z"))),
  P2_Census_2015_MechAfter = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentMechanical:Year2015", "z"))),
  
  P2_Immature_2009_BurnBefore= summary(powerSim(m_lupin_immature_count, nsim = Nsim, test=fixed("TreatmentBurn:Year2009", "z"))),
  P2_Immature_2009_MechBefore = summary(powerSim(m_lupin_immature_count, nsim = Nsim, test=fixed("TreatmentMechanical:Year2009", "z"))),
  P2_Immature_2011_BurnAfter = summary(powerSim(m_lupin_immature_count, nsim = Nsim, test=fixed("TreatmentBurn:Year2011", "z"))),
  P2_Immature_2011_MechAfter = summary(powerSim(m_lupin_immature_count, nsim = Nsim, test=fixed("TreatmentMechanical:Year2011", "z"))),
  P2_Immature_2012_BurnAfter = summary(powerSim(m_lupin_immature_count, nsim = Nsim, test=fixed("TreatmentBurn:Year2012", "z"))),
  P2_Immature_2012_MechAfter = summary(powerSim(m_lupin_immature_count, nsim = Nsim, test=fixed("TreatmentMechanical:Year2012", "z"))),
  P2_Immature_2013_BurnAfter = summary(powerSim(m_lupin_immature_count, nsim = Nsim, test=fixed("TreatmentBurn:Year2013", "z"))),
  P2_Immature_2013_MechAfter = summary(powerSim(m_lupin_immature_count, nsim = Nsim, test=fixed("TreatmentMechanical:Year2013", "z"))),
  P2_Immature_2015_BurnAfter= summary(powerSim(m_lupin_immature_count, nsim = Nsim, test=fixed("TreatmentBurn:Year2015", "z"))),
  P2_Immature_2015_MechAfter = summary(powerSim(m_lupin_immature_count, nsim = Nsim, test=fixed("TreatmentMechanical:Year2015", "z")))
  )


# CREATE A DATA FRAME WITH THE POWER RESULTS

# Save the model names from the power analysis
sim_name_census2015 <- names(power_result_census2015)

# Extract statistics while keeping them aligned
mean_c2015 <- sapply(power_result_census2015, function(x) x$mean)
lower_CI_c2015 <- sapply(power_result_census2015, function(x) x$lower)
upper_CI_c2015 <- sapply(power_result_census2015, function(x) x$upper)

# Split names into meaningful categories
split_names_c2015 <- str_split(sim_name_census2015, "_", simplify = TRUE)

# Ensure split_names has enough columns to prevent indexing errors
if (ncol(split_names_c2015) >= 4) {
  Group_c2015 <- split_names_c2015[, 2]
  ModelType_c2015 <- split_names_c2015[, 3]
  Test1.0_c2015 <- split_names_c2015[, 4]
  Test2.0_c2015 <- gsub("\\..*", "", Test1.0_c2015)
} else {
  stop("Unexpected naming structure in power_result2")
}

# Create a data frame with extracted information
power_census_2015 <- data.frame(
  Group = Group_c2015,
  Type = as.factor(ModelType_c2015),
  Fixed_Effect = Test2.0_c2015,
  Mean = mean_c2015,
  Lower_CI = lower_CI_c2015,
  Upper_CI = upper_CI_c2015,
  stringsAsFactors = FALSE
)


# Create a new column that puts year/type and fixed effect together
power_census_2015$test2 <- paste(power_census_2015$Type, 
                                 power_census_2015$Fixed_Effect, 
                                 sep = "_")


# Create a csv file for the dataframe, so we don't have to rerun the power analysis
# write.csv(power_census_2015,
#           file = file.path("Data", "power_census_2015.csv"),
#           row.names = FALSE)





# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ #


# POWER ANALYSIS FOR GLMER MODELS BY STATUS


# Extracting the fixed effects from the lupine model
fixef(m_Lupin_census_status_nb)["TreatmentBurn:Trt_Statusafter"] <- 0.5
fixef(m_Lupin_census_status_nb)["TreatmentMechanical:Trt_Statusafter"] <- 0.5
fixef(m_Lupin_census_status_nb)["scale(yearly_rain)"] <- 0.5


# Extracting the fixed effects from the shrub model
fixef(m_lupin_status_immature_count)["TreatmentBurn:Trt_Statusafter"] <- 0.5
fixef(m_lupin_status_immature_count)["TreatmentMechanical:Trt_Statusafter"] <- 0.5
fixef(m_lupin_status_immature_count)["scale(yearly_rain)"] <- 0.5


# Set the number of simulations
Nsim <- 100

# Run the analysis
power_result_census_Status_2015 <- list(
  P2_Census_PrePost_BurnAfter = summary(powerSim(m_Lupin_census_status_nb, nsim = Nsim, test=fixed("TreatmentBurn:Trt_Statusafter", "z"))),
  P2_Census_PrePost_MechAfter = summary(powerSim(m_Lupin_census_status_nb, nsim = Nsim, test=fixed("TreatmentMechanical:Trt_Statusafter", "z"))),
  P2_Census_PrePost_Rainfall = summary(powerSim(m_Lupin_census_status_nb, nsim = Nsim, test=fixed("scale(yearly_rain)", "z"))),
  
  P2_Immature_PrePost_BurnAfter = summary(powerSim(m_lupin_status_immature_count, nsim = Nsim, test=fixed("TreatmentBurn:Trt_Statusafter", "z"))),
  P2_Immature_PrePost_MechAfter = summary(powerSim(m_lupin_status_immature_count, nsim = Nsim, test=fixed("TreatmentMechanical:Trt_Statusafter", "z"))),
  P2_Immature_PrePost_Rainfall = summary(powerSim(m_lupin_status_immature_count, nsim = Nsim, test=fixed("scale(yearly_rain)", "z")))
  )


# CREATE A DATA FRAME WITH THE POWER RESULTS

# Save the model names from the power analysis
sim_name_census_Status_2015 <- names(power_result_census_Status_2015)

# Extract statistics while keeping them aligned
mean_sc2015 <- sapply(power_result_census_Status_2015, function(x) x$mean)
lower_CI_sc2015 <- sapply(power_result_census_Status_2015, function(x) x$lower)
upper_CI_sc2015 <- sapply(power_result_census_Status_2015, function(x) x$upper)

# Split names into meaningful categories
split_names_census_status_2015 <- str_split(sim_name_census_Status_2015, "_", simplify = TRUE)

# Ensure split_names has enough columns to prevent indexing errors
if (ncol(split_names_census_status_2015) >= 4) {
  Group_sc2015 <- split_names_census_status_2015[, 2]
  ModelType_sc2015 <- split_names_census_status_2015[, 3]
  Test1.0_sc2015 <- split_names_census_status_2015[, 4]
  Test2.0_sc2015 <- gsub("\\..*", "", Test1.0_sc2015)
} else {
  stop("Unexpected naming structure in power_result2")
}

# Create a data frame with extracted information
power_census_Status_2015 <- data.frame(
  Group = Group_sc2015,
  Type = as.factor(ModelType_sc2015),
  Fixed_Effect = Test2.0_sc2015,
  Mean = mean_sc2015,
  Lower_CI = lower_CI_sc2015,
  Upper_CI = upper_CI_sc2015,
  stringsAsFactors = FALSE
)


# Create a new column that puts year/type and fixed effect together
power_census_Status_2015$test2 <- paste(power_census_Status_2015$Type, 
                                     power_census_Status_2015$Fixed_Effect, 
                                     sep = "_")


# Create a csv file for the dataframe, so we don't have to rerun the power analysis
# write.csv(power_census_Status_2015,
#           file = file.path("Data", "power_census_Status_2015.csv"),
#           row.names = FALSE)




