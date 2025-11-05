source("Code_with_2015/3b_TransectGLMER_ModelRun.R")

# Install packages
library(lme4)
library(lmerTest) 
library(MuMIn) 
library(pwr)
library(simr) 
library(dplyr)
library(stringr)


# POWER ANALYSIS FOR BY YEAR MODELS

# Extracting the fixed effects from the lupine model
fixef(m_Lupin_Year_2015)["TreatmentBurn:Year2009"] <- 0.5
fixef(m_Lupin_Year_2015)["TreatmentMechanical:Year2009"] <- 0.5
fixef(m_Lupin_Year_2015)["TreatmentBurn:Year2011"] <- 0.5
fixef(m_Lupin_Year_2015)["TreatmentMechanical:Year2011"] <- 0.5
fixef(m_Lupin_Year_2015)["TreatmentBurn:Year2012"] <- 0.5
fixef(m_Lupin_Year_2015)["TreatmentMechanical:Year2012"] <- 0.5
fixef(m_Lupin_Year_2015)["TreatmentBurn:Year2013"] <- 0.5
fixef(m_Lupin_Year_2015)["TreatmentMechanical:Year2013"] <- 0.5
fixef(m_Lupin_Year_2015)["TreatmentBurn:Year2015"] <- 0.5
fixef(m_Lupin_Year_2015)["TreatmentMechanical:Year2015"] <- 0.5

# Extracting the fixed effects from the nativity model
fixef(m_Nativity_Year_2015)["TreatmentBurn:Year2009"] <- 0.25
fixef(m_Nativity_Year_2015)["TreatmentMechanical:Year2009"] <- 0.25
fixef(m_Nativity_Year_2015)["TreatmentBurn:Year2011"] <- 0.25
fixef(m_Nativity_Year_2015)["TreatmentMechanical:Year2011"] <- 0.25
fixef(m_Nativity_Year_2015)["TreatmentBurn:Year2012"] <- 0.25
fixef(m_Nativity_Year_2015)["TreatmentMechanical:Year2012"] <- 0.25
fixef(m_Nativity_Year_2015)["TreatmentBurn:Year2013"] <- 0.25
fixef(m_Nativity_Year_2015)["TreatmentMechanical:Year2013"] <- 0.25
fixef(m_Nativity_Year_2015)["TreatmentBurn:Year2015"] <- 0.25
fixef(m_Nativity_Year_2015)["TreatmentMechanical:Year2015"] <- 0.25

# Extracting the fixed effects from the invasive model
fixef(m_Invasive_Year_2015)["TreatmentBurn:Year2009"] <- 0.25
fixef(m_Invasive_Year_2015)["TreatmentMechanical:Year2009"] <- 0.25
fixef(m_Invasive_Year_2015)["TreatmentBurn:Year2011"] <- 0.25
fixef(m_Invasive_Year_2015)["TreatmentMechanical:Year2011"] <- 0.25
fixef(m_Invasive_Year_2015)["TreatmentBurn:Year2012"] <- 0.25
fixef(m_Invasive_Year_2015)["TreatmentMechanical:Year2012"] <- 0.25
fixef(m_Invasive_Year_2015)["TreatmentBurn:Year2013"] <- 0.25
fixef(m_Invasive_Year_2015)["TreatmentMechanical:Year2013"] <- 0.25
fixef(m_Invasive_Year_2015)["TreatmentBurn:Year2015"] <- 0.25
fixef(m_Invasive_Year_2015)["TreatmentMechanical:Year2015"] <- 0.25

# Extracting the fixed effects from the shrub model
fixef(m_Shrub_Year_2015)["TreatmentBurn:Year2009"] <- 0.25
fixef(m_Shrub_Year_2015)["TreatmentMechanical:Year2009"] <- 0.25
fixef(m_Shrub_Year_2015)["TreatmentBurn:Year2011"] <- 0.25
fixef(m_Shrub_Year_2015)["TreatmentMechanical:Year2011"] <- 0.25
fixef(m_Shrub_Year_2015)["TreatmentBurn:Year2012"] <- 0.25
fixef(m_Shrub_Year_2015)["TreatmentMechanical:Year2012"] <- 0.25
fixef(m_Shrub_Year_2015)["TreatmentBurn:Year2013"] <- 0.25
fixef(m_Shrub_Year_2015)["TreatmentMechanical:Year2013"] <- 0.25
fixef(m_Shrub_Year_2015)["TreatmentBurn:Year2015"] <- 0.25
fixef(m_Shrub_Year_2015)["TreatmentMechanical:Year2015"] <- 0.25


# Set the number of simulations
Nsim <- 100

# Run the analysis
power_result_LPI2015 <- list(
  P2_Lupine_2009_BurnBefore = summary(powerSim(m_Lupin_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2009", "z"))),
  P2_Lupine_2009_MechBefore = summary(powerSim(m_Lupin_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2009", "z"))),
  P2_Lupine_2011_BurnAfter = summary(powerSim(m_Lupin_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2011", "z"))),
  P2_Lupine_2011_MechAfter = summary(powerSim(m_Lupin_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2011", "z"))),
  P2_Lupine_2012_BurnAfter = summary(powerSim(m_Lupin_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2012", "z"))),
  P2_Lupine_2012_MechAfter = summary(powerSim(m_Lupin_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2012", "z"))),
  P2_Lupine_2013_BurnAfter = summary(powerSim(m_Lupin_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2013", "z"))),
  P2_Lupine_2013_MechAfter = summary(powerSim(m_Lupin_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2013", "z"))),
  P2_Lupine_2015_BurnAfter = summary(powerSim(m_Lupin_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2015", "z"))),
  P2_Lupine_2015_MechAfter = summary(powerSim(m_Lupin_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2015", "z"))),
  
  P2_Native_2009_BurnBefore= summary(powerSim(m_Nativity_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2009", "z"))),
  P2_Native_2009_MechBefore = summary(powerSim(m_Nativity_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2009", "z"))),
  P2_Native_2011_BurnAfter = summary(powerSim(m_Nativity_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2011", "z"))),
  P2_Native_2011_MechAfter = summary(powerSim(m_Nativity_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2011", "z"))),
  P2_Native_2012_BurnAfter = summary(powerSim(m_Nativity_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2012", "z"))),
  P2_Native_2012_MechAfter = summary(powerSim(m_Nativity_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2012", "z"))),
  P2_Native_2013_BurnAfter = summary(powerSim(m_Nativity_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2013", "z"))),
  P2_Native_2013_MechAfter = summary(powerSim(m_Nativity_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2013", "z"))),
  P2_Native_2015_BurnAfter= summary(powerSim(m_Nativity_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2015", "z"))),
  P2_Native_2015_MechAfter = summary(powerSim(m_Nativity_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2015", "z"))),
  
  P2_Invasive_2009_BurnBefore = summary(powerSim(m_Invasive_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2009", "z"))),
  P2_Invasive_2009_MechBefore = summary(powerSim(m_Invasive_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2009", "z"))),
  P2_Invasive_2011_BurnAfter = summary(powerSim(m_Invasive_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2011", "z"))),
  P2_Invasive_2011_MechAfter = summary(powerSim(m_Invasive_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2011", "z"))),
  P2_Invasive_2012_BurnAfter = summary(powerSim(m_Invasive_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2012", "z"))),
  P2_Invasive_2012_MechAfter = summary(powerSim(m_Invasive_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2012", "z"))),
  P2_Invasive_2013_BurnAfter = summary(powerSim(m_Invasive_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2013", "z"))),
  P2_Invasive_2013_MechAfter = summary(powerSim(m_Invasive_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2013", "z"))),
  P2_Invasive_2015_BurnAfter = summary(powerSim(m_Invasive_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2015", "z"))),
  P2_Invasive_2015_MechAfter = summary(powerSim(m_Invasive_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2015", "z"))),
  
  P2_Shrub_2009_BurnBefore= summary(powerSim(m_Shrub_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2009", "z"))),
  P2_Shrub_2009_MechBefore = summary(powerSim(m_Shrub_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2009", "z"))),
  P2_Shrub_2011_BurnAfter = summary(powerSim(m_Shrub_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2011", "z"))),
  P2_Shrub_2011_MechAfter = summary(powerSim(m_Shrub_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2011", "z"))),
  P2_Shrub_2012_BurnAfter = summary(powerSim(m_Shrub_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2012", "z"))),
  P2_Shrub_2012_MechAfter = summary(powerSim(m_Shrub_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2012", "z"))),
  P2_Shrub_2013_BurnAfter = summary(powerSim(m_Shrub_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2013", "z"))),
  P2_Shrub_2013_MechAfter = summary(powerSim(m_Shrub_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2013", "z"))),
  P2_Shrub_2015_BurnAfter= summary(powerSim(m_Shrub_Year_2015, nsim = Nsim, test=fixed("TreatmentBurn:Year2015", "z"))),
  P2_Shrub_2015_MechAfter = summary(powerSim(m_Shrub_Year_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Year2015", "z")))
  )


# CREATE A DATA FRAME WITH THE POWER RESULTS

# Save the model names from the power analysis
sim_name_LPI2015 <- names(power_result_LPI2015)

# Extract statistics while keeping them aligned
mean_2015 <- sapply(power_result_LPI2015, function(x) x$mean)
lower_CI_2015 <- sapply(power_result_LPI2015, function(x) x$lower)
upper_CI_2015 <- sapply(power_result_LPI2015, function(x) x$upper)

# Split names into meaningful categories
split_names_2015 <- str_split(sim_name_LPI2015, "_", simplify = TRUE)

# Ensure split_names has enough columns to prevent indexing errors
if (ncol(split_names_2015) >= 4) {
  Group_2015 <- split_names_2015[, 2]
  ModelType_2015 <- split_names_2015[, 3]
  Test1.0_2015 <- split_names_2015[, 4]
  Test2.0_2015 <- gsub("\\..*", "", Test1.0_2015)
} else {
  stop("Unexpected naming structure in power_result2")
}

# Create a data frame with extracted information
power_LPI_2015 <- data.frame(
  Group = Group_2015,
  Type = as.factor(ModelType_2015),
  Fixed_Effect = Test2.0_2015,
  Mean = mean_2015,
  Lower_CI = lower_CI_2015,
  Upper_CI = upper_CI_2015,
  stringsAsFactors = FALSE
)

# Create a new column that puts year/type and fixed effect together
power_LPI_2015$test2 <- paste(power_LPI_2015$Type, 
                              power_LPI_2015$Fixed_Effect, 
                              sep = "_")


# Create a csv file for the dataframe, so we don't have to rerun the power analysis
# write.csv(power_LPI_2015,
#           file = file.path("Data", "power_LPI_2015.csv"),
#           row.names = FALSE)



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ #




# POWER ANALYSIS FOR BY STATUS MODELS


# Extracting the fixed effects from the lupine model
fixef(m_Lupin_Status_2015)["TreatmentBurn:Trt_Statusafter"] <- 0.5
fixef(m_Lupin_Status_2015)["TreatmentMechanical:Trt_Statusafter"] <- 0.5
fixef(m_Lupin_Status_2015)["scale(yearly_rain)"] <- 0.5


# Extracting the fixed effects from the nativity model
fixef(m_Nativity_Status_2015)["TreatmentBurn:Trt_Statusafter"] <- 0.5
fixef(m_Nativity_Status_2015)["TreatmentMechanical:Trt_Statusafter"] <- 0.5
fixef(m_Nativity_Status_2015)["scale(yearly_rain)"] <- 0.5


# Extracting the fixed effects from the invasive model
fixef(m_Invasive_Status_2015)["TreatmentBurn:Trt_Statusafter"] <- 0.5
fixef(m_Invasive_Status_2015)["TreatmentMechanical:Trt_Statusafter"] <- 0.5
fixef(m_Invasive_Status_2015)["scale(yearly_rain)"] <- 0.5


# Extracting the fixed effects from the shrub model
fixef(m_Shrub_Status_2015)["TreatmentBurn:Trt_Statusafter"] <- 0.5
fixef(m_Shrub_Status_2015)["TreatmentMechanical:Trt_Statusafter"] <- 0.5
fixef(m_Shrub_Status_2015)["scale(yearly_rain)"] <- 0.5


# Set the number of simulations
Nsim <- 100

# Run the analysis
power_result_LPI_Status_2015 <- list(
  P2_Lupine_PrePost_BurnAfter = summary(powerSim(m_Lupin_Status_2015, nsim = Nsim, test=fixed("TreatmentBurn:Trt_Statusafter", "z"))),
  P2_Lupine_PrePost_MechAfter = summary(powerSim(m_Lupin_Status_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Trt_Statusafter", "z"))),
  P2_Lupine_PrePost_Rainfall = summary(powerSim(m_Lupin_Status_2015, nsim = Nsim, test=fixed("scale(yearly_rain)", "z"))),
  
  P2_Native_PrePost_BurnAfter= summary(powerSim(m_Nativity_Status_2015, nsim = Nsim, test=fixed("TreatmentBurn:Trt_Statusafter", "z"))),
  P2_Native_PrePost_MechAfter = summary(powerSim(m_Nativity_Status_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Trt_Statusafter", "z"))),
  P2_Native_PrePost_Rainfall = summary(powerSim(m_Nativity_Status_2015, nsim = Nsim, test=fixed("scale(yearly_rain)", "z"))),
  
  P2_Invasive_PrePost_BurnAfter = summary(powerSim(m_Invasive_Status_2015, nsim = Nsim, test=fixed("TreatmentBurn:Trt_Statusafter", "z"))),
  P2_Invasive_PrePost_MechAfter = summary(powerSim(m_Invasive_Status_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Trt_Statusafter", "z"))),
  P2_Invasive_PrePost_Rainfall = summary(powerSim(m_Invasive_Status_2015, nsim = Nsim, test=fixed("scale(yearly_rain)", "z"))),
  
  P2_Shrub_PrePost_BurnAfter = summary(powerSim(m_Shrub_Status_2015, nsim = Nsim, test=fixed("TreatmentBurn:Trt_Statusafter", "z"))),
  P2_Shrub_PrePost_MechAfter = summary(powerSim(m_Shrub_Status_2015, nsim = Nsim, test=fixed("TreatmentMechanical:Trt_Statusafter", "z"))),
  P2_Shrub_PrePost_Rainfall = summary(powerSim(m_Shrub_Status_2015, nsim = Nsim, test=fixed("scale(yearly_rain)", "z")))
  )
  

# CREATE A DATA FRAME WITH THE POWER RESULTS

# Save the model names from the power analysis
sim_name_LPI_Status_2015 <- names(power_result_LPI_Status_2015)

# Extract statistics while keeping them aligned
mean_s2015 <- sapply(power_result_LPI_Status_2015, function(x) x$mean)
lower_CI_s2015 <- sapply(power_result_LPI_Status_2015, function(x) x$lower)
upper_CI_s2015 <- sapply(power_result_LPI_Status_2015, function(x) x$upper)

# Split names into meaningful categories
split_names_status_2015 <- str_split(sim_name_LPI_Status_2015, "_", simplify = TRUE)

# Ensure split_names has enough columns to prevent indexing errors
if (ncol(split_names_status_2015) >= 4) {
  Group_s2015 <- split_names_status_2015[, 2]
  ModelType_s2015 <- split_names_status_2015[, 3]
  Test1.0_s2015 <- split_names_status_2015[, 4]
  Test2.0_s2015 <- gsub("\\..*", "", Test1.0_s2015)
} else {
  stop("Unexpected naming structure in power_result2")
}

# Create a data frame with extracted information
power_LPI_Status_2015 <- data.frame(
  Group = Group_s2015,
  Type = as.factor(ModelType_s2015),
  Fixed_Effect = Test2.0_s2015,
  Mean = mean_s2015,
  Lower_CI = lower_CI_s2015,
  Upper_CI = upper_CI_s2015,
  stringsAsFactors = FALSE
)


# Create a new column that puts year/type and fixed effect together
power_LPI_Status_2015$test2 <- paste(power_LPI_Status_2015$Type, 
                                     power_LPI_Status_2015$Fixed_Effect, 
                                     sep = "_")


# Create a csv file for the dataframe, so we don't have to rerun the power analysis
# write.csv(power_LPI_Status_2015,
#           file = file.path("Data", "power_LPI_Status_2015.csv"),
#           row.names = FALSE)



