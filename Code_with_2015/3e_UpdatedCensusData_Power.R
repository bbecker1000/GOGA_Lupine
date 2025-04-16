source("Code_with_2015/3b_UpdatedCensusData_GLMER.R")

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
fixef(m_lupin_allyears_immature)["TreatmentBurn:Year2009"] <- 0.5
fixef(m_lupin_allyears_immature)["TreatmentMechanical:Year2009"] <- 0.5
fixef(m_lupin_allyears_immature)["TreatmentBurn:Year2011"] <- 0.5
fixef(m_lupin_allyears_immature)["TreatmentMechanical:Year2011"] <- 0.5
fixef(m_lupin_allyears_immature)["TreatmentBurn:Year2012"] <- 0.5
fixef(m_lupin_allyears_immature)["TreatmentMechanical:Year2012"] <- 0.5
fixef(m_lupin_allyears_immature)["TreatmentBurn:Year2013"] <- 0.5
fixef(m_lupin_allyears_immature)["TreatmentMechanical:Year2013"] <- 0.5
fixef(m_lupin_allyears_immature)["TreatmentBurn:Year2015"] <- 0.5
fixef(m_lupin_allyears_immature)["TreatmentMechanical:Year2015"] <- 0.5


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
  
  P2_Immature_2009_BurnBefore= summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentBurn:Year2009", "z"))),
  P2_Immature_2009_MechBefore = summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentMechanical:Year2009", "z"))),
  P2_Immature_2011_BurnAfter = summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentBurn:Year2011", "z"))),
  P2_Immature_2011_MechAfter = summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentMechanical:Year2011", "z"))),
  P2_Immature_2012_BurnAfter = summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentBurn:Year2012", "z"))),
  P2_Immature_2012_MechAfter = summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentMechanical:Year2012", "z"))),
  P2_Immature_2013_BurnAfter = summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentBurn:Year2013", "z"))),
  P2_Immature_2013_MechAfter = summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentMechanical:Year2013", "z"))),
  P2_Immature_2015_BurnAfter= summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentBurn:Year2015", "z"))),
  P2_Immature_2015_MechAfter = summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentMechanical:Year2015", "z"))))

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
  Type = ModelType_c2015,
  Fixed_Effect = Test2.0_c2015,
  Mean = mean_c2015,
  Lower_CI = lower_CI_c2015,
  Upper_CI = upper_CI_c2015,
  stringsAsFactors = FALSE
)


power_GLMER_2015 <- read_csv("Data/power_analysis_results.csv")

#power_with_census_2015 <- rbind(power_GLMER, power_census)


#write.csv(power_with_census, "power_with_census.csv", row.names = FALSE)


