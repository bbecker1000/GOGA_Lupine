source("Code_with_2015/3b_TransectGLMER_ModelRun.R")

# Install packages
library(lme4)
library(lmerTest) 
library(MuMIn) 
library(pwr)
library(simr) 
library(dplyr)
library(stringr)


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
fixef(m_Nativity_Year_2015)["TreatmentBurn:Year2009"] <- 0.5
fixef(m_Nativity_Year_2015)["TreatmentMechanical:Year2009"] <- 0.5
fixef(m_Nativity_Year_2015)["TreatmentBurn:Year2011"] <- 0.5
fixef(m_Nativity_Year_2015)["TreatmentMechanical:Year2011"] <- 0.5
fixef(m_Nativity_Year_2015)["TreatmentBurn:Year2012"] <- 0.5
fixef(m_Nativity_Year_2015)["TreatmentMechanical:Year2012"] <- 0.5
fixef(m_Nativity_Year_2015)["TreatmentBurn:Year2013"] <- 0.5
fixef(m_Nativity_Year_2015)["TreatmentMechanical:Year2013"] <- 0.5
fixef(m_Nativity_Year_2015)["TreatmentBurn:Year2015"] <- 0.5
fixef(m_Nativity_Year_2015)["TreatmentMechanical:Year2015"] <- 0.5

# Extracting the fixed effects from the invasive model
fixef(m_Nativity_Year_2015)["TreatmentBurn:Year2009"] <- 0.5
fixef(m_Nativity_Year_2015)["TreatmentMechanical:Year2009"] <- 0.5
fixef(m_Nativity_Year_2015)["TreatmentBurn:Year2011"] <- 0.5
fixef(m_Nativity_Year_2015)["TreatmentMechanical:Year2011"] <- 0.5
fixef(m_Nativity_Year_2015)["TreatmentBurn:Year2012"] <- 0.5
fixef(m_Nativity_Year_2015)["TreatmentMechanical:Year2012"] <- 0.5
fixef(m_Nativity_Year_2015)["TreatmentBurn:Year2013"] <- 0.5
fixef(m_Nativity_Year_2015)["TreatmentMechanical:Year2013"] <- 0.5
fixef(m_Nativity_Year_2015)["TreatmentBurn:Year2015"] <- 0.5
fixef(m_Nativity_Year_2015)["TreatmentMechanical:Year2015"] <- 0.5

# Extracting the fixed effects from the shrub model
fixef(m_Shrub_Year_2015)["TreatmentBurn:Year2009"] <- 0.5
fixef(m_Shrub_Year_2015)["TreatmentMechanical:Year2009"] <- 0.5
fixef(m_Shrub_Year_2015)["TreatmentBurn:Year2011"] <- 0.5
fixef(m_Shrub_Year_2015)["TreatmentMechanical:Year2011"] <- 0.5
fixef(m_Shrub_Year_2015)["TreatmentBurn:Year2012"] <- 0.5
fixef(m_Shrub_Year_2015)["TreatmentMechanical:Year2012"] <- 0.5
fixef(m_Shrub_Year_2015)["TreatmentBurn:Year2013"] <- 0.5
fixef(m_Shrub_Year_2015)["TreatmentMechanical:Year2013"] <- 0.5
fixef(m_Shrub_Year_2015)["TreatmentBurn:Year2015"] <- 0.5
fixef(m_Shrub_Year_2015)["TreatmentMechanical:Year2015"] <- 0.5

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

