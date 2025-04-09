source("Code_with_2015/1b_UpdatedCensusData_GLMER.R")

# Install packages
library(lme4)
library(lmerTest) 
library(MuMIn) 
library(pwr)
library(simr) 
library(dplyr)
library(stringr)


# Extracting the fixed effects from the yearly model
fixef(m_Lupin_census_allyears_nb)["TreatmentBurn:as.factor(Year)2009"] <- 0.5
fixef(m_Lupin_census_allyears_nb)["TreatmentMechanical:as.factor(Year)2009"] <- 0.5
fixef(m_Lupin_census_allyears_nb)["TreatmentBurn:as.factor(Year)2011"] <- 0.5
fixef(m_Lupin_census_allyears_nb)["TreatmentMechanical:as.factor(Year)2011"] <- 0.5
fixef(m_Lupin_census_allyears_nb)["TreatmentBurn:as.factor(Year)2012"] <- 0.5
fixef(m_Lupin_census_allyears_nb)["TreatmentMechanical:as.factor(Year)2012"] <- 0.5
fixef(m_Lupin_census_allyears_nb)["TreatmentBurn:as.factor(Year)2013"] <- 0.5
fixef(m_Lupin_census_allyears_nb)["TreatmentMechanical:as.factor(Year)2013"] <- 0.5
fixef(m_Lupin_census_allyears_nb)["TreatmentBurn:as.factor(Year)2015"] <- 0.5
fixef(m_Lupin_census_allyears_nb)["TreatmentMechanical:as.factor(Year)2015"] <- 0.5

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
  P2_Census_2009_BurnAfter = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentBurn:as.factor(Year)2009", "z"))),
  P2_Census_2009_MechAfter = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentMechanical:as.factor(Year)2009", "z"))),
  P2_Census_2011_BurnAfter = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentBurn:as.factor(Year)2011", "z"))),
  P2_Census_2011_MechAfter = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentMechanical:as.factor(Year)2011", "z"))),
  P2_Census_2012_BurnAfter = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentBurn:as.factor(Year)2012", "z"))),
  P2_Census_2012_MechAfter = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentMechanical:as.factor(Year)2012", "z"))),
  P2_Census_2013_BurnAfter = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentBurn:as.factor(Year)2013", "z"))),
  P2_Census_2013_MechAfter = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentMechanical:as.factor(Year)2013", "z"))),
  P2_Census_2015_BurnAfter = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentBurn:as.factor(Year)2015", "z"))),
  P2_Census_2015_MechAfter = summary(powerSim(m_Lupin_census_allyears_nb, nsim = Nsim, test=fixed("TreatmentMechanical:as.factor(Year)2015", "z"))),
  
  P2_Immature_2009_BurnAfter= summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentBurn:Year2009", "z"))),
  P2_Immature_2009_MechAfter = summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentMechanical:Year2009", "z"))),
  P2_Immature_2011_BurnAfter= summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentBurn:Year2011", "z"))),
  P2_Immature_2011_MechAfter = summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentMechanical:Year2011", "z"))),
  P2_Immature_2012_BurnAfter = summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentBurn:Year2012", "z"))),
  P2_Immature_2012_MechAfter = summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentMechanical:Year2012", "z"))),
  P2_Immature_2013_BurnAfter = summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentBurn:Year2013", "z"))),
  P2_Immature_2013_MechAfter = summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentMechanical:Year2013", "z"))),
  P2_Immature_2015_BurnAfter= summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentBurn:Year2015", "z"))),
  P2_Immature_2015_MechAfter = summary(powerSim(m_lupin_allyears_immature, nsim = Nsim, test=fixed("TreatmentMechanical:Year2015", "z"))),
  )
