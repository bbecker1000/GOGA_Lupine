# Upload the data we will need
source("Code/2C_GLMER.R")
source("Code/1c_Data_setup_GLMER.R")

# Install packages
library(lme4)
library(lmerTest) 
library(MuMIn) 
library(pwr)
library(simr) 
library(dplyr)
library(stringr)

doTest(m2_Lupin_PrePost, fcompare(~ Treatment + Trt_Status))

# POWER ANALYSIS FOR LUPINE LPI PRE/POST TREATMENT

# Extracting the fixed effects from the model pre/post model
fixef(m2_Lupin_PrePost)["TreatmentMECHANICAL:Trt_Statusafter"] <- 0.5
fixef(m2_Lupin_PrePost)["TreatmentBURN:Trt_Statusafter"] <- 0.5  
fixef(m2_Lupin_PrePost)["scale(yearly_rain)"] <- 0.5


# Run power analysis
P_Lupine_PrePost_MechAfter <- powerSim(m2_Lupin_PrePost, nsim=100, 
                   test=fixed("TreatmentMECHANICAL:Trt_Statusafter", "z"))

P_Lupine_PrePost_BurnAfter <- powerSim(m2_Lupin_PrePost, nsim=100, 
                   test=fixed("TreatmentBURN:Trt_Statusafter", "z"))

P_Lupine_PrePost_Rain <- powerSim(m2_Lupin_PrePost, nsim=100, 
                   test=fixed("scale(yearly_rain)", "z"))

sum_P_Lupine_PrePost_MechAfter <- summary(P_Lupine_PrePost_MechAfter)
sum_P_Lupine_PrePost_BurnAfter <- summary(P_Lupine_PrePost_BurnAfter)
sum_P_Lupine_PrePost_Rain <- summary(P_Lupine_PrePost_Rain)



# POWER ANALYSIS FOR LUPINE LPI YEARLY

# Extracting the fixed effects from the model 
fixef(m_Lupin_Year)["TreatmentBURN:as.factor(Year)2010"] <- 0.5
fixef(m_Lupin_Year)["TreatmentMECHANICAL:as.factor(Year)2010"] <- 0.5
fixef(m_Lupin_Year)["TreatmentBURN:as.factor(Year)2011"] <- 0.5
fixef(m_Lupin_Year)["TreatmentMECHANICAL:as.factor(Year)2011"] <- 0.5
fixef(m_Lupin_Year)["TreatmentBURN:as.factor(Year)2012"] <- 0.5
fixef(m_Lupin_Year)["TreatmentMECHANICAL:as.factor(Year)2012"] <- 0.5
fixef(m_Lupin_Year)["TreatmentBURN:as.factor(Year)2013"] <- 0.5
fixef(m_Lupin_Year)["TreatmentMECHANICAL:as.factor(Year)2013"] <- 0.5


# Run power analysis
P_Lupine_2010_BurnAfter <- powerSim(m_Lupin_Year, nsim=100, 
                        test=fixed("TreatmentBURN:as.factor(Year)2010", "z"))

summary(P_Lupine_2010_BurnAfter)

P_Lupine_2010_MechAfter <- powerSim(m_Lupin_Year, nsim=100, 
                        test=fixed("TreatmentMECHANICAL:as.factor(Year)2010", "z"))

summary(P_Lupine_2010_MechAfter)

P_Lupine_2011_BurnAfter <- powerSim(m_Lupin_Year, nsim=100, 
                        test=fixed("TreatmentBURN:as.factor(Year)2011", "z"))

summary(P_Lupine_2011_BurnAfter)

P_Lupine_2011_MechAfter <- powerSim(m_Lupin_Year, nsim=100, 
                        test=fixed("TreatmentMECHANICAL:as.factor(Year)2011", "z"))

summary(P_Lupine_2011_MechAfter)

P_Lupine_2012_BurnAfter <- powerSim(m_Lupin_Year, nsim=100, 
                        test=fixed("TreatmentBURN:as.factor(Year)2012", "z"))

summary(P_Lupine_2012_BurnAfter)

P_Lupine_2012_MechAfter <- powerSim(m_Lupin_Year, nsim=100, 
                        test=fixed("TreatmentMECHANICAL:as.factor(Year)2012", "z"))

summary(P_Lupine_2012_MechAfter)

P_Lupine_2013_BurnAfter <- powerSim(m_Lupin_Year, nsim=100, 
                        test=fixed("TreatmentBURN:as.factor(Year)2013", "z"))

summary(P_Lupine_2013_BurnAfter)

P_Lupine_2013_MechAfter <- powerSim(m_Lupin_Year, nsim=100, 
                        test=fixed("TreatmentMECHANICAL:as.factor(Year)2013", "z"))

summary(P_Lupine_2013_MechAfter)





# POWER ANALYSIS FOR NATIVE LPI PRE/POST TREATMENT

# Extracting the fixed effects from the model pre/post model
fixef(m2_Nativity_PrePost)["TreatmentMECHANICAL:Trt_Statusafter"] <- 0.5
fixef(m2_Nativity_PrePost)["TreatmentBURN:Trt_Statusafter"] <- 0.5  
fixef(m2_Nativity_PrePost)["scale(yearly_rain)"] <- 0.5


# Run power analysis
P_Native_PrePost_MechAfter <- powerSim(m2_Nativity_PrePost, nsim=100, 
                           test=fixed("TreatmentMECHANICAL:Trt_Statusafter", "z"))

summary(P_Native_PrePost_MechAfter)

P_Native_PrePost_BurnAfter <- powerSim(m2_Nativity_PrePost, nsim=100, 
                            test=fixed("TreatmentBURN:Trt_Statusafter", "z"))

summary(P_Native_PrePost_BurnAfter)

P_Native_PrePost_Rain <- powerSim(m2_Nativity_PrePost, nsim=100, 
                       test=fixed("scale(yearly_rain)", "z"))

summary(P_Native_PrePost_Rain)




# POWER ANALYSIS FOR NATIVE LPI PRE/POST TREATMENT

# Extracting the fixed effects from the model pre/post model
fixef(m2_Nativity_PrePost)["TreatmentMECHANICAL:Trt_Statusafter"] <- 0.5
fixef(m2_Nativity_PrePost)["TreatmentBURN:Trt_Statusafter"] <- 0.5  
fixef(m2_Nativity_PrePost)["scale(yearly_rain)"] <- 0.5


# Run power analysis
P_Native_PrePost_MechAfter <- powerSim(m2_Nativity_PrePost, nsim=100, 
                                       test=fixed("TreatmentMECHANICAL:Trt_Statusafter", "z"))

summary(P_Native_PrePost_MechAfter)

P_Native_PrePost_BurnAfter <- powerSim(m2_Nativity_PrePost, nsim=100, 
                                       test=fixed("TreatmentBURN:Trt_Statusafter", "z"))

summary(P_Native_PrePost_BurnAfter)

P_Native_PrePost_Rain <- powerSim(m2_Nativity_PrePost, nsim=100, 
                                  test=fixed("scale(yearly_rain)", "z"))

summary(P_Native_PrePost_Rain)




# POWER ANALYSIS FOR NATIVE LPI YEARLY

# Extracting the fixed effects from the model
fixef(m_Nativity_Year)["TreatmentBURN:as.factor(Year)2010"] <- 0.5
fixef(m_Nativity_Year)["TreatmentMECHANICAL:as.factor(Year)2010"] <- 0.5
fixef(m_Nativity_Year)["TreatmentBURN:as.factor(Year)2011"] <- 0.5
fixef(m_Nativity_Year)["TreatmentMECHANICAL:as.factor(Year)2011"] <- 0.5
fixef(m_Nativity_Year)["TreatmentBURN:as.factor(Year)2012"] <- 0.5
fixef(m_Nativity_Year)["TreatmentMECHANICAL:as.factor(Year)2012"] <- 0.5
fixef(m_Nativity_Year)["TreatmentBURN:as.factor(Year)2013"] <- 0.5
fixef(m_Nativity_Year)["TreatmentMECHANICAL:as.factor(Year)2013"] <- 0.5


# Run power analysis
P_Native_2010_BurnAfter <- powerSim(m_Nativity_Year, nsim=100, 
                        test=fixed("TreatmentBURN:as.factor(Year)2010", "z"))

summary(P_Native_2010_BurnAfter)

P_Native_2010_MechAfter <- powerSim(m_Nativity_Year, nsim=100, 
                        test=fixed("TreatmentMECHANICAL:as.factor(Year)2010", "z"))

summary(P_Native_2010_MechAfter)

P_Native_2011_BurnAfter <- powerSim(m_Nativity_Year, nsim=100, 
                        test=fixed("TreatmentBURN:as.factor(Year)2011", "z"))

summary(P_Native_2011_BurnAfter)

P_Native_2011_MechAfter <- powerSim(m_Nativity_Year, nsim=100, 
                        test=fixed("TreatmentMECHANICAL:as.factor(Year)2011", "z"))

summary(P_Native_2011_MechAfter)

P_Native_2012_BurnAfter <- powerSim(m_Nativity_Year, nsim=100, 
                        test=fixed("TreatmentBURN:as.factor(Year)2012", "z"))

summary(P_Native_2012_BurnAfter)

P_Native_2012_MechAfter <- powerSim(m_Nativity_Year, nsim=100, 
                        test=fixed("TreatmentMECHANICAL:as.factor(Year)2012", "z"))

summary(P_Native_2012_MechAfter)

P_Native_2013_BurnAfter <- powerSim(m_Nativity_Year, nsim=100, 
                        test=fixed("TreatmentBURN:as.factor(Year)2013", "z"))

summary(P_Native_2013_BurnAfter)

P_Native_2013_MechAfter <- powerSim(m_Nativity_Year, nsim=100, 
                        test=fixed("TreatmentMECHANICAL:as.factor(Year)2013", "z"))

summary(P_Native_2013_MechAfter)




# POWER ANALYSIS FOR INVASIVE LPI PRE/POST TREATMENT

# Extracting the fixed effects from the model pre/post model
fixef(m2_Invasive_PrePost)["TreatmentMECHANICAL:Trt_Statusafter"] <- 0.5
fixef(m2_Invasive_PrePost)["TreatmentBURN:Trt_Statusafter"] <- 0.5  
fixef(m2_Invasive_PrePost)["scale(yearly_rain)"] <- 0.5


# Run power analysis
P_Invasive_PrePost_MechAfter <- powerSim(m2_Invasive_PrePost, nsim=100, 
                             test=fixed("TreatmentMECHANICAL:Trt_Statusafter", "z"))

summary(P_Invasive_PrePost_MechAfter)

P_Invasive_PrePost_BurnAfter <- powerSim(m2_Invasive_PrePost, nsim=100, 
                             test=fixed("TreatmentBURN:Trt_Statusafter", "z"))

summary(P_Invasive_PrePost_BurnAfter)

P_Invasive_PrePost_Rain <- powerSim(m2_Invasive_PrePost, nsim=100, 
                        test=fixed("scale(yearly_rain)", "z"))

summary(P_Invasive_PrePost_Rain)




# POWER ANALYSIS FOR INVASIVE LPI YEARLY

# Extracting the fixed effects from the model 
fixef(m_Invasive_Year)["TreatmentBURN:as.factor(Year)2010"] <- 0.5
fixef(m_Invasive_Year)["TreatmentMECHANICAL:as.factor(Year)2010"] <- 0.5
fixef(m_Invasive_Year)["TreatmentBURN:as.factor(Year)2011"] <- 0.5
fixef(m_Invasive_Year)["TreatmentMECHANICAL:as.factor(Year)2011"] <- 0.5
fixef(m_Invasive_Year)["TreatmentBURN:as.factor(Year)2012"] <- 0.5
fixef(m_Invasive_Year)["TreatmentMECHANICAL:as.factor(Year)2012"] <- 0.5
fixef(m_Invasive_Year)["TreatmentBURN:as.factor(Year)2013"] <- 0.5
fixef(m_Invasive_Year)["TreatmentMECHANICAL:as.factor(Year)2013"] <- 0.5


# Run power analysis
P_Invasive_2010_BurnAfter <- powerSim(m_Invasive_Year, nsim=100, 
                        test=fixed("TreatmentBURN:as.factor(Year)2010", "z"))

summary(P_Invasive_2010_BurnAfter)

P_Invasive_2010_MechAfter <- powerSim(m_Invasive_Year, nsim=100, 
                         test=fixed("TreatmentMECHANICAL:as.factor(Year)2010", "z"))

summary(P_Invasive_2010_MechAfter)

P_Invasive_2011_BurnAfter <- powerSim(m_Invasive_Year, nsim=100, 
                        test=fixed("TreatmentBURN:as.factor(Year)2011", "z"))

summary(P_Invasive_2011_BurnAfter)

P_Invasive_2011_MechAfter <- powerSim(m_Invasive_Year, nsim=100, 
                        test=fixed("TreatmentMECHANICAL:as.factor(Year)2011", "z"))

summary(P_Invasive_2011_MechAfter)

P_Invasive_2012_BurnAfter <- powerSim(m_Invasive_Year, nsim=100, 
                        test=fixed("TreatmentBURN:as.factor(Year)2012", "z"))

summary(P_Invasive_2012_BurnAfter)

P_Invasive_2012_MechAfter <- powerSim(m_Invasive_Year, nsim=100, 
                        test=fixed("TreatmentMECHANICAL:as.factor(Year)2012", "z"))

summary(P_Invasive_2012_MechAfter)

P_Invasive_2013_BurnAfter <- powerSim(m_Invasive_Year, nsim=100, 
                        test=fixed("TreatmentBURN:as.factor(Year)2013", "z"))

summary(P_Invasive_2013_BurnAfter)

P_Invasive_2013_MechAfter <- powerSim(m_Invasive_Year, nsim=100, 
                        test=fixed("TreatmentMECHANICAL:as.factor(Year)2013", "z"))

summary(P_Invasive_2013_MechAfter)



# POWER ANALYSIS FOR SHRUB LPI PRE/POST TREATMENT

# Extracting the fixed effects from the model pre/post model
fixef(m2_Shrub_PrePost)["TreatmentMECHANICAL:Trt_Statusafter"] <- 0.5
fixef(m2_Shrub_PrePost)["TreatmentBURN:Trt_Statusafter"] <- 0.5  
fixef(m2_Shrub_PrePost)["scale(yearly_rain)"] <- 0.5


# Run power analysis
P_Shrub_PrePost_MechAfter <- powerSim(m2_Shrub_PrePost, nsim=100, 
                             test=fixed("TreatmentMECHANICAL:Trt_Statusafter", "z"))

summary(P_Shrub_PrePost_MechAfter)

P_Shrub_PrePost_BurnAfter <- powerSim(m2_Shrub_PrePost, nsim=100, 
                             test=fixed("TreatmentBURN:Trt_Statusafter", "z"))

summary(P_Shrub_PrePost_BurnAfter)

P_Shrub_PrePost_Rain <- powerSim(m2_Shrub_PrePost, nsim=100, 
                        test=fixed("scale(yearly_rain)", "z"))

summary(P_Shrub_PrePost_Rain)



# POWER ANALYSIS FOR SHRUB LPI YEARLY

# Extracting the fixed effects from the model pre/post model
fixef(m_Shrub_Year)["TreatmentBURN:as.factor(Year)2010"] <- 0.5
fixef(m_Shrub_Year)["TreatmentMECHANICAL:as.factor(Year)2010"] <- 0.5
fixef(m_Shrub_Year)["TreatmentBURN:as.factor(Year)2011"] <- 0.5
fixef(m_Shrub_Year)["TreatmentMECHANICAL:as.factor(Year)2011"] <- 0.5
fixef(m_Shrub_Year)["TreatmentBURN:as.factor(Year)2012"] <- 0.5
fixef(m_Shrub_Year)["TreatmentMECHANICAL:as.factor(Year)2012"] <- 0.5
fixef(m_Shrub_Year)["TreatmentBURN:as.factor(Year)2013"] <- 0.5
fixef(m_Shrub_Year)["TreatmentMECHANICAL:as.factor(Year)2013"] <- 0.5


# Run power analysis
P_Shrub_2010_BurnAfter <- powerSim(m_Shrub_Year, nsim=100, 
                       test=fixed("TreatmentBURN:as.factor(Year)2010", "z"))

summary(P_Shrub_2010_BurnAfter)

P_Shrub_2010_MechAfter <- powerSim(m_Shrub_Year, nsim=100, 
                       test=fixed("TreatmentMECHANICAL:as.factor(Year)2010", "z"))

summary(P_Shrub_2010_MechAfter)

P_Shrub_2011_BurnAfter <- powerSim(m_Shrub_Year, nsim=100, 
                       test=fixed("TreatmentBURN:as.factor(Year)2011", "z"))

summary(P_Shrub_2011_BurnAfter)

P_Shrub_2011_MechAfter <- powerSim(m_Shrub_Year, nsim=100, 
                       test=fixed("TreatmentMECHANICAL:as.factor(Year)2011", "z"))

summary(P_Shrub_2011_MechAfter)

P_Shrub_2012_BurnAfter <- powerSim(m_Shrub_Year, nsim=100, 
                       test=fixed("TreatmentBURN:as.factor(Year)2012", "z"))

summary(P_Shrub_2012_BurnAfter)

P_Shrub_2012_MechAfter <- powerSim(m_Shrub_Year, nsim=100, 
                       test=fixed("TreatmentMECHANICAL:as.factor(Year)2012", "z"))

summary(P_Shrub_2012_MechAfter)

P_Shrub_2013_BurnAfter <- powerSim(m_Shrub_Year, nsim=100, 
                       test=fixed("TreatmentBURN:as.factor(Year)2013", "z"))

summary(P_Shrub_2013_BurnAfter)

P_Shrub_2013_MechAfter <- powerSim(m_Shrub_Year, nsim=100, 
                       test=fixed("TreatmentMECHANICAL:as.factor(Year)2013", "z"))

summary(P_Shrub_2013_MechAfter)

