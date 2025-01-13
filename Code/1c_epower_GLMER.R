#### Set up data for epower 
# FOR BEN: this is the file that the model is coming from
source("Code/2C_GLMER.R")

# Upload the data we will need
source("Code/1a_alldata.R")
source("Code/1c_Data_setup_GLMER.R")

# Install packages
library(lme4)
library(lmerTest) # a pimped-up version of lme4 which also provides pseudo-p-values.
library(MuMIn) # gives pseudo-R-squared via r.squaredGLMM()
library(pwr) # power analysis for lm
library(simr) # power analysis for generalized linear mixed models by simulation

# Power analysis with simr

#looking at the effect of treatment and treatment status
fixef(m2_Lupin_PrePost)["Trt_Statusafter"] <- 0.5  # Hypothetical effect size

# Run power analysis
power1 <- powerSim(m2_Lupin_PrePost, nsim=100, 
                  test=fixed("Trt_Statusafter", "z"))

# Summarize results
summary(power1)

#looking at the effect of treatment and treatment status
fixef(m2_Lupin_PrePost)["TreatmentBURN:Trt_Statusafter"] <- 0.5  # Hypothetical effect size

# Run power analysis
power2 <- powerSim(m2_Lupin_PrePost, nsim=100, 
                   test=fixed("TreatmentBURN:Trt_Statusafter ", "z"))

# Summarize results
summary(power)
