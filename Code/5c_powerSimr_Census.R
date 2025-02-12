source("Code/2d_Census_GLMER.R")

# Install packages
library(lme4)
library(lmerTest) 
library(MuMIn) 
library(pwr)
library(simr) 
library(dplyr)
library(stringr)

# Extracting the fixed effects from the model pre/post model
fixef(m_Lupin_census_nb_PrePost)["TreatmentM:Trt_Statusafter"] <- 0.5
fixef(m_Lupin_census_nb_PrePost)["TreatmentB:Trt_Statusafter"] <- 0.5  

fixef(m_Lupin_census_nb)["TreatmentB:as.factor(Year)2011"] <- 0.5
fixef(m_Lupin_census_nb)["TreatmentM:as.factor(Year)2011"] <- 0.5
fixef(m_Lupin_census_nb)["TreatmentB:as.factor(Year)2012"] <- 0.5
fixef(m_Lupin_census_nb)["TreatmentM:as.factor(Year)2012"] <- 0.5
fixef(m_Lupin_census_nb)["TreatmentB:as.factor(Year)2013"] <- 0.5
fixef(m_Lupin_census_nb)["TreatmentM:as.factor(Year)2013"] <- 0.5

Nsim <- 100

power_result_c <- list(
    P_Census_PrePost_MechAfter = summary(powerSim(m_Lupin_census_nb_PrePost, nsim = Nsim, test = fixed("TreatmentM:Trt_Statusafter", "z"))),
    P_Census_PrePost_BurnAfter = summary(powerSim(m_Lupin_census_nb_PrePost, nsim = Nsim, test = fixed("TreatmentB:Trt_Statusafter", "z"))),

    P_Census_2011_BurnAfter = summary(powerSim(m_Lupin_census_nb, nsim = Nsim, test=fixed("TreatmentB:as.factor(Year)2011", "z"))),
    P_Census_2011_MechAfter = summary(powerSim(m_Lupin_census_nb, nsim = Nsim, test=fixed("TreatmentM:as.factor(Year)2011", "z"))),
    P_Census_2012_BurnAfter = summary(powerSim(m_Lupin_census_nb, nsim = Nsim, test=fixed("TreatmentB:as.factor(Year)2012", "z"))),
    P_Census_2012_MechAfter = summary(powerSim(m_Lupin_census_nb, nsim = Nsim, test=fixed("TreatmentM:as.factor(Year)2012", "z"))),
    P_Census_2013_BurnAfter = summary(powerSim(m_Lupin_census_nb, nsim = Nsim, test=fixed("TreatmentB:as.factor(Year)2013", "z"))),
    P_Census_2013_MechAfter = summary(powerSim(m_Lupin_census_nb, nsim = Nsim, test=fixed("TreatmentM:as.factor(Year)2013", "z")))
    )

# Extract names for grouping
sim_name3 <- names(power_result_c)

# Extract statistics while keeping them aligned
mean3 <- sapply(power_result_c, function(x) x$mean)
lower_CI3 <- sapply(power_result_c, function(x) x$lower)
upper_CI3 <- sapply(power_result_c, function(x) x$upper)

# Split names into meaningful categories
split_names3 <- str_split(sim_name3, "_", simplify = TRUE)

# Ensure split_names has enough columns to prevent indexing errors
if (ncol(split_names3) >= 4) {
  Group3 <- split_names3[, 2]
  ModelType3 <- split_names3[, 3]
  Test1.0 <- split_names3[, 4]
  Test2.0 <- gsub("\\..*", "", Test1.0)
} else {
  stop("Unexpected naming structure in power_result2")
}

# Create a data frame with extracted information
power_census <- data.frame(
  Group = Group3,
  Type = ModelType3,
  Fixed_Effect = Test2.0,
  Mean = mean3,
  Lower_CI = lower_CI3,
  Upper_CI = upper_CI3,
  stringsAsFactors = FALSE
)


power_GLMER <- read_csv("Data/power_analysis_results.csv")
power_with_census <- rbind(power_GLMER, power_census)

#write.csv(power_with_census, "power_with_census.csv", row.names = FALSE)
