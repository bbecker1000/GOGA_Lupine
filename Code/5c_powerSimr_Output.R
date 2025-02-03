
# Compute power analysis results
power_result2 <- list(
  P_Lupine_PrePost_MechAfter = summary(powerSim(m2_Lupin_PrePost, nsim = Nsim, test = fixed("TreatmentMECHANICAL:Trt_Statusafter", "z"))),
  P_Lupine_PrePost_BurnAfter = summary(powerSim(m2_Lupin_PrePost, nsim = Nsim, test = fixed("TreatmentBURN:Trt_Statusafter", "z"))),
  P_Lupine_PrePost_Rain = summary(powerSim(m2_Lupin_PrePost, nsim = Nsim, test = fixed("scale(yearly_rain)", "z"))),
  
  P_Lupine_2010_BurnAfter = summary(powerSim(m_Lupin_Year, nsim = Nsim, test=fixed("TreatmentBURN:as.factor(Year)2010", "z"))),
  P_Lupine_2010_MechAfter = summary(powerSim(m_Lupin_Year, nsim = Nsim, test=fixed("TreatmentMECHANICAL:as.factor(Year)2010", "z"))),
  P_Lupine_2011_BurnAfter = summary(powerSim(m_Lupin_Year, nsim = Nsim, test=fixed("TreatmentBURN:as.factor(Year)2011", "z"))),
  P_Lupine_2011_MechAfter = summary(powerSim(m_Lupin_Year, nsim = Nsim, test=fixed("TreatmentMECHANICAL:as.factor(Year)2011", "z"))),
  P_Lupine_2012_BurnAfter = summary(powerSim(m_Lupin_Year, nsim = Nsim, test=fixed("TreatmentBURN:as.factor(Year)2012", "z"))),
  P_Lupine_2012_MechAfter = summary(powerSim(m_Lupin_Year, nsim = Nsim, test=fixed("TreatmentMECHANICAL:as.factor(Year)2012", "z"))),
  P_Lupine_2013_BurnAfter = summary(powerSim(m_Lupin_Year, nsim = Nsim, test=fixed("TreatmentBURN:as.factor(Year)2013", "z"))),
  P_Lupine_2013_MechAfter = summary(powerSim(m_Lupin_Year, nsim = Nsim, test=fixed("TreatmentMECHANICAL:as.factor(Year)2013", "z"))),
  
  P_Native_PrePost_MechAfter = summary(powerSim(m2_Nativity_PrePost, nsim = Nsim, test=fixed("TreatmentMECHANICAL:Trt_Statusafter", "z"))),
  P_Native_PrePost_BurnAfter = summary(powerSim(m2_Nativity_PrePost, nsim = Nsim, test=fixed("TreatmentBURN:Trt_Statusafter", "z"))),
  P_Native_PrePost_Rain = summary(powerSim(m2_Nativity_PrePost, nsim = Nsim, test=fixed("scale(yearly_rain)", "z"))),
  
  P_Native_PrePost_MechAfter = summary(powerSim(m2_Nativity_PrePost, nsim = Nsim, test=fixed("TreatmentMECHANICAL:Trt_Statusafter", "z"))),
  P_Native_PrePost_BurnAfter = summary(powerSim(m2_Nativity_PrePost, nsim = Nsim, test=fixed("TreatmentBURN:Trt_Statusafter", "z"))),
  P_Native_PrePost_Rain = summary(powerSim(m2_Nativity_PrePost, nsim = Nsim, test=fixed("scale(yearly_rain)", "z"))),

  P_Native_2010_BurnAfter = summary(powerSim(m_Nativity_Year, nsim = Nsim, test=fixed("TreatmentBURN:as.factor(Year)2010", "z"))),
  P_Native_2010_MechAfter = summary(powerSim(m_Nativity_Year, nsim = Nsim, test=fixed("TreatmentMECHANICAL:as.factor(Year)2010", "z"))),
  P_Native_2011_BurnAfter = summary(powerSim(m_Nativity_Year, nsim = Nsim, test=fixed("TreatmentBURN:as.factor(Year)2011", "z"))),
  P_Native_2011_MechAfter = summary(powerSim(m_Nativity_Year, nsim = Nsim, test=fixed("TreatmentMECHANICAL:as.factor(Year)2011", "z"))),
  P_Native_2012_BurnAfter = summary(powerSim(m_Nativity_Year, nsim = Nsim, test=fixed("TreatmentBURN:as.factor(Year)2012", "z"))),
  P_Native_2012_MechAfter = summary(powerSim(m_Nativity_Year, nsim = Nsim, test=fixed("TreatmentMECHANICAL:as.factor(Year)2012", "z"))),
  P_Native_2013_BurnAfter = summary(powerSim(m_Nativity_Year, nsim = Nsim, test=fixed("TreatmentBURN:as.factor(Year)2013", "z"))),
  P_Native_2013_MechAfter = summary(powerSim(m_Nativity_Year, nsim = Nsim, test=fixed("TreatmentMECHANICAL:as.factor(Year)2013", "z"))),
  
  P_Invasive_PrePost_MechAfter = summary(powerSim(m2_Invasive_PrePost, nsim = Nsim, test=fixed("TreatmentMECHANICAL:Trt_Statusafter", "z"))),
  P_Invasive_PrePost_BurnAfter = summary(powerSim(m2_Invasive_PrePost, nsim = Nsim, test=fixed("TreatmentBURN:Trt_Statusafter", "z"))),
  P_Invasive_PrePost_Rain = summary(powerSim(m2_Invasive_PrePost, nsim = Nsim, test=fixed("scale(yearly_rain)", "z"))),
  
  P_Invasive_2010_BurnAfter = summary(powerSim(m_Invasive_Year, nsim = Nsim, test=fixed("TreatmentBURN:as.factor(Year)2010", "z"))),
  P_Invasive_2010_MechAfter = summary(powerSim(m_Invasive_Year, nsim = Nsim, test=fixed("TreatmentMECHANICAL:as.factor(Year)2010", "z"))),
  P_Invasive_2011_BurnAfter = summary(powerSim(m_Invasive_Year, nsim = Nsim, test=fixed("TreatmentBURN:as.factor(Year)2011", "z"))),
  P_Invasive_2011_MechAfter = summary(powerSim(m_Invasive_Year, nsim = Nsim, test=fixed("TreatmentMECHANICAL:as.factor(Year)2011", "z"))),
  P_Invasive_2012_BurnAfter = summary(powerSim(m_Invasive_Year, nsim = Nsim, test=fixed("TreatmentBURN:as.factor(Year)2012", "z"))),
  P_Invasive_2012_MechAfter = summary(powerSim(m_Invasive_Year, nsim = Nsim, test=fixed("TreatmentMECHANICAL:as.factor(Year)2012", "z"))),
  P_Invasive_2013_BurnAfter = summary(powerSim(m_Invasive_Year, nsim = Nsim, test=fixed("TreatmentBURN:as.factor(Year)2013", "z"))),
  P_Invasive_2013_MechAfter = summary(powerSim(m_Invasive_Year, nsim = Nsim, test=fixed("TreatmentMECHANICAL:as.factor(Year)2013", "z"))),
  
  P_Shrub_PrePost_MechAfter = summary(powerSim(m2_Shrub_PrePost, nsim = Nsim, test=fixed("TreatmentMECHANICAL:Trt_Statusafter", "z"))),
  P_Shrub_PrePost_BurnAfter = summary(powerSim(m2_Shrub_PrePost, nsim = Nsim, test=fixed("TreatmentBURN:Trt_Statusafter", "z"))),
  P_Shrub_PrePost_Rain = summary(powerSim(m2_Shrub_PrePost, nsim = Nsim, test=fixed("scale(yearly_rain)", "z"))),
  
  P_Shrub_2010_BurnAfter = summary(powerSim(m_Shrub_Year, nsim = Nsim, test=fixed("TreatmentBURN:as.factor(Year)2010", "z"))),
  P_Shrub_2010_MechAfter = summary(powerSim(m_Shrub_Year, nsim = Nsim, test=fixed("TreatmentMECHANICAL:as.factor(Year)2010", "z"))),
  P_Shrub_2011_BurnAfter = summary(powerSim(m_Shrub_Year, nsim = Nsim, test=fixed("TreatmentBURN:as.factor(Year)2011", "z"))),
  P_Shrub_2011_MechAfter = summary(powerSim(m_Shrub_Year, nsim = Nsim, test=fixed("TreatmentMECHANICAL:as.factor(Year)2011", "z"))),
  P_Shrub_2012_BurnAfter = summary(powerSim(m_Shrub_Year, nsim = Nsim, test=fixed("TreatmentBURN:as.factor(Year)2012", "z"))),
  P_Shrub_2012_MechAfter = summary(powerSim(m_Shrub_Year, nsim = Nsim, test=fixed("TreatmentMECHANICAL:as.factor(Year)2012", "z"))),
  P_Shrub_2013_BurnAfter = summary(powerSim(m_Shrub_Year, nsim = Nsim, test=fixed("TreatmentBURN:as.factor(Year)2013", "z"))),
  P_Shrub_2013_MechAfter = summary(powerSim(m_Shrub_Year, nsim = Nsim, test=fixed("TreatmentMECHANICAL:as.factor(Year)2013", "z")))
  )

# Extract names for grouping
sim_name2 <- names(power_result2)

# Extract statistics while keeping them aligned
mean <- sapply(power_result2, function(x) x$mean)
lower_CI <- sapply(power_result2, function(x) x$lower)
upper_CI <- sapply(power_result2, function(x) x$upper)

# Split names into meaningful categories
split_names <- str_split(sim_name2, "_", simplify = TRUE)

# Ensure split_names has enough columns to prevent indexing errors
if (ncol(split_names) >= 4) {
  Group <- split_names[, 2]
  ModelType <- split_names[, 3]
  Test1 <- split_names[, 4]
  Test2 <- gsub("\\..*", "", Test1)
} else {
  stop("Unexpected naming structure in power_result2")
}

# Create a data frame with extracted information
power_output <- data.frame(
  Group = Group,
  Type = ModelType,
  Fixed_Effect = Test2,
  Mean = mean,
  Lower_CI = lower_CI,
  Upper_CI = upper_CI,
  stringsAsFactors = FALSE
)

# Remove duplicate rows (if needed)
power_df <- unique(w)

view(power_df)

write.csv(power_df, "power_analysis_results.csv", row.names = FALSE)



