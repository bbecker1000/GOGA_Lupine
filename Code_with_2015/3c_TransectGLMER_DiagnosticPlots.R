source("Code_with_2015/3b_UpdatedCensusData_GLMER.R")
library(sjPlot)

# # DATA DISTRIBUTION # #

# Plot LUPINE DATA in a histogram to determine distribution
hist(Lupin_data_2015_2$Lupin_Ratio)

# Plot NATIVITY DATA in a histogram to determine distribution
hist(Nativity_data_2015_2$Native_Ratio)

# Plot INVASIVE DATA in a histogram to determine distribution
hist(Invasive_data_2015_2$Invasive_Ratio)

# Plot SHRUB DATA in a histogram to determine distribution
hist(Shrub_data_2015_2$Shrub_Ratio)


# # LUPINE COVER BINOMIAL BY YEAR # #

# Plot residuals with negative binomial 
plot_model(m_Lupin_Year_2015,type = "diag")
plot(m_Lupin_Year_2015)

# Plot basic view of model
plot_model(m_Lupin_Year_2015, type = "eff", 
           terms = c("Year", "Treatment")) +
  theme_classic()

plot_model(m_Lupin_Year_2015)



# # NATIVE COVER BINOMIAL BY YEAR # #

# Plot residuals with negative binomial 
plot_model(m_Nativity_Year_2015,type = "diag")
plot(m_Nativity_Year_2015)

# Plot basic view of model
plot_model(m_Nativity_Year_2015, type = "eff", 
           terms = c("Year", "Treatment")) +
  theme_classic()

plot_model(m_Nativity_Year_2015)


# # INVASIVE COVER BINOMIAL BY YEAR # #

# Plot residuals with negative binomial 
plot_model(m_Invasive_Year_2015,type = "diag")
plot(m_Invasive_Year_2015)

# Plot basic view of model
plot_model(m_Invasive_Year_2015, type = "eff", 
           terms = c("Year", "Treatment")) +
  theme_classic()

plot_model(m_Invasive_Year_2015)


# # SHRUB COVER BINOMIAL BY YEAR # #

# Plot residuals with negative binomial 
plot_model(m_Shrub_Year_2015,type = "diag")
plot(m_Shrub_Year_2015)

# Plot basic view of model
plot_model(m_Shrub_Year_2015, type = "eff", 
           terms = c("Year", "Treatment")) +
  theme_classic()

plot_model(m_Shrub_Year_2015)
