source("Code_with_2015/4b_UpdatedCensusData_GLMER.R")
library(sjPlot)

# # DATA DISTRIBUTION # #

# Plot COUNT DATA in a histogram to determine distribution
hist(Lupine_Density_2009_2015_grouped_live$Count)

# Plot RATIO DATA in a histogram to determine distribution
hist(Lupin_Ratio_2009_2015$Ratio_I_M)



# # LUPINE CENSUS NEGATIVE BINOMIAL BY YEAR # #

# Plot residuals with negative binomial 
plot_model(m_Lupin_census_allyears_nb,type = "diag")
plot(m_Lupin_census_allyears_nb)

# Plot basic view of model
plot_model(m_Lupin_census_allyears_nb, type = "eff", 
           terms = c("Year", "Treatment")) +
  theme_classic()

plot_model(m_Lupin_census_allyears_nb)



# # PERCENT IMMATURE LUPINE BINOMIAL BY YEAR # #

# Plot residuals with negative binomial 
plot_model(m_lupin_allyears_immature,type = "diag")
plot(m_lupin_allyears_immature)

# Plot basic view of model
plot_model(m_lupin_allyears_immature, type = "eff", 
           terms = c("Year", "Treatment")) +
  theme_classic()

