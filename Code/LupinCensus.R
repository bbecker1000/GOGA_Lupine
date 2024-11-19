#Load Packages
library(tidyverse)
library(lme4)
library(sjPlot)

#Upload data
Lupin_Census <- read_csv("Data/Lupine_Combined_AllYears.csv")

# Separate our MacroPlot so you have columns with the Plot and Site
Lupin_Census$Plot <- str_extract(Lupin_Census$MacroPlot, "\\d+")
Lupin_Census$Site = str_extract(Lupin_Census$MacroPlot, "[^_]+$")

# Make year a factor
Lupin_Census$Year = as.factor(Lupin_Census$Year)

# Set control as base level
Lupin_Census$Treatment <- factor(Lupin_Census$Treatment, 
                                levels = c("C", "B", "M"))

# Take a preliminary look at the data
ggplot(Lupin_Census, aes(x=as.factor(Year), y = RowCount, color = Treatment)) +
  geom_boxplot() +
  geom_point(alpha = 0.25)

# Plot all data in a histogram to determine distribution
hist(Lupin_Census$RowCount)
# Based on output maybe Poisson or Negative binomial

# Run a GLMER on the Lupine census data with Poisson
m_Lupin_census_poisson <- glmer(RowCount ~ Treatment + 
                                    Year + 
                                    (1|Site/Plot),
                                    family = poisson, 
                                    data = Lupin_Census)

summary(m_Lupin_census_poisson)

# Plot residuals for Poisson
plot_model(m_Lupin_census_poisson,type = "diag")


# Run a GLMER on the Lupine census data with negative binomial
m_Lupin_census_nb <- glmer.nb(RowCount ~ Treatment + 
                          Year + 
                          (1|Site/Plot),
                        data = Lupin_Census)

summary(m_Lupin_census_nb)

# Plot residuals with negative binomial
plot_model(m_Lupin_census_nb,type = "diag")


