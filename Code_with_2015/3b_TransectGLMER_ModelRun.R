source("Code_with_2015/3a_TransectGLMER_Setup.R") 

library(lme4)
library(sjPlot)


# LUPINE MODEL

# Run binomial model
m_Lupin_Year_2015 <- glmer(cbind(Total_Lupin, Total_Count) ~ 
                        Treatment *
                        Year +
                        #scale(yearly_rain) +
                        (1|Plot), 
                      family = binomial, 
                      data = Lupin_data_2015)

# View model output
summary(m_Lupin_Year_2015)

# Extract model data
predicted_lupine_year_2015 <- Lupin_data_2015 %>%
  mutate(predicted = predict(m_Lupin_Year_2015, type = "response"))



# NATIVITY MODEL

# Run binomial model
m_Nativity_Year_2015 <- glmer(cbind(Total_Native, Total_Count) ~ 
                           Treatment *
                           Year +  
                           #scale(yearly_rain) + 
                           (1|Plot), 
                         family = binomial, 
                         data = Nativity_data_2015)

# View model output
summary(m_Nativity_Year_2015)

# Extract model data
predicted_nativity_year_2015 <- Nativity_data_2015 %>%
  mutate(predicted = predict(m_Nativity_Year_2015, type = "response"))



# INVASIVE MODEL

# Run binomial model
m_Invasive_Year_2015 <- glmer(cbind(Total_Invasive, Total_Count) ~ 
                           Treatment *
                           Year +  
                           #scale(yearly_rain) + 
                           (1|Plot), 
                         family = binomial, 
                         data = Invasive_data_2015)

# View model output
summary(m_Invasive_Year_2015)

# Extract model data
predicted_invasive_year_2015 <- Invasive_data_2015 %>%
  mutate(predicted = predict(m_Invasive_Year_2015, type = "response"))



# SHRUB MODEL

# Run binomial model
m_Shrub_Year_2015 <- glmer(cbind(Total_Shrubs, Total_Count) ~ 
                        Treatment *
                        Year +  
                        #scale(yearly_rain) + 
                        (1|Plot), 
                      family = binomial, 
                      data = Shrubs_data_2015)

# View model output
summary(m_Shrub_Year_2015)

# Extract model data
predicted_shrub_year_2015 <- Shrubs_data_2015 %>%
  mutate(predicted = predict(m_Shrub_Year_2015, type = "response"))

