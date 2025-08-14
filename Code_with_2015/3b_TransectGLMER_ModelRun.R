source("Code_with_2015/3a_TransectGLMER_Setup.R") 

library(lme4)
library(sjPlot)


# LUPINE MODEL BY YEAR

# Run binomial model
m_Lupin_Year_2015 <- glmer(cbind(Total_Lupin, (Total_Count-Total_Lupin)) ~ #corrected 2025-05-28
                        Treatment *
                        Year +
                        #scale(yearly_rain) +
                        (1|Plot), 
                      family = binomial, 
                      data = Lupin_data_2015)


# View model output
sum_lupine <- summary(m_Lupin_Year_2015)


# Extract model data
predicted_lupine_year_2015 <- Lupin_data_2015 %>%
  mutate(predicted = predict(m_Lupin_Year_2015, type = "response"))




# NATIVITY MODEL BY YEAR

# Run binomial model
m_Nativity_Year_2015 <- glmer(cbind(Total_Native, (Total_Count-Total_Native)) ~ #corrected 2025-05-28
                           Treatment *
                           Year +  
                           #scale(yearly_rain) + 
                           (1|Plot), 
                         family = binomial, 
                         data = Nativity_data_2015)

# View model output
sum_native <- summary(m_Nativity_Year_2015)

# Extract model data
predicted_nativity_year_2015 <- Nativity_data_2015 %>%
  mutate(predicted = predict(m_Nativity_Year_2015, type = "response"))


#BB add 2025-05-28 
#test if predict function working correctly for binomial models
library(sjPlot)
plot_model(m_Nativity_Year_2015, terms = c("Year", "Treatment"), type = "pred")
#they look just like CW's predict plots, so no problem there.
#next check the raw data. --> ok
# turns out need to code binomial as success:failure instead of success:total

# check beta regression vs binomial:
library(glmmTMB)
m_Nativity_Year_2015_beta <- glmmTMB((Total_Native/Total_Count) ~ 
                                Treatment *
                                Year +  
                                #scale(yearly_rain) + 
                                (1|Plot), 
                              family = beta_family(link = "logit"), 
                              data = Nativity_data_2015)
summary(m_Nativity_Year_2015_beta)

#coefficients and P values essentially identical for beta and binomial...
#no differences in inference
# p values less strong with beta, but still similar <0.05 for all
# treatment x year tests.





# INVASIVE MODEL BY YEAR

# Run binomial model
m_Invasive_Year_2015 <- glmer(cbind(Total_Invasive, (Total_Count-Total_Invasive)) ~ #corrected 2025-05-28
                           Treatment *
                           Year +  
                           #scale(yearly_rain) + 
                           (1|Plot), 
                         family = binomial, 
                         data = Invasive_data_2015)

# View model output
sum_invasive <- summary(m_Invasive_Year_2015)

# Extract model data
predicted_invasive_year_2015 <- Invasive_data_2015 %>%
  mutate(predicted = predict(m_Invasive_Year_2015, type = "response"))



# SHRUB MODEL BY YEAR

# Run binomial model
m_Shrub_Year_2015 <- glmer(cbind(Total_Shrubs, (Total_Count-Total_Shrubs)) ~ #corrected 2025-05-28
                        Treatment *
                        Year +  
                        #scale(yearly_rain) + 
                        (1|Plot), 
                      family = binomial, 
                      data = Shrubs_data_2015)

# View model output
sum_shrub <- summary(m_Shrub_Year_2015)

# Extract model data
predicted_shrub_year_2015 <- Shrubs_data_2015 %>%
  mutate(predicted = predict(m_Shrub_Year_2015, type = "response"))



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ #



# LUPINE MODEL BY STATUS

# Run binomial model
m_Lupin_Status_2015 <- glmer(cbind(Total_Lupin, Total_Count) ~ 
                             Treatment *
                             Trt_Status +
                             scale(yearly_rain) +
                             (1|Plot), 
                           family = binomial, 
                           data = Lupin_data_2015)

# View model output
summary(m_Lupin_Status_2015)

# Extract model data
predicted_lupine_status_2015 <- Lupin_data_2015 %>%
  mutate(predicted = predict(m_Lupin_Status_2015, type = "response"))




# NATIVITY MODEL BY STATUS

# Run binomial model
m_Nativity_Status_2015 <- glmer(cbind(Total_Native, Total_Count) ~ 
                                Treatment *
                                Trt_Status +  
                                scale(yearly_rain) + 
                                (1|Plot), 
                              family = binomial, 
                              data = Nativity_data_2015)

# View model output
summary(m_Nativity_Status_2015)

# Extract model data
predicted_nativity_status_2015 <- Nativity_data_2015 %>%
  mutate(predicted = predict(m_Nativity_Status_2015, type = "response"))



# INVASIVE MODEL BY STATUS

# Run binomial model
m_Invasive_Status_2015 <- glmer(cbind(Total_Invasive, Total_Count) ~ 
                                Treatment *
                                Trt_Status +  
                                scale(yearly_rain) + 
                                (1|Plot), 
                              family = binomial, 
                              data = Invasive_data_2015)

# View model output
summary(m_Invasive_Status_2015)

# Extract model data
predicted_invasive_status_2015 <- Invasive_data_2015 %>%
  mutate(predicted = predict(m_Invasive_Status_2015, type = "response"))



# SHRUB MODEL BY STATUS

# Run binomial model
m_Shrub_Status_2015 <- glmer(cbind(Total_Shrubs, Total_Count) ~ 
                             Treatment *
                             Trt_Status +  
                             scale(yearly_rain) + 
                             (1|Plot), 
                           family = binomial, 
                           data = Shrubs_data_2015)

# View model output
summary(m_Shrub_Status_2015)

# Extract model data
predicted_shrub_status_2015 <- Shrubs_data_2015 %>%
  mutate(predicted = predict(m_Shrub_Status_2015, type = "response"))


